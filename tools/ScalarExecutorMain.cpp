#include "formal/Executor.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <memory>
#include <string>
#include <vector>

namespace {

void usage(const char *Program) {
  llvm::errs() << "usage: " << Program
               << " <module.ll> --entry <function> [--timeout-ms N] "
                  "[--max-instructions N] [--m4]\n";
}

bool parseUnsigned(const std::string &Text, std::uint64_t &Value) {
  if (Text.empty())
    return false;
  char *End = nullptr;
  errno = 0;
  unsigned long long Parsed = std::strtoull(Text.c_str(), &End, 10);
  if (errno != 0 || End != Text.c_str() + Text.size())
    return false;
  Value = Parsed;
  return true;
}

std::string valueText(const formal::SymbolicValue &Value) {
  if (!Value.bitVector()->isConstant())
    return "bv" + std::to_string(Value.width()) + ":symbolic";
  llvm::SmallString<64> Text;
  Value.bitVector()->constantValue().toString(Text, 10, false);
  return "bv" + std::to_string(Value.width()) + ":" + Text.str().str();
}

const char *pathReason(const formal::PathResult &Path) {
  switch (Path.Status) {
  case formal::PathStatus::Completed:
    return "NORMAL-RETURN";
  case formal::PathStatus::Incomplete:
    return formal::toReasonCode(Path.IncompleteReason);
  case formal::PathStatus::Error:
    return formal::toReasonCode(Path.ErrorReason);
  case formal::PathStatus::InternalError:
    return formal::toReasonCode(Path.InternalReason);
  }
  return "INTERNAL-UNKNOWN-REASON";
}

} // namespace

int main(int argc, char **argv) {
  if (argc < 4) {
    usage(argv[0]);
    return 3;
  }
  std::string Input = argv[1];
  std::string Entry;
  std::uint64_t TimeoutMs = 30000;
  std::uint64_t MaxInstructions = 100000;
  bool EnableMemory = false;
  for (int Index = 2; Index < argc; ++Index) {
    std::string Option = argv[Index];
    if ((Option == "--entry" || Option == "--timeout-ms" ||
         Option == "--max-instructions") &&
        Index + 1 >= argc) {
      usage(argv[0]);
      return 3;
    }
    if (Option == "--entry") {
      Entry = argv[++Index];
    } else if (Option == "--timeout-ms") {
      if (!parseUnsigned(argv[++Index], TimeoutMs)) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--max-instructions") {
      if (!parseUnsigned(argv[++Index], MaxInstructions) ||
          MaxInstructions == 0) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--m4") {
      EnableMemory = true;
    } else {
      usage(argv[0]);
      return 3;
    }
  }
  if (Entry.empty() ||
      TimeoutMs > static_cast<std::uint64_t>(
                      std::chrono::milliseconds::max().count())) {
    usage(argv[0]);
    return 3;
  }

  llvm::LLVMContext LLVMContext;
  llvm::SMDiagnostic Diagnostic;
  std::unique_ptr<llvm::Module> Module =
      llvm::parseIRFile(Input, Diagnostic, LLVMContext);
  if (!Module) {
    Diagnostic.print(argv[0], llvm::errs());
    return 3;
  }
  if (llvm::verifyModule(*Module, &llvm::errs()))
    return 3;
  llvm::Function *Function = Module->getFunction(Entry);
  if (!Function || Function->isDeclaration()) {
    llvm::errs() << "M3 driver: missing defined entry '" << Entry << "'\n";
    return 3;
  }

  formal::ExprContext Expressions;
  formal::STPSolverOptions SolverOptions;
  SolverOptions.Timeout = std::chrono::milliseconds(TimeoutMs);
  SolverOptions.FailureQueryDirectory =
      std::filesystem::path("m3-solver-queries");
  formal::STPSolver Solver(std::move(SolverOptions));
  formal::ExecutorOptions ExecutorOptions;
  ExecutorOptions.MaxInstructionsPerState = MaxInstructions;
  ExecutorOptions.EnableMemory = EnableMemory;
  formal::ScalarExecutor Executor(Expressions, Solver, ExecutorOptions);
  std::vector<formal::ArgumentBinding> Bindings;
  unsigned ArgumentIndex = 0;
  for (llvm::Argument &Argument : Function->args()) {
    if (!Argument.getType()->isIntegerTy()) {
      llvm::errs() << "M3 driver: argument " << ArgumentIndex
                   << " is not an integer scalar\n";
      return 3;
    }
    unsigned Width =
        llvm::cast<llvm::IntegerType>(Argument.getType())->getBitWidth();
    Bindings.push_back(
        {&Argument, formal::SymbolicValue(Expressions.symbol(
                        Width, "arg" + std::to_string(ArgumentIndex)))});
    ++ArgumentIndex;
  }

  formal::ExecutionResult Result = Executor.execute(*Function, Bindings);
  const char *Prefix = EnableMemory ? "M4" : "M3";
  const char *Status =
      EnableMemory && Result.Status == formal::ExecutionStatus::Complete
          ? "M4_EXECUTOR_COMPLETE"
          : formal::toString(Result.Status);
  llvm::outs() << Prefix << " status " << Status << '\n';
  llvm::outs() << Prefix << " paths " << Result.Paths.size() << '\n';
  for (const formal::PathResult &Path : Result.Paths) {
    llvm::outs() << Prefix << " path state=" << Path.StateId
                 << " status=" << formal::toString(Path.Status)
                 << " reason=" << pathReason(Path)
                 << " constraints=" << Path.Constraints.size()
                 << " location=" << Path.Location.Function << ':'
                 << Path.Location.BasicBlock << ':' << Path.Location.Opcode
                 << '#' << Path.Location.InstructionOrdinal;
    if (Path.ReturnValue)
      llvm::outs() << " return=" << valueText(*Path.ReturnValue);
    else if (Path.Status == formal::PathStatus::Completed)
      llvm::outs() << " return=void";
    llvm::outs() << '\n';
  }
  const formal::ExecutionStatistics &Stats = Result.Statistics;
  llvm::outs() << Prefix << " stats states=" << Stats.StatesCreated
               << " forks=" << Stats.Forks
               << " pruned=" << Stats.PrunedPaths
               << " instructions=" << Stats.ExecutedInstructions
               << " queries=" << Stats.SolverQueries << '\n';
  if (EnableMemory)
    llvm::outs() << "M4 memory objects=" << Stats.MemoryObjects
                 << " object_states=" << Stats.ObjectStates
                 << " cow_copies=" << Stats.COWCopies
                 << " byte_reads=" << Stats.ByteReadExprs
                 << " byte_writes=" << Stats.ByteWriteExprs
                 << " pointer_resolutions=" << Stats.PointerResolutions
                 << " pointer_candidates=" << Stats.PointerCandidates
                 << " pointer_forks=" << Stats.PointerForks
                 << " cap_hits=" << Stats.PointerCapHits
                 << " resolution_queries=" << Stats.ResolutionSolverQueries
                 << " resolution_us=" << Stats.ResolutionSolverMicroseconds
                 << " ast_nodes=" << Stats.ASTNodes
                 << " max_updates=" << Stats.MaxUpdateListLength
                 << " peak_bytes=" << Stats.PeakResidentBytes << '\n';
  return 0;
}
