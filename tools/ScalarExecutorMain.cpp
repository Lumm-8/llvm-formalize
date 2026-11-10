#include "formal/Executor.h"
#include "formal/FormalPass.h"
#include "formal/M5Artifacts.h"
#include "formal/M5Run.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace {

void usage(const char *Program) {
  llvm::errs() << "usage: " << Program
               << " <module.ll> --entry <function> [--timeout-ms N] "
                  "[--max-instructions N] [--max-call-depth N] "
                  "[--loop-bound N] [--require-local-loop-bound] "
                  "[--max-states N] [--max-time-ms N] "
                  "[--max-solver-queries N] "
                  "[--loop-limit-policy unknown|error|assume-exit] "
                  "[--allocation-failures] "
                  "[--m4 | --m5 --output-dir DIR]\n";
}

bool parseUnsigned(const std::string &Text, std::uint64_t &Value) {
  if (Text.empty())
    return false;
  for (char Digit : Text)
    if (Digit < '0' || Digit > '9')
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
#ifdef FORMAL_CHECK_DEFAULT_M5
  if (argc == 2 && std::string(argv[1]) == "--version") {
    llvm::outs() << "formal-check LLVM " << LLVM_VERSION_STRING << " revision "
                 << FORMAL_PINNED_LLVM_REVISION << '\n';
    return 0;
  }
#endif
  if (argc < 4) {
    usage(argv[0]);
    return 3;
  }
  std::string Input = argv[1];
  std::string Entry;
  std::uint64_t TimeoutMs = 30000;
  std::uint64_t MaxInstructions = 100000;
  std::uint64_t MaxCallDepth = 32;
  std::uint64_t LoopBound = 32;
  bool RequireLocalLoopBound = false;
  std::uint64_t MaxStates = 10000;
  std::uint64_t MaxTimeMs = 0;
  std::uint64_t MaxSolverQueries = UINT64_MAX;
  formal::LoopLimitPolicy LoopPolicy = formal::LoopLimitPolicy::Unknown;
  bool ModelAllocationFailure = false;
  bool EnableMemory =
#ifdef FORMAL_CHECK_DEFAULT_M5
      true;
#else
      false;
#endif
  bool EnableRegistration =
#ifdef FORMAL_CHECK_DEFAULT_M5
      true;
#else
      false;
#endif
  std::string OutputDir;
  bool AllowUnversionedIR = false;
  for (int Index = 2; Index < argc; ++Index) {
    std::string Option = argv[Index];
    if ((Option == "--entry" || Option == "--timeout-ms" ||
         Option == "--max-instructions" || Option == "--max-call-depth" ||
         Option == "--loop-bound" || Option == "--max-states" ||
         Option == "--max-time-ms" ||
         Option == "--max-solver-queries" ||
         Option == "--loop-limit-policy" ||
         Option == "--output-dir") &&
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
    } else if (Option == "--max-call-depth") {
      if (!parseUnsigned(argv[++Index], MaxCallDepth) || MaxCallDepth == 0 ||
          MaxCallDepth > 100000) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--loop-bound") {
      if (!parseUnsigned(argv[++Index], LoopBound)) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--require-local-loop-bound") {
      RequireLocalLoopBound = true;
    } else if (Option == "--loop-limit-policy") {
      std::string Value = argv[++Index];
      if (Value == "unknown")
        LoopPolicy = formal::LoopLimitPolicy::Unknown;
      else if (Value == "error")
        LoopPolicy = formal::LoopLimitPolicy::Error;
      else if (Value == "assume-exit")
        LoopPolicy = formal::LoopLimitPolicy::AssumeExit;
      else {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--max-states") {
      if (!parseUnsigned(argv[++Index], MaxStates) || MaxStates == 0 ||
          MaxStates > std::numeric_limits<std::size_t>::max()) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--max-time-ms") {
      if (!parseUnsigned(argv[++Index], MaxTimeMs)) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--max-solver-queries") {
      if (!parseUnsigned(argv[++Index], MaxSolverQueries)) {
        usage(argv[0]);
        return 3;
      }
    } else if (Option == "--allocation-failures") {
      ModelAllocationFailure = true;
    } else if (Option == "--m4") {
      EnableMemory = true;
    } else if (Option == "--m5") {
      EnableMemory = true;
      EnableRegistration = true;
    } else if (Option == "--output-dir") {
      OutputDir = argv[++Index];
    } else if (Option == "--allow-unversioned-ir") {
      AllowUnversionedIR = true;
    } else {
      usage(argv[0]);
      return 3;
    }
  }
  if (Entry.empty() || (EnableRegistration && OutputDir.empty()) ||
      TimeoutMs > static_cast<std::uint64_t>(
                      std::chrono::milliseconds::max().count())) {
    usage(argv[0]);
    return 3;
  }
  if (EnableRegistration) {
    std::error_code Error;
    std::filesystem::create_directories(OutputDir, Error);
    if (!Error)
      for (const char *Name :
           {"result.json", "relation.smt2", "counterexample.json"}) {
        std::filesystem::remove(std::filesystem::path(OutputDir) / Name, Error);
        if (Error)
          break;
      }
    if (Error) {
      llvm::errs() << "M5 cannot invalidate stale result: " << Error.message()
                   << '\n';
      return 4;
    }
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
#ifdef FORMAL_CHECK_DEFAULT_M5
  formal::PreflightResult Validation = formal::validateModule(
      *Module, Entry, LLVM_VERSION_MAJOR, FORMAL_PINNED_LLVM_VERSION,
      FORMAL_PINNED_LLVM_REVISION, AllowUnversionedIR);
  if (!Validation.valid()) {
    llvm::errs() << "formal-check preflight "
                 << formal::preflightCodeName(Validation.Code) << ": "
                 << Validation.Message << '\n';
    return Validation.incomplete() ? 2 : 3;
  }
#else
  (void)AllowUnversionedIR;
#endif
  llvm::Function *Function = Module->getFunction(Entry);
  if (!Function || Function->isDeclaration()) {
    llvm::errs() << "M3 driver: missing defined entry '" << Entry << "'\n";
    return 3;
  }

  formal::ExprContext Expressions;
  formal::STPSolverOptions SolverOptions;
  SolverOptions.Timeout = std::chrono::milliseconds(TimeoutMs);
  SolverOptions.FailureQueryDirectory =
      EnableRegistration ? std::filesystem::path(OutputDir) / "queries"
                         : std::filesystem::path("m3-solver-queries");
  formal::STPSolver Solver(std::move(SolverOptions));
  formal::ExecutorOptions ExecutorOptions;
  ExecutorOptions.MaxInstructionsPerState = MaxInstructions;
  ExecutorOptions.MaxCallDepth = static_cast<std::size_t>(MaxCallDepth);
  ExecutorOptions.DefaultLoopBound = LoopBound;
  ExecutorOptions.RequireLocalLoopBound = RequireLocalLoopBound;
  ExecutorOptions.LoopPolicy = LoopPolicy;
  ExecutorOptions.MaxStates = static_cast<std::size_t>(MaxStates);
  ExecutorOptions.MaxWallTimeMs = MaxTimeMs;
  ExecutorOptions.MaxSolverQueries = MaxSolverQueries;
  ExecutorOptions.SolverTimeoutMs = TimeoutMs;
  ExecutorOptions.EnableMemory = EnableMemory;
  ExecutorOptions.EnableRegistration = EnableRegistration;
  ExecutorOptions.ModelAllocationFailure = ModelAllocationFailure;
  if (EnableRegistration &&
      (!Function->getReturnType()->isVoidTy() || Function->arg_size() != 0)) {
    llvm::errs() << "M5 entry must have signature void ()\n";
    return 3;
  }
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

  formal::M5Analysis Analysis;
  formal::ExecutionResult OrdinaryResult;
  if (EnableRegistration)
    Analysis =
        formal::analyzeM5(*Function, Expressions, Solver, ExecutorOptions);
  else {
    formal::ScalarExecutor Executor(Expressions, Solver, ExecutorOptions);
    OrdinaryResult = Executor.execute(*Function, Bindings);
  }
  const formal::ExecutionResult &Result =
      EnableRegistration ? Analysis.Execution : OrdinaryResult;
  if (EnableRegistration) {
    formal::M5ArtifactContext Context;
    Context.Entry = Entry;
#ifdef FORMAL_CHECK_DEFAULT_M5
    Context.ClangVersion = Validation.Producer;
#else
    Context.ClangVersion = "unknown (preflight disabled)";
#endif
    Context.SolverVersion = Solver.backendVersion();
    Context.TimeoutMs = TimeoutMs;
    Context.Options = ExecutorOptions;
    std::string Failure =
        formal::writeM5Artifacts(OutputDir, *Module, Analysis, Context);
    if (!Failure.empty()) {
      llvm::errs() << "M5 artifact write failed: " << Failure << '\n';
      return 4;
    }
  }
  const char *Prefix = EnableRegistration ? "M5" : EnableMemory ? "M4" : "M3";
  const bool ConditionalUnwinding =
      Result.Status == formal::ExecutionStatus::Unknown &&
      Result.HasUnwindingAssumption &&
      std::all_of(Result.Paths.begin(), Result.Paths.end(),
                  [](const formal::PathResult &Path) {
                    return Path.Status != formal::PathStatus::Incomplete ||
                           Path.IncompleteReason ==
                               formal::IncompletenessReason::LoopBound;
                  });
  const char *Status =
      EnableRegistration ? Analysis.Verdict.c_str()
      : Result.Status != formal::ExecutionStatus::InternalError &&
                Result.HasLoopPolicyError
          ? "INVALID_TASK"
      : ConditionalUnwinding
          ? "CONDITIONAL"
      : EnableMemory && Result.Status == formal::ExecutionStatus::Complete
          ? "M4_EXECUTOR_COMPLETE"
          : formal::toString(Result.Status);
  llvm::outs() << Prefix << " status " << Status << '\n';
  if (EnableRegistration && !Analysis.Relation.Complete)
    llvm::outs() << "M5 relation reason=" << Analysis.Reason << '\n';
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
  if (EnableRegistration) {
    if (Analysis.Verdict == "INTERNAL_ERROR")
      return 4;
    if (Analysis.Verdict == "UNSAFE")
      return 1;
    if (Analysis.Verdict == "INVALID_TASK")
      return 3;
    if (Analysis.Verdict == "UNKNOWN" ||
        Analysis.Verdict == "CONDITIONAL")
      return 2;
  }
  if (Result.Status != formal::ExecutionStatus::InternalError &&
      Result.HasLoopPolicyError)
    return 3;
  if (ConditionalUnwinding)
    return 2;
  return 0;
}
