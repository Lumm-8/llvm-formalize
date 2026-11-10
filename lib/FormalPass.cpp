#include "formal/FormalPass.h"
#include "formal/M5Artifacts.h"
#include "formal/M5Run.h"

#include "llvm/ADT/Twine.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>

using namespace llvm;

namespace {

cl::opt<std::string>
    FormalEntry("formal-entry",
                cl::desc("Top-level function analyzed by FormalPass"),
                cl::value_desc("function"), cl::init("formal_top"));

cl::opt<std::string>
    FormalOutput("formal-output-dir",
                 cl::desc("Validated directory for formal artifacts"),
                 cl::value_desc("directory"), cl::init("formal-results"));

cl::opt<bool> FormalAllowUnversionedIR(
    "formal-allow-unversioned-ir",
    cl::desc(
        "Trust hand-written IR without authenticated Clang producer metadata"),
    cl::init(false));

cl::opt<bool> FormalRunM5(
    "formal-run-m5",
    cl::desc("Execute the M5 registered byte relation after preflight"),
    cl::init(false));

cl::opt<bool> FormalAllocationFailures(
    "formal-allocation-failures",
    cl::desc("Fork null allocation failure paths in M5 execution"),
    cl::init(false));
cl::opt<std::size_t> FormalMaxCallDepth(
    "formal-max-call-depth",
    cl::desc("Maximum dynamic direct-call frame depth"), cl::init(32));
cl::opt<std::uint64_t> FormalLoopBound(
    "formal-loop-bound",
    cl::desc("Default maximum IR backedge traversals per loop activation"),
    cl::init(32));
cl::opt<bool> FormalRequireLocalLoopBound(
    "formal-require-local-loop-bound",
    cl::desc("Require a surviving local bound on every reached natural loop"),
    cl::init(false));
cl::opt<std::string> FormalLoopLimitPolicy(
    "formal-loop-limit-policy",
    cl::desc("Loop bound policy: unknown, error, or assume-exit"),
    cl::init("unknown"));
cl::opt<std::size_t> FormalMaxStates(
    "formal-max-states", cl::desc("Maximum symbolic states"),
    cl::init(10000));
cl::opt<std::uint64_t> FormalMaxTimeMs(
    "formal-max-time-ms", cl::desc("Run-wide wall-time limit in milliseconds (0 disables)"),
    cl::init(0));
cl::opt<std::uint64_t> FormalMaxSolverQueries(
    "formal-max-solver-queries", cl::desc("Maximum solver queries per run"),
    cl::init(UINT64_MAX));

cl::opt<std::size_t>
    FormalMaxFunctions("formal-max-functions",
                       cl::desc("Maximum functions accepted at preflight"),
                       cl::init(100000));
cl::opt<std::size_t>
    FormalMaxBasicBlocks("formal-max-basic-blocks",
                         cl::desc("Maximum basic blocks accepted at preflight"),
                         cl::init(1000000));
cl::opt<std::size_t> FormalMaxInstructions(
    "formal-max-instructions",
    cl::desc("Maximum instructions accepted at preflight"), cl::init(10000000));
cl::opt<std::size_t> FormalMaxConstantNodes(
    "formal-max-constant-nodes",
    cl::desc("Maximum constant traversal work items at preflight"),
    cl::init(1000000));
cl::opt<std::size_t> FormalMaxConstantBytes(
    "formal-max-constant-bytes",
    cl::desc("Maximum constant payload bytes inspected at preflight"),
    cl::init(64U << 20));
cl::opt<unsigned> FormalMaxConstantDepth(
    "formal-max-constant-depth",
    cl::desc("Maximum constant graph nesting accepted at preflight"),
    cl::init(256));
cl::opt<std::size_t> FormalMaxGlobalInitializerBytes(
    "formal-max-global-initializer-bytes",
    cl::desc("Maximum total global initializer allocation bytes"),
    cl::init(64U << 20));
cl::opt<std::size_t> FormalMaxTypeWork(
    "formal-max-type-work",
    cl::desc("Maximum type traversal work items at preflight"),
    cl::init(1000000));
cl::opt<std::size_t> FormalMaxMetadataNodes(
    "formal-max-metadata-nodes",
    cl::desc("Maximum metadata nodes and roots inspected at preflight"),
    cl::init(1000000));
cl::opt<std::size_t> FormalMaxMetadataBytes(
    "formal-max-metadata-bytes",
    cl::desc("Maximum total metadata string bytes at preflight"),
    cl::init(64U << 20));
cl::opt<std::size_t> FormalMaxProducerEntries(
    "formal-max-producer-entries",
    cl::desc("Maximum llvm.ident entries accepted at preflight"),
    cl::init(1024));
cl::opt<std::size_t> FormalMaxProducerBytes(
    "formal-max-producer-bytes",
    cl::desc("Maximum total llvm.ident string bytes at preflight"),
    cl::init(1U << 20));
cl::opt<std::size_t> FormalMaxModuleTextBytes(
    "formal-max-module-text-bytes",
    cl::desc("Maximum canonical textual IR bytes hashed for an artifact"),
    cl::init(128U << 20));
cl::opt<unsigned> FormalMaxIntegerBitWidth(
    "formal-max-integer-bit-width",
    cl::desc("Maximum LLVM integer type width accepted at preflight"),
    cl::init(1U << 20));
cl::opt<unsigned> FormalMaxAggregateDepth(
    "formal-max-aggregate-depth",
    cl::desc("Maximum LLVM aggregate type nesting accepted at preflight"),
    cl::init(256));
cl::opt<unsigned> FormalMaxMetadataDepth(
    "formal-max-metadata-depth",
    cl::desc("Maximum metadata nesting accepted at preflight"), cl::init(256));

class FormalPass : public PassInfoMixin<FormalPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    StringRef OutputDirectory = FormalOutput;
    if (FormalMaxFunctions == 0 || FormalMaxBasicBlocks == 0 ||
        FormalMaxInstructions == 0 || FormalMaxConstantNodes == 0 ||
        FormalMaxConstantBytes == 0 || FormalMaxConstantDepth == 0 ||
        FormalMaxGlobalInitializerBytes == 0 || FormalMaxTypeWork == 0 ||
        FormalMaxMetadataNodes == 0 || FormalMaxMetadataBytes == 0 ||
        FormalMaxProducerEntries == 0 ||
        FormalMaxProducerBytes == 0 || FormalMaxModuleTextBytes == 0 ||
        FormalMaxIntegerBitWidth == 0 || FormalMaxAggregateDepth == 0 ||
        FormalMaxMetadataDepth == 0 || FormalMaxCallDepth == 0 ||
        FormalMaxStates == 0) {
      errs() << "FormalPass: INVALID_CONFIGURATION all resource limits must "
                "be positive\n";
      std::exit(3);
    }
    formal::LoopLimitPolicy LoopPolicy;
    if (FormalLoopLimitPolicy == "unknown")
      LoopPolicy = formal::LoopLimitPolicy::Unknown;
    else if (FormalLoopLimitPolicy == "error")
      LoopPolicy = formal::LoopLimitPolicy::Error;
    else if (FormalLoopLimitPolicy == "assume-exit")
      LoopPolicy = formal::LoopLimitPolicy::AssumeExit;
    else {
      errs() << "FormalPass: INVALID_CONFIGURATION invalid loop limit policy\n";
      std::exit(3);
    }
    if (Error ConfigurationError =
            formal::validateOutputDirectory(OutputDirectory)) {
      errs() << "FormalPass: INVALID_CONFIGURATION "
             << toString(std::move(ConfigurationError)) << '\n';
      std::exit(3);
    }
    Expected<formal::ArtifactRun> Run =
        formal::beginArtifactRun(OutputDirectory);
    if (!Run) {
      errs() << "FormalPass: INTERNAL_ERROR cannot create isolated artifact "
                "run: "
             << toString(Run.takeError()) << '\n';
      std::exit(4);
    }

    Expected<formal::HostToolInfo> HostTool = formal::validateHostTool(
        FORMAL_PINNED_OPT_EXECUTABLE, FORMAL_PINNED_OPT_SHA256,
        FORMAL_PINNED_OPT_SIZE);
    if (!HostTool) {
      errs() << "FormalPass: INTERNAL_ERROR cannot inspect host opt: "
             << toString(HostTool.takeError()) << '\n';
      std::exit(4);
    }

    formal::PreflightResult Validation;
    if (!HostTool->MatchesPinned) {
      Validation.Code = formal::PreflightCode::HostToolMismatch;
      Validation.Message = HostTool->MismatchMessage;
    } else {
      Validation = formal::validateModule(
          M, FormalEntry, LLVM_VERSION_MAJOR, FORMAL_PINNED_LLVM_VERSION,
          FORMAL_PINNED_LLVM_REVISION, FormalAllowUnversionedIR,
          {FormalMaxFunctions, FormalMaxBasicBlocks, FormalMaxInstructions,
           FormalMaxConstantNodes, FormalMaxConstantBytes,
           FormalMaxConstantDepth, FormalMaxGlobalInitializerBytes,
           FormalMaxTypeWork, FormalMaxMetadataNodes, FormalMaxMetadataBytes,
           FormalMaxProducerEntries, FormalMaxProducerBytes,
           FormalMaxModuleTextBytes, FormalMaxIntegerBitWidth,
           FormalMaxAggregateDepth, FormalMaxMetadataDepth});
    }

    if (Error WriteError = formal::writePreflightResult(
            *Run, Validation, M, LLVM_VERSION_STRING, *HostTool)) {
      errs() << "FormalPass: INTERNAL_ERROR cannot write result.json: "
             << toString(std::move(WriteError)) << '\n'
             << "FormalPass: failed artifact run '" << Run->Id << "'\n"
             << "FormalPass: failed artifact directory '" << Run->Directory
             << "'\n";
      std::exit(4);
    }
    errs() << "FormalPass: artifact run '" << Run->Id << "'\n"
           << "FormalPass: artifact directory '" << Run->Directory << "'\n";

    if (Validation.incomplete()) {
      errs() << "FormalPass: UNKNOWN ["
             << formal::preflightCodeName(Validation.Code) << "] "
             << Validation.Message << '\n';
      std::exit(2);
    }

    if (!Validation.valid()) {
      errs() << "FormalPass: INVALID_TASK ["
             << formal::preflightCodeName(Validation.Code) << "] "
             << Validation.Message << '\n';
      std::exit(3);
    }

    if (FormalRunM5) {
      const std::filesystem::path Directory(Run->Directory);
      std::error_code MoveError;
      std::filesystem::rename(Directory / "result.json",
                              Directory / "preflight.json", MoveError);
      if (MoveError) {
        errs() << "FormalPass: INTERNAL_ERROR cannot preserve M0 preflight "
                  "artifact: "
               << MoveError.message() << '\n';
        std::exit(4);
      }
      formal::ExprContext Expressions;
      formal::STPSolverOptions SolverOptions;
      SolverOptions.FailureQueryDirectory = Directory / "queries";
      formal::STPSolver Solver(std::move(SolverOptions));
      formal::ExecutorOptions Options;
      Options.MaxCallDepth = FormalMaxCallDepth;
      Options.DefaultLoopBound = FormalLoopBound;
      Options.RequireLocalLoopBound = FormalRequireLocalLoopBound;
      Options.LoopPolicy = LoopPolicy;
      Options.MaxStates = FormalMaxStates;
      Options.MaxWallTimeMs = FormalMaxTimeMs;
      Options.MaxSolverQueries = FormalMaxSolverQueries;
      Options.ModelAllocationFailure = FormalAllocationFailures;
      formal::M5Analysis Analysis =
          formal::analyzeM5(*Validation.Entry, Expressions, Solver, Options);
      formal::M5ArtifactContext Context;
      Context.Entry = Validation.Entry->getName().str();
      Context.RunId = Run->Id;
      Context.ClangVersion = Validation.Producer;
      Context.SolverVersion = Solver.backendVersion();
      Context.HostTool = HostTool->Path;
      Context.HostToolSHA256 = HostTool->SHA256;
      Context.Options = Options;
      std::string ArtifactError =
          formal::writeM5Artifacts(Directory, M, Analysis, Context);
      if (!ArtifactError.empty()) {
        errs() << "FormalPass: INTERNAL_ERROR " << ArtifactError << '\n';
        std::exit(4);
      }
      errs() << "FormalPass: M5 status " << Analysis.Verdict << '\n'
             << "FormalPass: M5 result '"
             << (Directory / "result.json").string() << "'\n";
      if (Analysis.Verdict == "INTERNAL_ERROR")
        std::exit(4);
      if (Analysis.Verdict == "INVALID_TASK")
        std::exit(3);
      if (Analysis.Verdict == "UNSAFE")
        std::exit(1);
      if (Analysis.Verdict == "UNKNOWN" ||
          Analysis.Verdict == "CONDITIONAL")
        std::exit(2);
      return PreservedAnalyses::all();
    }

    errs() << "FormalPass: preflight validated (opt operation succeeded; no "
              "formal verdict)\n"
           << "FormalPass: LLVM version '" << LLVM_VERSION_STRING << "'\n"
           << "FormalPass: entry function '" << Validation.Entry->getName()
           << "'\n"
           << "FormalPass: output directory '" << OutputDirectory << "'\n"
           << "FormalPass: host opt '" << HostTool->Path << "'\n"
           << "FormalPass: TargetTriple '" << M.getTargetTriple() << "'\n"
           << "FormalPass: DataLayout '"
           << M.getDataLayout().getStringRepresentation() << "'\n";
    for (const std::string &Warning : Validation.Warnings)
      errs() << "FormalPass: warning: " << Warning << '\n';

    return PreservedAnalyses::all();
  }
};

PassPluginLibraryInfo getFormalPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "FormalPass", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name != "formal-exec")
                    return false;
                  MPM.addPass(FormalPass());
                  return true;
                });
          }};
}

} // namespace

extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo() {
  return getFormalPassPluginInfo();
}
