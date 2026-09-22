#include "formal/FormalPass.h"

#include "llvm/ADT/Twine.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>
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
        FormalMaxMetadataDepth == 0) {
      errs() << "FormalPass: INVALID_CONFIGURATION all resource limits must "
                "be positive\n";
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
