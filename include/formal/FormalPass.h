#ifndef FORMAL_FORMALPASS_H
#define FORMAL_FORMALPASS_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

#include <string>
#include <vector>

namespace llvm {
class Function;
class Module;
} // namespace llvm

namespace formal {

enum class EntryValidationError {
  None,
  NotFound,
  DeclarationOnly,
  InvalidSignature,
  InvalidLinkage,
  InvalidCallingConvention,
};

constexpr EntryValidationError
validateEntryProperties(bool Exists, bool IsDeclaration, bool ReturnsVoid,
                        unsigned ParameterCount, bool IsVarArg) {
  if (!Exists)
    return EntryValidationError::NotFound;
  if (IsDeclaration)
    return EntryValidationError::DeclarationOnly;
  if (!ReturnsVoid || ParameterCount != 0 || IsVarArg)
    return EntryValidationError::InvalidSignature;
  return EntryValidationError::None;
}

struct EntryValidationResult {
  llvm::Function *Entry = nullptr;
  EntryValidationError Error = EntryValidationError::None;
};

EntryValidationResult validateEntryFunction(llvm::Module &M,
                                            llvm::StringRef EntryName);

enum class PreflightCode {
  Valid,
  MalformedIR,
  MissingTargetTriple,
  MissingDataLayout,
  UnsupportedTarget,
  IncompatibleDataLayout,
  IncompatibleProducer,
  UnversionedProducer,
  HostToolMismatch,
  EntryNotFound,
  EntryDeclarationOnly,
  EntryInvalidSignature,
  EntryInvalidLinkage,
  EntryInvalidCallingConvention,
  DynamicGlobalInitializers,
  ReachableDeclaration,
  ReachableInexactDefinition,
  IndirectCall,
  InlineAssembly,
  AtomicInstruction,
  ScalableVector,
  UnsupportedAddressSpace,
  UnsupportedCallingConvention,
  ExceptionHandling,
  FloatingPoint,
  FixedVector,
  FreezeInstruction,
  UnsupportedValueSemantics,
  UnsupportedOpcode,
  UnsupportedIntrinsic,
  InvalidRegistration,
  ResourceLimit,
};

constexpr PreflightCode
validateTargetProperties(bool HasTriple, bool HasDataLayout, bool IsX86_64Linux,
                         bool IsLittleEndian, unsigned DefaultPointerWidth) {
  if (!HasTriple)
    return PreflightCode::MissingTargetTriple;
  if (!HasDataLayout)
    return PreflightCode::MissingDataLayout;
  if (!IsX86_64Linux)
    return PreflightCode::UnsupportedTarget;
  if (!IsLittleEndian || DefaultPointerWidth != 64)
    return PreflightCode::IncompatibleDataLayout;
  return PreflightCode::Valid;
}

struct PreflightLimits {
  std::size_t MaxFunctions = 100000;
  std::size_t MaxBasicBlocks = 1000000;
  std::size_t MaxInstructions = 10000000;
  std::size_t MaxConstantNodes = 1000000;
  std::size_t MaxConstantBytes = 64U << 20;
  unsigned MaxConstantDepth = 256;
  std::size_t MaxGlobalInitializerBytes = 64U << 20;
  std::size_t MaxTypeWork = 1000000;
  std::size_t MaxMetadataNodes = 1000000;
  std::size_t MaxMetadataBytes = 64U << 20;
  std::size_t MaxProducerEntries = 1024;
  std::size_t MaxProducerBytes = 1U << 20;
  std::size_t MaxModuleTextBytes = 128U << 20;
  unsigned MaxIntegerBitWidth = 1U << 20;
  unsigned MaxAggregateDepth = 256;
  unsigned MaxMetadataDepth = 256;
};

struct HostToolInfo {
  std::string Path;
  std::string SHA256;
  std::uint64_t Size = 0;
  bool MatchesPinned = false;
  std::string MismatchMessage;
};

struct ArtifactRun {
  std::string Id;
  std::string Directory;
};

struct PreflightResult {
  PreflightCode Code = PreflightCode::Valid;
  llvm::Function *Entry = nullptr;
  std::string Message;
  std::string Producer;
  std::vector<std::string> Warnings;

  bool valid() const noexcept { return Code == PreflightCode::Valid; }
  bool incomplete() const noexcept;
};

PreflightResult validateModule(llvm::Module &M, llvm::StringRef EntryName,
                               unsigned ExpectedLLVMVersionMajor,
                               llvm::StringRef ExpectedLLVMVersion,
                               llvm::StringRef ExpectedLLVMRevision,
                               bool AllowUnversionedIR,
                               PreflightLimits Limits = {});
llvm::StringRef preflightCodeName(PreflightCode Code);
llvm::Error validateOutputDirectory(llvm::StringRef OutputDirectory);
llvm::Expected<ArtifactRun> beginArtifactRun(llvm::StringRef OutputDirectory);
llvm::Expected<HostToolInfo>
validateHostTool(llvm::StringRef ExpectedExecutable,
                 llvm::StringRef ExpectedSHA256, std::uint64_t ExpectedSize);
llvm::Expected<std::string> sha256File(llvm::StringRef Path);
llvm::Error writePreflightResult(const ArtifactRun &Run,
                                 const PreflightResult &Result,
                                 const llvm::Module &M,
                                 llvm::StringRef PluginVersion,
                                 const HostToolInfo &HostTool);

} // namespace formal

#endif // FORMAL_FORMALPASS_H
