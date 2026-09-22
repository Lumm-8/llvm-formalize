#include "formal/FormalPass.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SHA256.h"
#include "llvm/Support/raw_ostream.h"

namespace {

class SHA256Stream final : public llvm::raw_ostream {
public:
  SHA256Stream() { SetUnbuffered(); }

  std::string digest() {
    flush();
    return llvm::toHex(Hash.final(), true);
  }

private:
  void write_impl(const char *Pointer, std::size_t Size) override {
    Hash.update(llvm::StringRef(Pointer, Size));
    Position += Size;
  }

  std::uint64_t current_pos() const override { return Position; }

  llvm::SHA256 Hash;
  std::uint64_t Position = 0;
};

std::string hashModule(const llvm::Module &M) {
  SHA256Stream Stream;
  M.print(Stream, nullptr);
  return Stream.digest();
}

} // namespace

llvm::Error formal::validateOutputDirectory(llvm::StringRef OutputDirectory) {
  if (OutputDirectory.empty())
    return llvm::createStringError(std::errc::invalid_argument,
                                   "formal output directory is empty");
  for (auto Component = llvm::sys::path::begin(OutputDirectory),
            End = llvm::sys::path::end(OutputDirectory);
       Component != End; ++Component)
    if (*Component == "..")
      return llvm::createStringError(
          std::errc::invalid_argument,
          "formal output directory must not contain '..' components");

  if (llvm::sys::fs::exists(OutputDirectory) &&
      !llvm::sys::fs::is_directory(OutputDirectory))
    return llvm::createStringError(std::errc::not_a_directory,
                                   "formal output path is not a directory");
  return llvm::Error::success();
}

llvm::Expected<formal::ArtifactRun>
formal::beginArtifactRun(llvm::StringRef OutputDirectory) {
  if (llvm::Error Error = validateOutputDirectory(OutputDirectory))
    return Error;
  if (std::error_code EC = llvm::sys::fs::create_directories(OutputDirectory))
    return llvm::errorCodeToError(EC);

  llvm::SmallString<256> RunsDirectory(OutputDirectory);
  llvm::sys::path::append(RunsDirectory, "runs");
  if (std::error_code EC = llvm::sys::fs::create_directories(RunsDirectory))
    return llvm::errorCodeToError(EC);

  llvm::SmallString<256> Prefix(RunsDirectory);
  llvm::sys::path::append(Prefix, "run");
  llvm::SmallString<256> RunDirectory;
  if (std::error_code EC =
          llvm::sys::fs::createUniqueDirectory(Prefix, RunDirectory))
    return llvm::errorCodeToError(EC);
  return ArtifactRun{llvm::sys::path::filename(RunDirectory).str(),
                     RunDirectory.str().str()};
}

llvm::Error formal::writePreflightResult(const ArtifactRun &Run,
                                         const PreflightResult &Result,
                                         const llvm::Module &M,
                                         llvm::StringRef PluginVersion,
                                         const HostToolInfo &HostTool) {
  llvm::SmallString<256> FinalPath(Run.Directory);
  llvm::sys::path::append(FinalPath, "result.json");

  llvm::json::Array Warnings;
  for (const std::string &Warning : Result.Warnings)
    Warnings.push_back(Warning);

  llvm::json::Value ModuleDigest = nullptr;
  llvm::StringRef ModuleDigestStatus = "omitted-before-resource-validation";
  if (Result.Code != PreflightCode::MalformedIR &&
      Result.Code != PreflightCode::ResourceLimit &&
      Result.Code != PreflightCode::HostToolMismatch) {
    ModuleDigest = hashModule(M);
    ModuleDigestStatus = "complete-canonical-ir-sha256";
  }

  llvm::json::Object Root{
      {"schema_version", 1},
      {"run_id", Run.Id},
      {"phase", "M0_PREFLIGHT"},
      {"status", Result.valid()        ? "VALIDATED"
                 : Result.incomplete() ? "UNKNOWN"
                                       : "INVALID_TASK"},
      {"reason", preflightCodeName(Result.Code)},
      {"message", Result.Message},
      {"entry", Result.Entry ? Result.Entry->getName() : ""},
      {"llvm_version", PluginVersion},
      {"pinned_llvm_version", FORMAL_PINNED_LLVM_VERSION},
      {"pinned_llvm_revision", FORMAL_PINNED_LLVM_REVISION},
      {"actual_llvm_revision", FORMAL_ACTUAL_LLVM_REVISION},
      {"opt_llvm_version", HostTool.MatchesPinned
                               ? llvm::json::Value(PluginVersion)
                               : llvm::json::Value(nullptr)},
      {"host_opt_path", HostTool.Path},
      {"host_opt_sha256", HostTool.SHA256},
      {"host_opt_sha256_basis", "runtime-sha256-of-running-executable-image"},
      {"host_opt_size", HostTool.Size},
      {"host_opt_identity",
       HostTool.MatchesPinned ? "exact-pinned-executable" : "mismatch"},
      {"host_opt_llvm_version", HostTool.MatchesPinned
                                    ? llvm::json::Value(PluginVersion)
                                    : llvm::json::Value(nullptr)},
      {"host_opt_llvm_revision",
       HostTool.MatchesPinned ? llvm::json::Value(FORMAL_ACTUAL_LLVM_REVISION)
                              : llvm::json::Value(nullptr)},
      {"pinned_opt_path", FORMAL_PINNED_OPT_EXECUTABLE},
      {"pinned_opt_sha256", FORMAL_PINNED_OPT_SHA256},
      {"pinned_opt_size", FORMAL_PINNED_OPT_SIZE},
      {"input_module_sha256", std::move(ModuleDigest)},
      {"input_module_digest_status", ModuleDigestStatus},
      {"clang_version", Result.Producer},
      {"producer", Result.Producer},
      {"target_triple", M.getTargetTriple()},
      {"data_layout", M.getDataLayoutStr()},
      {"warnings", std::move(Warnings)},
      {"verdict", Result.incomplete() ? llvm::json::Value("UNKNOWN")
                                      : llvm::json::Value(nullptr)},
  };

  llvm::SmallString<256> TemporaryModel(Run.Directory);
  llvm::sys::path::append(TemporaryModel, ".result-%%%%%%.json.tmp");
  llvm::SmallString<256> TemporaryPath;
  int FileDescriptor = -1;
  if (std::error_code EC = llvm::sys::fs::createUniqueFile(
          TemporaryModel, FileDescriptor, TemporaryPath))
    return llvm::errorCodeToError(EC);

  {
    llvm::raw_fd_ostream Output(FileDescriptor, true);
    Output << llvm::formatv("{0:2}", llvm::json::Value(std::move(Root)))
           << '\n';
    Output.flush();
    if (Output.has_error()) {
      std::error_code EC = Output.error();
      llvm::sys::fs::remove(TemporaryPath);
      return llvm::errorCodeToError(EC);
    }
  }

  if (std::error_code EC = llvm::sys::fs::rename(TemporaryPath, FinalPath)) {
    llvm::sys::fs::remove(TemporaryPath);
    return llvm::errorCodeToError(EC);
  }
  return llvm::Error::success();
}
