#ifndef FORMAL_M5_ARTIFACTS_H
#define FORMAL_M5_ARTIFACTS_H

#include "formal/M5Run.h"

#include <cstdint>
#include <filesystem>
#include <string>

namespace llvm {
class Module;
}

namespace formal {

struct M5ArtifactContext {
  std::string Entry;
  std::string RunId;
  std::string ClangVersion;
  std::string SolverVersion;
  std::string HostTool;
  std::string HostToolSHA256;
  std::uint64_t TimeoutMs = 30000;
  ExecutorOptions Options;
};

// Returns an error message on failure. result.json is published last and
// removed before writing any artifact, so a failed run cannot look successful.
std::string writeM5Artifacts(const std::filesystem::path &Directory,
                             const llvm::Module &Module,
                             const M5Analysis &Analysis,
                             const M5ArtifactContext &Context);

} // namespace formal

#endif // FORMAL_M5_ARTIFACTS_H
