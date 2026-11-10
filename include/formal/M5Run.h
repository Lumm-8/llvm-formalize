#ifndef FORMAL_M5_RUN_H
#define FORMAL_M5_RUN_H

#include "formal/Executor.h"
#include "formal/Relation.h"

#include <optional>
#include <string>

namespace formal {

struct M5Analysis {
  ExecutionResult Execution;
  RelationArtifact Relation;
  std::optional<std::string> CounterexampleJSON;
  std::string Verdict;
  std::string Reason;
};

// The pass and formal-check use this single verdict and relation construction
// path. The caller owns Expressions for the lifetime of the returned paths.
M5Analysis analyzeM5(llvm::Function &Entry, ExprContext &Expressions,
                     Solver &SolverBackend, ExecutorOptions Options);

} // namespace formal

#endif // FORMAL_M5_RUN_H
