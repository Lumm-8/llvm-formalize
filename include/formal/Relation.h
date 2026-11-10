#ifndef FORMAL_RELATION_H
#define FORMAL_RELATION_H

#include "formal/Result.h"

#include <optional>
#include <string>

namespace formal {

struct RelationArtifact {
  bool Complete = false;
  bool UnderApproximation = false;
  std::string Reason;
  std::string SMTLib;
  std::string JSON;
};

// Exports only an execution whose every feasible path completed and whose
// registration descriptors agree. A failed export never carries a relation.
RelationArtifact exportRelation(const ExecutionResult &Result,
                                ExprContext &Expressions);
bool hasClosedRegisteredErrorPath(const PathResult &Path);
std::optional<std::string>
exportCounterexampleJSON(const ExecutionResult &Result);

} // namespace formal

#endif // FORMAL_RELATION_H
