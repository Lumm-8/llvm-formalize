#include "formal/Constraints.h"

#include <stdexcept>
#include <utility>

namespace formal {

void ConstraintManager::add(ExprRef Constraint) {
  if (!Constraint || !Constraint->sort().isBool())
    throw std::invalid_argument("path constraint must have Bool sort");
  Constraints.push_back(std::move(Constraint));
}

} // namespace formal
