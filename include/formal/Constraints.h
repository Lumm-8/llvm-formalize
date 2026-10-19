#ifndef FORMAL_CONSTRAINTS_H
#define FORMAL_CONSTRAINTS_H

#include "formal/Solver.h"

#include <cstddef>

namespace formal {

class ConstraintManager final {
public:
  void add(ExprRef Constraint);

  const ConstraintSet &constraints() const noexcept { return Constraints; }
  std::size_t size() const noexcept { return Constraints.size(); }
  bool empty() const noexcept { return Constraints.empty(); }

private:
  ConstraintSet Constraints;
};

} // namespace formal

#endif // FORMAL_CONSTRAINTS_H
