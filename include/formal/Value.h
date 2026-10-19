#ifndef FORMAL_VALUE_H
#define FORMAL_VALUE_H

#include "formal/Expr.h"
#include "formal/Memory.h"

#include <stdexcept>
#include <utility>
#include <variant>

namespace formal {

class SymbolicValue final {
public:
  explicit SymbolicValue(ExprRef BitVector) : Data(std::move(BitVector)) {
    const ExprRef &Value = std::get<ExprRef>(Data);
    if (!Value || !Value->sort().isBitVector())
      throw std::invalid_argument(
          "SymbolicValue requires a bitvector expression");
  }
  explicit SymbolicValue(PointerValue Pointer) : Data(std::move(Pointer)) {
    if (!std::get<PointerValue>(Data).Width ||
        std::get<PointerValue>(Data).Candidates.empty())
      throw std::invalid_argument("SymbolicValue requires pointer candidates");
  }

  bool isPointer() const noexcept {
    return std::holds_alternative<PointerValue>(Data);
  }
  const ExprRef &bitVector() const { return std::get<ExprRef>(Data); }
  const PointerValue &pointer() const { return std::get<PointerValue>(Data); }
  unsigned width() const {
    return isPointer() ? pointer().Width : bitVector()->width();
  }

private:
  std::variant<ExprRef, PointerValue> Data;
};

} // namespace formal

#endif // FORMAL_VALUE_H
