#ifndef FORMAL_VALUE_H
#define FORMAL_VALUE_H

#include "formal/Expr.h"
#include "formal/Memory.h"

#include <stdexcept>
#include <utility>
#include <variant>
#include <vector>

namespace llvm { class Type; }

namespace formal {

struct AggregateValue final {
  const llvm::Type *Type = nullptr;
  std::vector<ExprRef> Bytes;
  std::vector<ExprRef> KnownMasks;
  std::vector<ExprRef> AddressSensitiveBytes;
  std::map<std::uint64_t, PointerShadowEntry> Shadows;
  bool operator==(const AggregateValue &) const = default;
};

class SymbolicValue final {
public:
  bool operator==(const SymbolicValue &) const = default;
  explicit SymbolicValue(ExprRef BitVector, ExprRef Poison = nullptr)
      : Data(std::move(BitVector)), PoisonCondition(std::move(Poison)) {
    const ExprRef &Value = std::get<ExprRef>(Data);
    if (!Value || !Value->sort().isBitVector())
      throw std::invalid_argument(
          "SymbolicValue requires a bitvector expression");
    if (PoisonCondition && !PoisonCondition->sort().isBool())
      throw std::invalid_argument("poison condition must be Boolean");
    if (PoisonCondition && PoisonCondition->isConstant() &&
        PoisonCondition->constantValue().isZero())
      PoisonCondition.reset();
  }
  explicit SymbolicValue(PointerValue Pointer) : Data(std::move(Pointer)) {
    if (!std::get<PointerValue>(Data).Width ||
        std::get<PointerValue>(Data).Candidates.empty())
      throw std::invalid_argument("SymbolicValue requires pointer candidates");
  }
  explicit SymbolicValue(AggregateValue Aggregate)
      : Data(std::move(Aggregate)) {
    const auto &Value = std::get<AggregateValue>(Data);
    if (!Value.Type || Value.Bytes.size() != Value.KnownMasks.size() ||
        Value.Bytes.size() != Value.AddressSensitiveBytes.size())
      throw std::invalid_argument("invalid aggregate value");
  }

  bool isPointer() const noexcept {
    return std::holds_alternative<PointerValue>(Data);
  }
  bool isAggregate() const noexcept {
    return std::holds_alternative<AggregateValue>(Data);
  }
  const ExprRef &bitVector() const { return std::get<ExprRef>(Data); }
  const ExprRef &poisonCondition() const noexcept { return PoisonCondition; }
  const PointerValue &pointer() const { return std::get<PointerValue>(Data); }
  const AggregateValue &aggregate() const {
    return std::get<AggregateValue>(Data);
  }
  unsigned width() const {
    return isAggregate() ? 0 : isPointer() ? pointer().Width : bitVector()->width();
  }

private:
  std::variant<ExprRef, PointerValue, AggregateValue> Data;
  ExprRef PoisonCondition;
};

} // namespace formal

#endif // FORMAL_VALUE_H
