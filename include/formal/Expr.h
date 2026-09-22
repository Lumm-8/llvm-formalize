#ifndef FORMAL_EXPR_H
#define FORMAL_EXPR_H

#include "llvm/ADT/APInt.h"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace formal {

enum class SortKind { Bool, BitVector, Array };

class Sort final {
public:
  static Sort boolean() { return Sort(SortKind::Bool, 1); }
  static Sort bitVector(unsigned Width) {
    return Sort(SortKind::BitVector, Width);
  }
  static Sort byteArray(unsigned IndexWidth) {
    return Sort(SortKind::Array, IndexWidth);
  }

  SortKind kind() const noexcept { return Kind; }
  bool isBool() const noexcept { return Kind == SortKind::Bool; }
  bool isBitVector() const noexcept { return Kind == SortKind::BitVector; }
  bool isArray() const noexcept { return Kind == SortKind::Array; }
  unsigned bitWidth() const;
  unsigned indexWidth() const;
  unsigned rangeWidth() const;

  friend bool operator==(Sort Left, Sort Right) {
    return Left.Kind == Right.Kind && Left.Width == Right.Width;
  }
  friend bool operator!=(Sort Left, Sort Right) { return !(Left == Right); }

private:
  Sort(SortKind Kind, unsigned Width);
  SortKind Kind;
  unsigned Width;
};

enum class ExprKind {
  Constant,
  Symbol,
  Read,
  Add,
  Sub,
  Mul,
  UDiv,
  SDiv,
  URem,
  SRem,
  And,
  Or,
  Xor,
  Shl,
  LShr,
  AShr,
  Eq,
  Ult,
  Ule,
  Slt,
  Sle,
  Not,
  Select,
  Concat,
  Extract,
  ZExt,
  SExt,
  Trunc,
};

class Expr;
class Array;
class UpdateNode;
using ExprRef = std::shared_ptr<const Expr>;
using ArrayRef = std::shared_ptr<const Array>;
using UpdateNodeRef = std::shared_ptr<const UpdateNode>;

class UpdateList final {
public:
  const ArrayRef &root() const noexcept { return Root; }
  const UpdateNodeRef &head() const noexcept { return Head; }
  unsigned size() const noexcept;

private:
  friend class ExprContext;
  UpdateList(ArrayRef Root, UpdateNodeRef Head);
  ArrayRef Root;
  UpdateNodeRef Head;
};

class Array final {
public:
  const std::string &name() const noexcept { return Name; }
  Sort sort() const noexcept { return ArraySort; }

private:
  friend class ExprContext;
  Array(std::uint64_t Owner, std::string Name, Sort ArraySort);
  std::uint64_t Owner;
  const std::string Name;
  const Sort ArraySort;
};

class UpdateNode final {
public:
  const ExprRef &index() const noexcept { return Index; }
  const ExprRef &value() const noexcept { return Value; }
  const UpdateNodeRef &next() const noexcept { return Next; }
  unsigned size() const noexcept { return Size; }

private:
  friend class ExprContext;
  UpdateNode(ExprRef Index, ExprRef Value, UpdateNodeRef Next);
  const ExprRef Index;
  const ExprRef Value;
  const UpdateNodeRef Next;
  const unsigned Size;
};

class Expr final {
public:
  ExprKind kind() const noexcept { return Kind; }
  Sort sort() const noexcept { return ExprSort; }
  unsigned width() const;
  const llvm::APInt &constantValue() const;
  const std::string &symbolName() const;
  unsigned offset() const noexcept { return Offset; }
  unsigned depth() const noexcept { return Depth; }
  const std::vector<ExprRef> &operands() const noexcept { return Operands; }
  const ExprRef &operand(std::size_t Index) const;
  const UpdateList &updates() const;
  bool isConstant() const noexcept { return Kind == ExprKind::Constant; }

private:
  friend class ExprContext;
  Expr(std::uint64_t Owner, ExprKind Kind, Sort ExprSort, llvm::APInt Value,
       std::string Name, unsigned Offset, unsigned Depth,
       std::vector<ExprRef> Operands,
       std::shared_ptr<const UpdateList> Updates);

  const std::uint64_t Owner;
  const ExprKind Kind;
  const Sort ExprSort;
  const llvm::APInt Value;
  const std::string Name;
  const unsigned Offset;
  const unsigned Depth;
  const std::vector<ExprRef> Operands;
  const std::shared_ptr<const UpdateList> Updates;
};

struct ExprContextOptions {
  unsigned MaxBitWidth = 1U << 20;
  unsigned MaxDepth = 1024;
  std::size_t MaxNodes = 1U << 20;
  std::size_t MaxUpdates = 1U << 20;
  std::size_t MaxArrays = 1U << 20;
};

class ExprContext final {
public:
  explicit ExprContext(ExprContextOptions Options = {});
  ~ExprContext();
  ExprContext(const ExprContext &) = delete;
  ExprContext &operator=(const ExprContext &) = delete;

  ExprRef constant(const llvm::APInt &Value);
  ExprRef constant(unsigned Width, std::uint64_t Value);
  ExprRef signedConstant(unsigned Width, std::int64_t Value);
  ExprRef boolean(bool Value);
  ExprRef symbol(unsigned Width, std::string Name);
  ExprRef booleanSymbol(std::string Name);

  ArrayRef array(unsigned IndexWidth, std::string Name);
  UpdateList updates(ArrayRef Root);
  UpdateList write(const UpdateList &Updates, ExprRef Index, ExprRef Value);
  ExprRef read(const UpdateList &Updates, ExprRef Index);

  ExprRef add(ExprRef Left, ExprRef Right);
  ExprRef sub(ExprRef Left, ExprRef Right);
  ExprRef mul(ExprRef Left, ExprRef Right);
  ExprRef udiv(ExprRef Left, ExprRef Right);
  ExprRef sdiv(ExprRef Left, ExprRef Right);
  ExprRef urem(ExprRef Left, ExprRef Right);
  ExprRef srem(ExprRef Left, ExprRef Right);
  ExprRef bitAnd(ExprRef Left, ExprRef Right);
  ExprRef bitOr(ExprRef Left, ExprRef Right);
  ExprRef bitXor(ExprRef Left, ExprRef Right);
  ExprRef shl(ExprRef Left, ExprRef Right);
  ExprRef lshr(ExprRef Left, ExprRef Right);
  ExprRef ashr(ExprRef Left, ExprRef Right);

  ExprRef eq(ExprRef Left, ExprRef Right);
  ExprRef ult(ExprRef Left, ExprRef Right);
  ExprRef ule(ExprRef Left, ExprRef Right);
  ExprRef slt(ExprRef Left, ExprRef Right);
  ExprRef sle(ExprRef Left, ExprRef Right);
  ExprRef logicalNot(ExprRef Value);
  ExprRef toBool(ExprRef Value);
  ExprRef select(ExprRef Condition, ExprRef TrueValue, ExprRef FalseValue);

  ExprRef concat(ExprRef High, ExprRef Low);
  ExprRef extract(ExprRef Value, unsigned Offset, unsigned Width);
  ExprRef zext(ExprRef Value, unsigned Width);
  ExprRef sext(ExprRef Value, unsigned Width);
  ExprRef trunc(ExprRef Value, unsigned Width);

  std::size_t nodeCount() const noexcept;
  std::size_t updateCount() const noexcept;
  std::size_t arrayCount() const noexcept;

private:
  struct Impl;
  std::unique_ptr<Impl> Implementation;

  ExprRef binary(ExprKind Kind, ExprRef Left, ExprRef Right);
  ExprRef compare(ExprKind Kind, ExprRef Left, ExprRef Right);
  ExprRef intern(ExprKind Kind, Sort ExprSort, llvm::APInt Value,
                 std::string Name, unsigned Offset,
                 std::vector<ExprRef> Operands,
                 std::shared_ptr<const UpdateList> Updates = nullptr);
  void requireOwned(const ExprRef &Expression) const;
  void requireOwned(const ArrayRef &Array) const;
};

} // namespace formal

#endif // FORMAL_EXPR_H
