#include "formal/Expr.h"

#include "llvm/ADT/SmallString.h"

#include <algorithm>
#include <atomic>
#include <limits>
#include <stdexcept>
#include <unordered_map>
#include <utility>

namespace formal {
namespace {

std::atomic<std::uint64_t> NextOwner{1};

std::string apIntKey(const llvm::APInt &Value) {
  llvm::SmallString<64> Buffer;
  Value.toString(Buffer, 16, false);
  return std::to_string(Value.getBitWidth()) + ":" + Buffer.str().str();
}

void validateName(const std::string &Name) {
  if (Name.empty())
    throw std::invalid_argument("symbol name must not be empty");
  if (Name.size() > 1024)
    throw std::length_error("symbol name exceeds the 1024-byte limit");
  for (unsigned char Character : Name)
    if (Character < 0x21 || Character > 0x7e || Character == '|' ||
        Character == '\\')
      throw std::invalid_argument(
          "symbol names must use printable ASCII excluding '|' and '\\'");
}

bool isZero(const ExprRef &Expression) {
  return Expression->isConstant() && Expression->constantValue().isZero();
}

bool isOne(const ExprRef &Expression) {
  return Expression->isConstant() && Expression->constantValue().isOne();
}

bool isAllOnes(const ExprRef &Expression) {
  return Expression->isConstant() && Expression->constantValue().isAllOnes();
}

struct NodeKey {
  ExprKind Kind;
  SortKind Sort;
  unsigned Width;
  std::string Value;
  std::string Name;
  unsigned Offset;
  std::vector<const Expr *> Operands;
  const Array *Root = nullptr;
  const UpdateNode *Updates = nullptr;

  bool operator==(const NodeKey &Other) const {
    return Kind == Other.Kind && Sort == Other.Sort && Width == Other.Width &&
           Value == Other.Value && Name == Other.Name &&
           Offset == Other.Offset && Operands == Other.Operands &&
           Root == Other.Root && Updates == Other.Updates;
  }
};

void hashCombine(std::size_t &Seed, std::size_t Value) {
  Seed ^= Value + 0x9e3779b97f4a7c15ULL + (Seed << 6) + (Seed >> 2);
}

struct NodeKeyHash {
  std::size_t operator()(const NodeKey &Key) const {
    std::size_t Result = static_cast<std::size_t>(Key.Kind);
    hashCombine(Result, static_cast<std::size_t>(Key.Sort));
    hashCombine(Result, Key.Width);
    hashCombine(Result, std::hash<std::string>{}(Key.Value));
    hashCombine(Result, std::hash<std::string>{}(Key.Name));
    hashCombine(Result, Key.Offset);
    hashCombine(Result, std::hash<const Array *>{}(Key.Root));
    hashCombine(Result, std::hash<const UpdateNode *>{}(Key.Updates));
    for (const Expr *Operand : Key.Operands)
      hashCombine(Result, std::hash<const Expr *>{}(Operand));
    return Result;
  }
};

struct UpdateKey {
  const Expr *Index;
  const Expr *Value;
  const UpdateNode *Next;

  bool operator==(const UpdateKey &Other) const {
    return Index == Other.Index && Value == Other.Value && Next == Other.Next;
  }
};

struct UpdateKeyHash {
  std::size_t operator()(const UpdateKey &Key) const {
    std::size_t Result = std::hash<const Expr *>{}(Key.Index);
    hashCombine(Result, std::hash<const Expr *>{}(Key.Value));
    hashCombine(Result, std::hash<const UpdateNode *>{}(Key.Next));
    return Result;
  }
};

} // namespace

Sort::Sort(SortKind Kind, unsigned Width) : Kind(Kind), Width(Width) {
  if (Width == 0)
    throw std::invalid_argument("sort width must be positive");
}

unsigned Sort::bitWidth() const {
  if (!isBitVector())
    throw std::logic_error("sort is not a bitvector");
  return Width;
}

unsigned Sort::indexWidth() const {
  if (!isArray())
    throw std::logic_error("sort is not an array");
  return Width;
}

unsigned Sort::rangeWidth() const {
  if (!isArray())
    throw std::logic_error("sort is not an array");
  return 8;
}

UpdateList::UpdateList(ArrayRef Root, UpdateNodeRef Head)
    : Root(std::move(Root)), Head(std::move(Head)) {}

unsigned UpdateList::size() const noexcept { return Head ? Head->size() : 0; }

Array::Array(std::uint64_t Owner, std::string Name, Sort ArraySort)
    : Owner(Owner), Name(std::move(Name)), ArraySort(ArraySort) {}

UpdateNode::UpdateNode(ExprRef Index, ExprRef Value, UpdateNodeRef Next)
    : Index(std::move(Index)), Value(std::move(Value)), Next(std::move(Next)),
      Size(this->Next ? this->Next->size() + 1 : 1) {}

Expr::Expr(std::uint64_t Owner, ExprKind Kind, Sort ExprSort, llvm::APInt Value,
           std::string Name, unsigned Offset, unsigned Depth,
           std::vector<ExprRef> Operands,
           std::shared_ptr<const UpdateList> Updates)
    : Owner(Owner), Kind(Kind), ExprSort(ExprSort), Value(std::move(Value)),
      Name(std::move(Name)), Offset(Offset), Depth(Depth),
      Operands(std::move(Operands)), Updates(std::move(Updates)) {}

unsigned Expr::width() const {
  if (!ExprSort.isBitVector())
    throw std::logic_error("expression is not a bitvector");
  return ExprSort.bitWidth();
}

const llvm::APInt &Expr::constantValue() const {
  if (!isConstant())
    throw std::logic_error("expression is not a constant");
  return Value;
}

const std::string &Expr::symbolName() const {
  if (Kind != ExprKind::Symbol)
    throw std::logic_error("expression is not a symbol");
  return Name;
}

const ExprRef &Expr::operand(std::size_t Index) const {
  return Operands.at(Index);
}

const UpdateList &Expr::updates() const {
  if (Kind != ExprKind::Read || !Updates)
    throw std::logic_error("expression is not an array read");
  return *Updates;
}

struct ExprContext::Impl {
  explicit Impl(ExprContextOptions Options)
      : Owner(NextOwner.fetch_add(1)), Options(Options) {
    if (Options.MaxBitWidth == 0 || Options.MaxDepth == 0 ||
        Options.MaxNodes == 0 || Options.MaxUpdates == 0 ||
        Options.MaxArrays == 0)
      throw std::invalid_argument("ExprContext limits must be positive");
  }

  std::uint64_t Owner;
  ExprContextOptions Options;
  std::unordered_map<NodeKey, ExprRef, NodeKeyHash> Nodes;
  std::unordered_map<std::string, ArrayRef> Arrays;
  std::unordered_map<std::string, Sort> Names;
  std::unordered_map<UpdateKey, UpdateNodeRef, UpdateKeyHash> UpdateNodes;
};

ExprContext::ExprContext(ExprContextOptions Options)
    : Implementation(std::make_unique<Impl>(Options)) {}
ExprContext::~ExprContext() = default;

void ExprContext::requireOwned(const ExprRef &Expression) const {
  if (!Expression)
    throw std::invalid_argument("expression must not be null");
  if (Expression->Owner != Implementation->Owner)
    throw std::invalid_argument("expression belongs to another ExprContext");
}

void ExprContext::requireOwned(const ArrayRef &ArrayValue) const {
  if (!ArrayValue)
    throw std::invalid_argument("array must not be null");
  if (ArrayValue->Owner != Implementation->Owner)
    throw std::invalid_argument("array belongs to another ExprContext");
}

ExprRef ExprContext::intern(ExprKind Kind, Sort ExprSort, llvm::APInt Value,
                            std::string Name, unsigned Offset,
                            std::vector<ExprRef> Operands,
                            std::shared_ptr<const UpdateList> UpdatesValue) {
  std::vector<const Expr *> OperandKeys;
  OperandKeys.reserve(Operands.size());
  unsigned Depth = 1;
  for (const ExprRef &Operand : Operands) {
    requireOwned(Operand);
    OperandKeys.push_back(Operand.get());
    Depth = std::max(Depth, Operand->depth() + 1);
  }
  if (UpdatesValue) {
    requireOwned(UpdatesValue->root());
    for (UpdateNodeRef Node = UpdatesValue->head(); Node; Node = Node->next()) {
      Depth = std::max(Depth, Node->index()->depth() + 1);
      Depth = std::max(Depth, Node->value()->depth() + 1);
    }
  }
  if (Depth > Implementation->Options.MaxDepth)
    throw std::length_error("expression depth limit exceeded");

  unsigned SortWidth = ExprSort.isArray()       ? ExprSort.indexWidth()
                       : ExprSort.isBitVector() ? ExprSort.bitWidth()
                                                : 1;
  NodeKey Key{Kind,
              ExprSort.kind(),
              SortWidth,
              apIntKey(Value),
              Name,
              Offset,
              std::move(OperandKeys),
              UpdatesValue ? UpdatesValue->root().get() : nullptr,
              UpdatesValue ? UpdatesValue->head().get() : nullptr};
  auto Existing = Implementation->Nodes.find(Key);
  if (Existing != Implementation->Nodes.end())
    return Existing->second;
  if (Implementation->Nodes.size() >= Implementation->Options.MaxNodes)
    throw std::length_error("expression node limit exceeded");

  ExprRef Result(new Expr(Implementation->Owner, Kind, ExprSort,
                          std::move(Value), std::move(Name), Offset, Depth,
                          std::move(Operands), std::move(UpdatesValue)));
  Implementation->Nodes.emplace(std::move(Key), Result);
  return Result;
}

ExprRef ExprContext::constant(const llvm::APInt &Value) {
  if (Value.getBitWidth() > Implementation->Options.MaxBitWidth)
    throw std::length_error("bitvector width limit exceeded");
  return intern(ExprKind::Constant, Sort::bitVector(Value.getBitWidth()), Value,
                {}, 0, {});
}

ExprRef ExprContext::constant(unsigned Width, std::uint64_t Value) {
  if (Width == 0)
    throw std::invalid_argument("bitvector width must be non-zero");
  if (Width > Implementation->Options.MaxBitWidth)
    throw std::length_error("bitvector width limit exceeded");
  return constant(llvm::APInt(Width, Value));
}

ExprRef ExprContext::signedConstant(unsigned Width, std::int64_t Value) {
  if (Width == 0)
    throw std::invalid_argument("bitvector width must be non-zero");
  if (Width > Implementation->Options.MaxBitWidth)
    throw std::length_error("bitvector width limit exceeded");
  return constant(llvm::APInt(Width, static_cast<std::uint64_t>(Value), true));
}

ExprRef ExprContext::boolean(bool Value) {
  return intern(ExprKind::Constant, Sort::boolean(), llvm::APInt(1, Value), {},
                0, {});
}

ExprRef ExprContext::symbol(unsigned Width, std::string Name) {
  if (Width > Implementation->Options.MaxBitWidth)
    throw std::length_error("bitvector width limit exceeded");
  validateName(Name);
  Sort SymbolSort = Sort::bitVector(Width);
  auto [Position, Inserted] = Implementation->Names.emplace(Name, SymbolSort);
  if (!Inserted && Position->second != SymbolSort)
    throw std::invalid_argument(
        "symbol name is already used with another sort");
  return intern(ExprKind::Symbol, SymbolSort, llvm::APInt(1, 0),
                std::move(Name), 0, {});
}

ExprRef ExprContext::booleanSymbol(std::string Name) {
  validateName(Name);
  Sort SymbolSort = Sort::boolean();
  auto [Position, Inserted] = Implementation->Names.emplace(Name, SymbolSort);
  if (!Inserted && Position->second != SymbolSort)
    throw std::invalid_argument(
        "symbol name is already used with another sort");
  return intern(ExprKind::Symbol, SymbolSort, llvm::APInt(1, 0),
                std::move(Name), 0, {});
}

ArrayRef ExprContext::array(unsigned IndexWidth, std::string Name) {
  if (IndexWidth > Implementation->Options.MaxBitWidth)
    throw std::length_error("array index width limit exceeded");
  validateName(Name);
  Sort ArraySort = Sort::byteArray(IndexWidth);
  auto NamePosition = Implementation->Names.find(Name);
  if (NamePosition != Implementation->Names.end() &&
      NamePosition->second != ArraySort)
    throw std::invalid_argument(
        "symbol name is already used with another sort");
  auto Existing = Implementation->Arrays.find(Name);
  if (Existing != Implementation->Arrays.end())
    return Existing->second;
  if (Implementation->Arrays.size() >= Implementation->Options.MaxArrays)
    throw std::length_error("array count limit exceeded");
  if (NamePosition == Implementation->Names.end())
    Implementation->Names.emplace(Name, ArraySort);
  ArrayRef Result(
      new Array(Implementation->Owner, Name, Sort::byteArray(IndexWidth)));
  Implementation->Arrays.emplace(std::move(Name), Result);
  return Result;
}

UpdateList ExprContext::updates(ArrayRef Root) {
  requireOwned(Root);
  return UpdateList(std::move(Root), nullptr);
}

UpdateList ExprContext::write(const UpdateList &UpdatesValue, ExprRef Index,
                              ExprRef Value) {
  requireOwned(UpdatesValue.root());
  requireOwned(Index);
  requireOwned(Value);
  if (Index->sort() !=
      Sort::bitVector(UpdatesValue.root()->sort().indexWidth()))
    throw std::invalid_argument("array write index has the wrong sort");
  if (Value->sort() != Sort::bitVector(8))
    throw std::invalid_argument("array write value must be an 8-bit bitvector");

  UpdateNodeRef Next = UpdatesValue.head();
  if (Next && Next->index() == Index)
    Next = Next->next();
  UpdateKey Key{Index.get(), Value.get(), Next.get()};
  auto Existing = Implementation->UpdateNodes.find(Key);
  UpdateNodeRef Head;
  if (Existing != Implementation->UpdateNodes.end()) {
    Head = Existing->second;
  } else {
    if (Implementation->UpdateNodes.size() >=
        Implementation->Options.MaxUpdates)
      throw std::length_error("array update limit exceeded");
    Head = UpdateNodeRef(
        new UpdateNode(std::move(Index), std::move(Value), std::move(Next)));
    Implementation->UpdateNodes.emplace(Key, Head);
  }
  return UpdateList(UpdatesValue.root(), std::move(Head));
}

ExprRef ExprContext::read(const UpdateList &UpdatesValue, ExprRef Index) {
  requireOwned(UpdatesValue.root());
  requireOwned(Index);
  if (Index->sort() !=
      Sort::bitVector(UpdatesValue.root()->sort().indexWidth()))
    throw std::invalid_argument("array read index has the wrong sort");

  UpdateNodeRef Head = UpdatesValue.head();
  while (Head) {
    if (Head->index() == Index)
      return Head->value();
    if (!(Head->index()->isConstant() && Index->isConstant() &&
          Head->index()->constantValue() != Index->constantValue()))
      break;
    Head = Head->next();
  }
  auto EffectiveUpdates = std::shared_ptr<const UpdateList>(
      new UpdateList(UpdatesValue.root(), Head));
  return intern(ExprKind::Read, Sort::bitVector(8), llvm::APInt(1, 0), {}, 0,
                {std::move(Index)}, std::move(EffectiveUpdates));
}

ExprRef ExprContext::binary(ExprKind Kind, ExprRef Left, ExprRef Right) {
  requireOwned(Left);
  requireOwned(Right);
  if (!Left->sort().isBitVector() || Left->sort() != Right->sort())
    throw std::invalid_argument(
        "bitvector operands must have identical bitvector sorts");
  unsigned Width = Left->width();

  if (Left->isConstant() && Right->isConstant()) {
    const llvm::APInt &L = Left->constantValue();
    const llvm::APInt &R = Right->constantValue();
    llvm::APInt Result(Width, 0);
    bool Fold = true;
    switch (Kind) {
    case ExprKind::Add:
      Result = L + R;
      break;
    case ExprKind::Sub:
      Result = L - R;
      break;
    case ExprKind::Mul:
      Result = L * R;
      break;
    case ExprKind::UDiv:
      if (R.isZero())
        Fold = false;
      else
        Result = L.udiv(R);
      break;
    case ExprKind::SDiv:
      if (R.isZero() || (L.isMinSignedValue() && R.isAllOnes()))
        Fold = false;
      else
        Result = L.sdiv(R);
      break;
    case ExprKind::URem:
      if (R.isZero())
        Fold = false;
      else
        Result = L.urem(R);
      break;
    case ExprKind::SRem:
      if (R.isZero())
        Fold = false;
      else
        Result = L.srem(R);
      break;
    case ExprKind::And:
      Result = L & R;
      break;
    case ExprKind::Or:
      Result = L | R;
      break;
    case ExprKind::Xor:
      Result = L ^ R;
      break;
    case ExprKind::Shl: {
      std::uint64_t Amount = R.getLimitedValue(Width);
      Result = Amount >= Width ? llvm::APInt(Width, 0) : L.shl(Amount);
      break;
    }
    case ExprKind::LShr: {
      std::uint64_t Amount = R.getLimitedValue(Width);
      Result = Amount >= Width ? llvm::APInt(Width, 0) : L.lshr(Amount);
      break;
    }
    case ExprKind::AShr: {
      std::uint64_t Amount = R.getLimitedValue(Width);
      Result = Amount >= Width
                   ? (L.isNegative() ? llvm::APInt::getAllOnes(Width)
                                     : llvm::APInt(Width, 0))
                   : L.ashr(Amount);
      break;
    }
    default:
      Fold = false;
      break;
    }
    if (Fold)
      return constant(Result);
  }

  switch (Kind) {
  case ExprKind::Add:
  case ExprKind::Or:
  case ExprKind::Xor:
    if (isZero(Left))
      return Right;
    if (isZero(Right))
      return Left;
    break;
  case ExprKind::Sub:
    if (isZero(Right))
      return Left;
    if (Left == Right)
      return constant(Width, 0);
    break;
  case ExprKind::Mul:
    if (isZero(Left) || isZero(Right))
      return constant(Width, 0);
    if (isOne(Left))
      return Right;
    if (isOne(Right))
      return Left;
    break;
  case ExprKind::And:
    if (isZero(Left) || isZero(Right))
      return constant(Width, 0);
    if (isAllOnes(Left))
      return Right;
    if (isAllOnes(Right))
      return Left;
    if (Left == Right)
      return Left;
    break;
  default:
    break;
  }
  if (Kind == ExprKind::Or && Left == Right)
    return Left;
  if (Kind == ExprKind::Xor && Left == Right)
    return constant(Width, 0);
  Sort ResultSort = Left->sort();
  return intern(Kind, ResultSort, llvm::APInt(1, 0), {}, 0,
                {std::move(Left), std::move(Right)});
}

#define FORMAL_BINARY_METHOD(Name, Kind)                                       \
  ExprRef ExprContext::Name(ExprRef Left, ExprRef Right) {                     \
    return binary(ExprKind::Kind, std::move(Left), std::move(Right));          \
  }
FORMAL_BINARY_METHOD(add, Add)
FORMAL_BINARY_METHOD(sub, Sub)
FORMAL_BINARY_METHOD(mul, Mul)
FORMAL_BINARY_METHOD(udiv, UDiv)
FORMAL_BINARY_METHOD(sdiv, SDiv)
FORMAL_BINARY_METHOD(urem, URem)
FORMAL_BINARY_METHOD(srem, SRem)
FORMAL_BINARY_METHOD(bitAnd, And)
FORMAL_BINARY_METHOD(bitOr, Or)
FORMAL_BINARY_METHOD(bitXor, Xor)
FORMAL_BINARY_METHOD(shl, Shl)
FORMAL_BINARY_METHOD(lshr, LShr)
FORMAL_BINARY_METHOD(ashr, AShr)
#undef FORMAL_BINARY_METHOD

ExprRef ExprContext::compare(ExprKind Kind, ExprRef Left, ExprRef Right) {
  requireOwned(Left);
  requireOwned(Right);
  if (!Left->sort().isBitVector() || Left->sort() != Right->sort())
    throw std::invalid_argument(
        "comparison operands must have identical bitvector sorts");
  if (Left == Right) {
    bool Result =
        Kind == ExprKind::Eq || Kind == ExprKind::Ule || Kind == ExprKind::Sle;
    return boolean(Result);
  }
  if (Left->isConstant() && Right->isConstant()) {
    const llvm::APInt &L = Left->constantValue();
    const llvm::APInt &R = Right->constantValue();
    bool Result = false;
    switch (Kind) {
    case ExprKind::Eq:
      Result = L == R;
      break;
    case ExprKind::Ult:
      Result = L.ult(R);
      break;
    case ExprKind::Ule:
      Result = L.ule(R);
      break;
    case ExprKind::Slt:
      Result = L.slt(R);
      break;
    case ExprKind::Sle:
      Result = L.sle(R);
      break;
    default:
      throw std::invalid_argument("invalid comparison kind");
    }
    return boolean(Result);
  }
  return intern(Kind, Sort::boolean(), llvm::APInt(1, 0), {}, 0,
                {std::move(Left), std::move(Right)});
}

#define FORMAL_COMPARE_METHOD(Name, Kind)                                      \
  ExprRef ExprContext::Name(ExprRef Left, ExprRef Right) {                     \
    return compare(ExprKind::Kind, std::move(Left), std::move(Right));         \
  }
FORMAL_COMPARE_METHOD(eq, Eq)
FORMAL_COMPARE_METHOD(ult, Ult)
FORMAL_COMPARE_METHOD(ule, Ule)
FORMAL_COMPARE_METHOD(slt, Slt)
FORMAL_COMPARE_METHOD(sle, Sle)
#undef FORMAL_COMPARE_METHOD

ExprRef ExprContext::logicalNot(ExprRef Value) {
  requireOwned(Value);
  if (!Value->sort().isBool())
    throw std::invalid_argument("logical Not requires a Bool operand");
  if (Value->isConstant())
    return boolean(Value->constantValue().isZero());
  if (Value->kind() == ExprKind::Not)
    return Value->operand(0);
  return intern(ExprKind::Not, Sort::boolean(), llvm::APInt(1, 0), {}, 0,
                {std::move(Value)});
}

ExprRef ExprContext::toBool(ExprRef Value) {
  requireOwned(Value);
  if (Value->sort().isBool())
    return Value;
  if (Value->sort() == Sort::bitVector(1))
    return eq(std::move(Value), constant(1, 1));
  throw std::invalid_argument(
      "only Bool or one-bit bitvector converts to Bool");
}

ExprRef ExprContext::select(ExprRef Condition, ExprRef TrueValue,
                            ExprRef FalseValue) {
  requireOwned(Condition);
  requireOwned(TrueValue);
  requireOwned(FalseValue);
  if (!Condition->sort().isBool())
    throw std::invalid_argument("Select condition must have Bool sort");
  if (TrueValue->sort() != FalseValue->sort() || TrueValue->sort().isArray())
    throw std::invalid_argument("Select arms must have identical scalar sorts");
  if (TrueValue == FalseValue)
    return TrueValue;
  if (Condition->isConstant())
    return Condition->constantValue().isZero() ? FalseValue : TrueValue;
  Sort ResultSort = TrueValue->sort();
  return intern(
      ExprKind::Select, ResultSort, llvm::APInt(1, 0), {}, 0,
      {std::move(Condition), std::move(TrueValue), std::move(FalseValue)});
}

ExprRef ExprContext::concat(ExprRef High, ExprRef Low) {
  requireOwned(High);
  requireOwned(Low);
  if (!High->sort().isBitVector() || !Low->sort().isBitVector())
    throw std::invalid_argument("Concat operands must be bitvectors");
  if (High->width() > std::numeric_limits<unsigned>::max() - Low->width())
    throw std::length_error("Concat width overflows unsigned");
  unsigned Width = High->width() + Low->width();
  if (Width > Implementation->Options.MaxBitWidth)
    throw std::length_error("Concat exceeds bitvector width limit");
  if (High->isConstant() && Low->isConstant()) {
    llvm::APInt Result = High->constantValue().zext(Width).shl(Low->width());
    Result |= Low->constantValue().zext(Width);
    return constant(Result);
  }
  return intern(ExprKind::Concat, Sort::bitVector(Width), llvm::APInt(1, 0), {},
                0, {std::move(High), std::move(Low)});
}

ExprRef ExprContext::extract(ExprRef Value, unsigned Offset, unsigned Width) {
  requireOwned(Value);
  if (!Value->sort().isBitVector())
    throw std::invalid_argument("Extract operand must be a bitvector");
  if (Width == 0 || Offset > Value->width() || Width > Value->width() - Offset)
    throw std::invalid_argument("Extract range is outside its operand");
  if (Offset == 0 && Width == Value->width())
    return Value;
  if (Value->isConstant())
    return constant(Value->constantValue().lshr(Offset).trunc(Width));
  return intern(ExprKind::Extract, Sort::bitVector(Width), llvm::APInt(1, 0),
                {}, Offset, {std::move(Value)});
}

ExprRef ExprContext::zext(ExprRef Value, unsigned Width) {
  requireOwned(Value);
  if (!Value->sort().isBitVector() || Width < Value->width())
    throw std::invalid_argument("ZExt requires a bitvector and cannot narrow");
  if (Width > Implementation->Options.MaxBitWidth)
    throw std::length_error("ZExt exceeds bitvector width limit");
  if (Width == Value->width())
    return Value;
  if (Value->isConstant())
    return constant(Value->constantValue().zext(Width));
  return intern(ExprKind::ZExt, Sort::bitVector(Width), llvm::APInt(1, 0), {},
                0, {std::move(Value)});
}

ExprRef ExprContext::sext(ExprRef Value, unsigned Width) {
  requireOwned(Value);
  if (!Value->sort().isBitVector() || Width < Value->width())
    throw std::invalid_argument("SExt requires a bitvector and cannot narrow");
  if (Width > Implementation->Options.MaxBitWidth)
    throw std::length_error("SExt exceeds bitvector width limit");
  if (Width == Value->width())
    return Value;
  if (Value->isConstant())
    return constant(Value->constantValue().sext(Width));
  return intern(ExprKind::SExt, Sort::bitVector(Width), llvm::APInt(1, 0), {},
                0, {std::move(Value)});
}

ExprRef ExprContext::trunc(ExprRef Value, unsigned Width) {
  requireOwned(Value);
  if (!Value->sort().isBitVector() || Width == 0 || Width > Value->width())
    throw std::invalid_argument(
        "Trunc requires a positive no-larger bitvector width");
  if (Width == Value->width())
    return Value;
  if (Value->isConstant())
    return constant(Value->constantValue().trunc(Width));
  return intern(ExprKind::Trunc, Sort::bitVector(Width), llvm::APInt(1, 0), {},
                0, {std::move(Value)});
}

std::size_t ExprContext::nodeCount() const noexcept {
  return Implementation->Nodes.size();
}

std::size_t ExprContext::updateCount() const noexcept {
  return Implementation->UpdateNodes.size();
}

std::size_t ExprContext::arrayCount() const noexcept {
  return Implementation->Arrays.size();
}

} // namespace formal
