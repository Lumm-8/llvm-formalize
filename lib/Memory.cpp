#include "formal/Memory.h"

#include <limits>
#include <iterator>
#include <stdexcept>
#include <utility>

namespace formal {

PointerValue PointerValue::null(unsigned Width, ExprContext &Expressions,
                                unsigned AddressSpace) {
  if (!Width)
    throw MemoryModelException("zero-width pointer");
  ExprRef Zero = Expressions.constant(Width, 0);
  return {AddressSpace, Width, {{nullptr, Zero, nullptr}}, Zero};
}

PointerValue PointerValue::object(std::shared_ptr<const MemoryObject> Object,
                                  ExprRef Offset, ExprContext &Expressions,
                                  unsigned AddressSpace) {
  if (!Object || !Offset || !Offset->sort().isBitVector())
    throw MemoryModelException("invalid object pointer");
  const unsigned Width = Offset->width();
  ExprRef Numeric = Expressions.add(
      Expressions.constant(Width, Object->LogicalBase), Offset);
  return {AddressSpace, Width,
          {{std::move(Object), std::move(Offset), nullptr}}, Numeric};
}

ObjectState::ObjectState(std::shared_ptr<const MemoryObject> Object,
                         ExprContext &Expressions, unsigned IndexWidth)
    : Object(std::move(Object)),
      Bytes(Expressions.updates(Expressions.array(
          IndexWidth, "memory_object_" +
                          std::to_string(this->Object ? this->Object->Id : 0)))),
      Initialized(static_cast<std::size_t>(this->Object ? this->Object->Size : 0),
                  Expressions.boolean(false)),
      KnownMasks(static_cast<std::size_t>(this->Object ? this->Object->Size : 0),
                 Expressions.constant(8, 0)),
      AddressSensitiveBytes(
          static_cast<std::size_t>(this->Object ? this->Object->Size : 0),
          Expressions.boolean(false)),
      IndexWidth(IndexWidth) {
  if (!this->Object || !IndexWidth)
    throw MemoryModelException("invalid memory object state");
}

ExprRef ObjectState::initializedAt(ExprRef Offset,
                                    ExprContext &Expressions,
                                    std::uint8_t RequiredMask) const {
  if (!Offset || Offset->width() != IndexWidth)
    throw MemoryModelException("initialization index width mismatch");
  if (Offset->isConstant()) {
    const llvm::APInt &Value = Offset->constantValue();
    if (Value.ugt(std::numeric_limits<std::uint64_t>::max()))
      return Expressions.boolean(false);
    std::uint64_t Index = Value.getZExtValue();
    if (Index >= Initialized.size())
      return Expressions.boolean(false);
    if (RequiredMask == 0xff)
      return Initialized[static_cast<std::size_t>(Index)];
    ExprRef Mask = Expressions.constant(8, RequiredMask);
    return Expressions.eq(
        Expressions.bitAnd(KnownMasks[static_cast<std::size_t>(Index)], Mask),
        Mask);
  }
  ExprRef Result = Expressions.boolean(false);
  for (std::size_t Index = Initialized.size(); Index != 0; --Index) {
    const std::size_t Position = Index - 1;
    ExprRef Match = Expressions.eq(
        Offset, Expressions.constant(IndexWidth, Position));
    ExprRef Known = Initialized[Position];
    if (RequiredMask != 0xff) {
      ExprRef Mask = Expressions.constant(8, RequiredMask);
      Known = Expressions.eq(
          Expressions.bitAnd(KnownMasks[Position], Mask), Mask);
    }
    Result = Expressions.select(Match, Known, Result);
  }
  return Result;
}

ExprRef ObjectState::readByte(ExprRef Offset,
                              ExprContext &Expressions) const {
  return Expressions.read(Bytes, std::move(Offset));
}

ExprRef ObjectState::addressSensitiveAt(ExprRef Offset,
                                        ExprContext &Expressions) const {
  if (!Offset || Offset->width() != IndexWidth)
    throw MemoryModelException("address-sensitivity index width mismatch");
  if (Offset->isConstant()) {
    const llvm::APInt &Value = Offset->constantValue();
    if (Value.ugt(std::numeric_limits<std::uint64_t>::max()) ||
        Value.getZExtValue() >= AddressSensitiveBytes.size())
      return Expressions.boolean(false);
    return AddressSensitiveBytes[static_cast<std::size_t>(Value.getZExtValue())];
  }
  ExprRef Result = Expressions.boolean(false);
  for (std::size_t Index = AddressSensitiveBytes.size(); Index != 0; --Index) {
    const std::size_t Position = Index - 1;
    ExprRef Match = Expressions.eq(
        Offset, Expressions.constant(IndexWidth, Position));
    Result = Expressions.select(Match, AddressSensitiveBytes[Position], Result);
  }
  return Result;
}

void ObjectState::writeByte(ExprRef Offset, ExprRef Byte,
                            ExprContext &Expressions, bool Initializer,
                            ExprRef AddressSensitive) {
  if (Object->ReadOnly && !Initializer)
    throw MemoryModelException("writeByte on read-only memory object");
  if (!Offset || Offset->width() != IndexWidth || !Byte ||
      !Byte->sort().isBitVector() || Byte->width() != 8)
    throw MemoryModelException("invalid byte write");
  if (AddressSensitive && !AddressSensitive->sort().isBool())
    throw MemoryModelException("address sensitivity must be Bool");
  if (!AddressSensitive)
    AddressSensitive = Expressions.boolean(false);
  invalidateShadows(Offset, 1);
  if (Offset->isConstant()) {
    const llvm::APInt &Value = Offset->constantValue();
    if (Value.ugt(std::numeric_limits<std::uint64_t>::max()) ||
        Value.getZExtValue() >= Initialized.size())
      throw MemoryModelException("byte write outside object");
    Initialized[static_cast<std::size_t>(Value.getZExtValue())] =
        Expressions.boolean(true);
    KnownMasks[static_cast<std::size_t>(Value.getZExtValue())] =
        Expressions.constant(8, 0xff);
    AddressSensitiveBytes[static_cast<std::size_t>(Value.getZExtValue())] =
        AddressSensitive;
  } else {
    for (std::size_t Index = 0; Index < Initialized.size(); ++Index) {
      ExprRef Match = Expressions.eq(
          Offset, Expressions.constant(IndexWidth, Index));
      Initialized[Index] = Expressions.select(
          Match, Expressions.boolean(true), Initialized[Index]);
      KnownMasks[Index] = Expressions.select(
          Match, Expressions.constant(8, 0xff), KnownMasks[Index]);
      AddressSensitiveBytes[Index] = Expressions.select(
          Match, AddressSensitive, AddressSensitiveBytes[Index]);
    }
  }
  Bytes = Expressions.write(Bytes, std::move(Offset), std::move(Byte));
}

void ObjectState::setKnownMask(ExprRef Offset, std::uint8_t Mask,
                               ExprContext &Expressions) {
  if (!Offset || Offset->width() != IndexWidth)
    throw MemoryModelException("known-mask index width mismatch");
  ExprRef MaskExpr = Expressions.constant(8, Mask);
  ExprRef FullyKnown = Expressions.boolean(Mask == 0xff);
  if (Offset->isConstant()) {
    auto Index = Offset->constantValue().getZExtValue();
    if (Index >= KnownMasks.size())
      throw MemoryModelException("known-mask update outside object");
    KnownMasks[static_cast<std::size_t>(Index)] = MaskExpr;
    Initialized[static_cast<std::size_t>(Index)] = FullyKnown;
  } else {
    for (std::size_t Index = 0; Index < KnownMasks.size(); ++Index) {
      ExprRef Match = Expressions.eq(
          Offset, Expressions.constant(IndexWidth, Index));
      KnownMasks[Index] =
          Expressions.select(Match, MaskExpr, KnownMasks[Index]);
      Initialized[Index] =
          Expressions.select(Match, FullyKnown, Initialized[Index]);
    }
  }
}

void ObjectState::installShadow(PointerShadowEntry Entry, bool Initializer) {
  if (Object->ReadOnly && !Initializer)
    throw MemoryModelException("installShadow on read-only memory object");
  if (Entry.Size == 0 || Entry.Start > Object->Size ||
      Entry.Size > Object->Size - Entry.Start)
    throw MemoryModelException("pointer shadow outside object");
  Shadows[Entry.Start] = std::move(Entry);
}

const PointerShadowEntry *
ObjectState::findShadow(std::uint64_t Start, std::uint64_t Size,
                        unsigned Width, unsigned AddressSpace) const noexcept {
  auto It = Shadows.find(Start);
  if (It == Shadows.end() || It->second.Size != Size ||
      It->second.PointerWidth != Width ||
      It->second.AddressSpace != AddressSpace)
    return nullptr;
  return &It->second;
}

void ObjectState::invalidateShadows(ExprRef Offset, std::uint64_t Size) {
  if (!Offset || !Offset->isConstant()) {
    Shadows.clear();
    return;
  }
  const llvm::APInt &Value = Offset->constantValue();
  if (Value.ugt(std::numeric_limits<std::uint64_t>::max())) {
    Shadows.clear();
    return;
  }
  const std::uint64_t Start = Value.getZExtValue();
  for (auto It = Shadows.begin(); It != Shadows.end();) {
    const PointerShadowEntry &Entry = It->second;
    const bool Overlaps = Start < Entry.Start + Entry.Size &&
                          Entry.Start < Start + Size;
    if (Overlaps)
      It = Shadows.erase(It);
    else
      ++It;
  }
}

AddressSpace::WriteHandle::WriteHandle(std::shared_ptr<ObjectState> State,
                                      std::shared_ptr<std::size_t> Active)
    : State(std::move(State)), Active(std::move(Active)) {
  ++*this->Active;
}

AddressSpace::WriteHandle::WriteHandle(WriteHandle &&Other) noexcept
    : State(std::move(Other.State)), Active(std::move(Other.Active)) {}

AddressSpace::WriteHandle &
AddressSpace::WriteHandle::operator=(WriteHandle &&Other) noexcept {
  if (this != &Other) {
    release();
    State = std::move(Other.State);
    Active = std::move(Other.Active);
  }
  return *this;
}

AddressSpace::WriteHandle::~WriteHandle() { release(); }

void AddressSpace::WriteHandle::release() noexcept {
  if (Active) {
    --*Active;
    Active.reset();
  }
  State.reset();
}

AddressSpace::AddressSpace(const AddressSpace &Other)
    : Entries(Other.Entries), Bases(Other.Bases) {
  // A live write lease may be used after this copy. Detach its object in the
  // new address space now, while untouched objects retain ordinary COW.
  for (auto &[Id, Bound] : Entries) {
    (void)Id;
    if (*Bound.ActiveWriters != 0) {
      Bound.State = std::make_shared<ObjectState>(*Bound.State);
      Bound.ActiveWriters = std::make_shared<std::size_t>(0);
    }
  }
}

AddressSpace &AddressSpace::operator=(const AddressSpace &Other) {
  if (this != &Other) {
    AddressSpace Copy(Other);
    *this = std::move(Copy);
  }
  return *this;
}

bool AddressSpace::bind(std::shared_ptr<const MemoryObject> Object,
                        std::shared_ptr<ObjectState> State) {
  if (!Object || !State || &State->object() != Object.get())
    throw MemoryModelException("mismatched address-space binding");
  // Do not retain a mutable alias supplied by the caller. A moved-in unique
  // state needs no copy, while an lvalue shared_ptr remains caller-owned.
  if (!State.unique())
    State = std::make_shared<ObjectState>(*State);
  const std::uint64_t Id = Object->Id;
  if (Entries.count(Id) || Bases.count(Object->LogicalBase))
    return false;
  auto Next = Bases.lower_bound(Object->LogicalBase);
  if (Next != Bases.end() &&
      Object->Size > Next->first - Object->LogicalBase)
    throw MemoryModelException("overlapping logical memory objects");
  if (Next != Bases.begin()) {
    auto Previous = std::prev(Next);
    const MemoryObject &Prior = *Entries.at(Previous->second).Object;
    if (Prior.Size > Object->LogicalBase - Prior.LogicalBase)
      throw MemoryModelException("overlapping logical memory objects");
  }
  const std::uint64_t Base = Object->LogicalBase;
  Entries.emplace(Id, Entry{std::move(Object), std::move(State)});
  Bases.emplace(Base, Id);
  return true;
}

const AddressSpace::Entry *AddressSpace::find(std::uint64_t Id) const noexcept {
  auto It = Entries.find(Id);
  return It == Entries.end() ? nullptr : &It->second;
}

const AddressSpace::Entry *
AddressSpace::findConcrete(std::uint64_t Address) const noexcept {
  auto It = Bases.upper_bound(Address);
  if (It == Bases.begin())
    return nullptr;
  --It;
  const Entry *Candidate = find(It->second);
  if (Candidate &&
      Address - Candidate->Object->LogicalBase < Candidate->Object->Size)
    return Candidate;
  return nullptr;
}

bool AddressSpace::unbind(std::uint64_t Id) {
  auto It = Entries.find(Id);
  if (It == Entries.end())
    return false;
  Bases.erase(It->second.Object->LogicalBase);
  Entries.erase(It);
  return true;
}

AddressSpace::WriteHandle AddressSpace::getWriteable(std::uint64_t Id,
                                                     bool &Copied) {
  Copied = false;
  auto It = Entries.find(Id);
  if (It == Entries.end())
    return {};
  if (*It->second.ActiveWriters == 0 && !It->second.State.unique()) {
    It->second.State = std::make_shared<ObjectState>(*It->second.State);
    It->second.ActiveWriters = std::make_shared<std::size_t>(0);
    Copied = true;
  }
  // Bound states originate from mutable construction or the clone above.
  return WriteHandle(
      std::const_pointer_cast<ObjectState>(It->second.State),
      It->second.ActiveWriters);
}

} // namespace formal
