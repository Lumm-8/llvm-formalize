#ifndef FORMAL_MEMORY_H
#define FORMAL_MEMORY_H

#include "formal/Expr.h"

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace formal {

class MemoryModelException final : public std::logic_error {
public:
  using std::logic_error::logic_error;
};

enum class AllocationKind { Global, Stack };

// Object identity and layout are immutable.  Liveness belongs to ObjectState:
// sibling execution states may end a stack lifetime independently.
struct MemoryObject final {
  std::uint64_t Id = 0;
  AllocationKind Kind = AllocationKind::Stack;
  std::uint64_t Size = 0;
  std::uint64_t Alignment = 1;
  std::uint64_t LogicalBase = 0;
  std::string AllocationSite;
  bool ReadOnly = false;
  // A constant global with unnamed_addr may share its address with another
  // constant global, including one whose address is significant.
  bool MergeableGlobalAddress = false;
  // The IR initializer does not describe the value observed at entry.
  bool ExternallyInitialized = false;
};

struct PointerCandidate final {
  std::shared_ptr<const MemoryObject> Object; // null denotes null pointer
  ExprRef Offset;                              // signed pointer-width offset
  ExprRef Guard;                               // Bool; null means unconditional
};

struct PointerValue final {
  unsigned AddressSpace = 0;
  unsigned Width = 0;
  std::vector<PointerCandidate> Candidates;
  ExprRef Numeric; // optional exact numeric representation, never provenance

  static PointerValue null(unsigned Width, ExprContext &Expressions,
                           unsigned AddressSpace = 0);
  static PointerValue object(std::shared_ptr<const MemoryObject> Object,
                             ExprRef Offset, ExprContext &Expressions,
                             unsigned AddressSpace = 0);
};

struct PointerShadowEntry final {
  std::uint64_t Start = 0;
  std::uint64_t Size = 0;
  unsigned PointerWidth = 0;
  unsigned AddressSpace = 0;
  PointerValue Pointer;
  ExprRef Numeric;
};

// Updates are immutable M1 nodes.  Only the small initialization map and
// pointer shadows are copied when an AddressSpace first writes a shared state.
class ObjectState final {
public:
  ObjectState(std::shared_ptr<const MemoryObject> Object,
              ExprContext &Expressions, unsigned IndexWidth);

  const MemoryObject &object() const noexcept { return *Object; }
  bool live() const noexcept { return Live; }
  void endLifetime() noexcept { Live = false; }
  const UpdateList &bytes() const noexcept { return Bytes; }
  const std::vector<ExprRef> &initialization() const noexcept {
    return Initialized;
  }
  const std::map<std::uint64_t, PointerShadowEntry> &shadows() const noexcept {
    return Shadows;
  }

  ExprRef initializedAt(ExprRef Offset, ExprContext &Expressions,
                        std::uint8_t RequiredMask = 0xff) const;
  ExprRef addressSensitiveAt(ExprRef Offset, ExprContext &Expressions) const;
  ExprRef readByte(ExprRef Offset, ExprContext &Expressions) const;
  void writeByte(ExprRef Offset, ExprRef Byte, ExprContext &Expressions,
                 bool Initializer = false,
                 ExprRef AddressSensitive = nullptr);
  void setKnownMask(ExprRef Offset, std::uint8_t Mask,
                    ExprContext &Expressions);
  void installShadow(PointerShadowEntry Entry, bool Initializer = false);
  const PointerShadowEntry *findShadow(std::uint64_t Start,
                                       std::uint64_t Size,
                                       unsigned Width,
                                       unsigned AddressSpace) const noexcept;
  void invalidateShadows(ExprRef Offset, std::uint64_t Size);

private:
  std::shared_ptr<const MemoryObject> Object;
  UpdateList Bytes;
  std::vector<ExprRef> Initialized; // Bool per byte
  std::vector<ExprRef> KnownMasks; // BV8; partial-byte LLVM stores
  std::vector<ExprRef> AddressSensitiveBytes; // Bool per byte
  std::map<std::uint64_t, PointerShadowEntry> Shadows;
  unsigned IndexWidth;
  bool Live = true;
};

class AddressSpace final {
public:
  class WriteHandle final {
  public:
    WriteHandle() = default;
    WriteHandle(const WriteHandle &) = delete;
    WriteHandle &operator=(const WriteHandle &) = delete;
    WriteHandle(WriteHandle &&Other) noexcept;
    WriteHandle &operator=(WriteHandle &&Other) noexcept;
    ~WriteHandle();
    explicit operator bool() const noexcept { return static_cast<bool>(State); }
    ObjectState *operator->() const noexcept { return State.get(); }
    ObjectState &operator*() const noexcept { return *State; }

  private:
    friend class AddressSpace;
    WriteHandle(std::shared_ptr<ObjectState> State,
                std::shared_ptr<std::size_t> Active);
    void release() noexcept;
    std::shared_ptr<ObjectState> State;
    std::shared_ptr<std::size_t> Active;
  };

  struct Entry final {
    std::shared_ptr<const MemoryObject> Object;
    std::shared_ptr<const ObjectState> State;
    std::shared_ptr<std::size_t> ActiveWriters = std::make_shared<std::size_t>(0);
  };

  AddressSpace() = default;
  AddressSpace(const AddressSpace &Other);
  AddressSpace &operator=(const AddressSpace &Other);
  AddressSpace(AddressSpace &&) noexcept = default;
  AddressSpace &operator=(AddressSpace &&) noexcept = default;

  bool bind(std::shared_ptr<const MemoryObject> Object,
            std::shared_ptr<ObjectState> State);
  const Entry *find(std::uint64_t Id) const noexcept;
  const Entry *findConcrete(std::uint64_t Address) const noexcept;
  bool unbind(std::uint64_t Id);
  WriteHandle getWriteable(std::uint64_t Id, bool &Copied);
  const std::map<std::uint64_t, Entry> &entries() const noexcept {
    return Entries;
  }

private:
  std::map<std::uint64_t, Entry> Entries;
  std::map<std::uint64_t, std::uint64_t> Bases; // logical base -> object id
};

} // namespace formal

#endif // FORMAL_MEMORY_H
