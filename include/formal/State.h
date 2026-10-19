#ifndef FORMAL_STATE_H
#define FORMAL_STATE_H

#include "formal/Constraints.h"
#include "formal/Result.h"
#include "formal/Value.h"

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace llvm {
class Argument;
class BasicBlock;
class Function;
class Instruction;
class Value;
} // namespace llvm

namespace formal {

enum class ExecutionStateStatus {
  Running,
  Completed,
  Error,
  Incomplete,
  // Internal scheduler marker; it is never exposed as a PathResult status.
  Scheduled,
};

class ProgramCounter final {
public:
  ProgramCounter() = default;
  ProgramCounter(llvm::Function *Function, llvm::BasicBlock *Block,
                 llvm::Instruction *Instruction)
      : CurrentFunction(Function), CurrentBlock(Block),
        CurrentInstruction(Instruction) {}

  llvm::Function *function() const noexcept { return CurrentFunction; }
  llvm::BasicBlock *block() const noexcept { return CurrentBlock; }
  llvm::Instruction *instruction() const noexcept {
    return CurrentInstruction;
  }

  void set(llvm::Function *Function, llvm::BasicBlock *Block,
           llvm::Instruction *Instruction) noexcept;
  void advance() noexcept;

private:
  llvm::Function *CurrentFunction = nullptr;
  llvm::BasicBlock *CurrentBlock = nullptr;
  llvm::Instruction *CurrentInstruction = nullptr;
};

class StackFrame final {
public:
  explicit StackFrame(llvm::Function *Function = nullptr)
      : CurrentFunction(Function) {}

  llvm::Function *function() const noexcept { return CurrentFunction; }
  void bind(const llvm::Value *Value, SymbolicValue Binding);
  const SymbolicValue *lookup(const llvm::Value *Value) const noexcept;
  void addStackObject(std::uint64_t Id) { StackObjects.push_back(Id); }
  const std::vector<std::uint64_t> &stackObjects() const noexcept {
    return StackObjects;
  }

private:
  llvm::Function *CurrentFunction = nullptr;
  std::unordered_map<const llvm::Value *, SymbolicValue> Locals;
  std::vector<std::uint64_t> StackObjects;
};

class ExecutionState final {
public:
  ExecutionState(std::uint64_t Id, llvm::Function *Function);

  std::uint64_t id() const noexcept { return Id; }
  void setId(std::uint64_t NewId) noexcept { Id = NewId; }
  unsigned depth() const noexcept { return Depth; }
  void setDepth(unsigned NewDepth) noexcept { Depth = NewDepth; }
  std::uint64_t executedInstructions() const noexcept {
    return ExecutedInstructions;
  }
  void addExecutedInstructions(std::uint64_t Count) noexcept {
    ExecutedInstructions += Count;
  }

  ExecutionStateStatus status() const noexcept { return Status; }
  void setStatus(ExecutionStateStatus NewStatus) noexcept { Status = NewStatus; }

  ProgramCounter &pc() noexcept { return PC; }
  const ProgramCounter &pc() const noexcept { return PC; }
  llvm::BasicBlock *predecessor() const noexcept { return Predecessor; }
  void setPredecessor(llvm::BasicBlock *Block) noexcept { Predecessor = Block; }

  StackFrame &frame() noexcept { return Frames.back(); }
  const StackFrame &frame() const noexcept { return Frames.back(); }
  std::vector<StackFrame> &frames() noexcept { return Frames; }
  const std::vector<StackFrame> &frames() const noexcept { return Frames; }

  ConstraintManager &constraints() noexcept { return Constraints; }
  const ConstraintManager &constraints() const noexcept { return Constraints; }
  AddressSpace &addressSpace() noexcept { return Memory; }
  const AddressSpace &addressSpace() const noexcept { return Memory; }

  bool hasVisited(const llvm::BasicBlock *Block) const noexcept;
  void markVisited(const llvm::BasicBlock *Block);

  IncompletenessReason incompleteReason() const noexcept {
    return IncompleteReason;
  }
  const IRLocation &terminationLocation() const noexcept {
    return TerminationLocation;
  }
  const std::string &terminationDetail() const noexcept {
    return TerminationDetail;
  }
  void setIncomplete(IncompletenessReason Reason, IRLocation Location,
                     std::string Detail) {
    Status = ExecutionStateStatus::Incomplete;
    IncompleteReason = Reason;
    TerminationLocation = std::move(Location);
    TerminationDetail = std::move(Detail);
  }
  void setTerminationLocation(IRLocation Location, std::string Detail) {
    TerminationLocation = std::move(Location);
    TerminationDetail = std::move(Detail);
  }

private:
  std::uint64_t Id = 0;
  unsigned Depth = 0;
  std::uint64_t ExecutedInstructions = 0;
  ExecutionStateStatus Status = ExecutionStateStatus::Running;
  ProgramCounter PC;
  llvm::BasicBlock *Predecessor = nullptr;
  std::vector<StackFrame> Frames;
  ConstraintManager Constraints;
  AddressSpace Memory;
  std::unordered_set<const llvm::BasicBlock *> VisitedBlocks;
  IncompletenessReason IncompleteReason = IncompletenessReason::None;
  IRLocation TerminationLocation;
  std::string TerminationDetail;
};

} // namespace formal

#endif // FORMAL_STATE_H
