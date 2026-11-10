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
  struct LoopActivation {
    std::uint64_t Id = 0;
    std::uint64_t Traversals = 0;
    std::uint64_t Bound = 0;
    std::string BoundSource;
  };
  explicit StackFrame(llvm::Function *Function = nullptr)
      : CurrentFunction(Function) {}

  llvm::Function *function() const noexcept { return CurrentFunction; }
  std::uint64_t id() const noexcept { return Id; }
  void setId(std::uint64_t NewId) noexcept { Id = NewId; }
  void bind(const llvm::Value *Value, SymbolicValue Binding);
  const SymbolicValue *lookup(const llvm::Value *Value) const noexcept;
  void addStackObject(std::uint64_t Id) { StackObjects.push_back(Id); }
  const std::vector<std::uint64_t> &stackObjects() const noexcept {
    return StackObjects;
  }
  void setCallSite(llvm::Instruction *Call) noexcept { CallSite = Call; }
  llvm::Instruction *callSite() const noexcept { return CallSite; }
  void setContinuation(ProgramCounter Next) noexcept { Continuation = Next; }
  const ProgramCounter &continuation() const noexcept { return Continuation; }
  bool hasVisited(const llvm::BasicBlock *Block) const noexcept {
    return VisitedBlocks.count(Block) != 0;
  }
  void markVisited(const llvm::BasicBlock *Block) {
    VisitedBlocks.insert(Block);
  }
  llvm::BasicBlock *predecessor() const noexcept { return Predecessor; }
  void setPredecessor(llvm::BasicBlock *Block) noexcept { Predecessor = Block; }
  std::unordered_map<const llvm::BasicBlock *, LoopActivation> &
  loopActivations() noexcept {
    return LoopActivations;
  }
  const std::unordered_map<const llvm::BasicBlock *, LoopActivation> &
  loopActivations() const noexcept {
    return LoopActivations;
  }

private:
  std::uint64_t Id = 0;
  llvm::Function *CurrentFunction = nullptr;
  llvm::Instruction *CallSite = nullptr;
  ProgramCounter Continuation;
  llvm::BasicBlock *Predecessor = nullptr;
  std::unordered_map<const llvm::Value *, SymbolicValue> Locals;
  std::vector<std::uint64_t> StackObjects;
  std::unordered_set<const llvm::BasicBlock *> VisitedBlocks;
  std::unordered_map<const llvm::BasicBlock *, LoopActivation> LoopActivations;
};

class ExecutionState final {
public:
  struct MemoryRead {
    std::uint64_t ObjectId = 0;
    ExprRef Offset;
    std::uint64_t Size = 0;
  };
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
  llvm::BasicBlock *predecessor() const noexcept {
    return frame().predecessor();
  }
  void setPredecessor(llvm::BasicBlock *Block) noexcept {
    frame().setPredecessor(Block);
  }

  StackFrame &frame() noexcept { return Frames.back(); }
  const StackFrame &frame() const noexcept { return Frames.back(); }
  std::vector<StackFrame> &frames() noexcept { return Frames; }
  const std::vector<StackFrame> &frames() const noexcept { return Frames; }

  ConstraintManager &constraints() noexcept { return Constraints; }
  const ConstraintManager &constraints() const noexcept { return Constraints; }
  AddressSpace &addressSpace() noexcept { return Memory; }
  const AddressSpace &addressSpace() const noexcept { return Memory; }
  std::vector<PathResult::RegisteredRegion> &regions() noexcept {
    return Regions;
  }
  const std::vector<PathResult::RegisteredRegion> &regions() const noexcept {
    return Regions;
  }
  std::vector<MemoryRead> &memoryReads() noexcept { return MemoryReads; }
  const std::vector<MemoryRead> &memoryReads() const noexcept {
    return MemoryReads;
  }
  std::vector<ExecutionEvent> &events() noexcept { return Events; }
  const std::vector<ExecutionEvent> &events() const noexcept { return Events; }

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
  void setLoopPolicyError() noexcept { LoopPolicyError = true; }
  bool loopPolicyError() const noexcept { return LoopPolicyError; }
  void setUnwindingAssumption() noexcept { UnwindingAssumption = true; }
  bool unwindingAssumption() const noexcept { return UnwindingAssumption; }
  void setLoopDiagnostic(LoopDiagnostic Diagnostic) {
    PendingLoopDiagnostic = std::move(Diagnostic);
  }
  const std::optional<LoopDiagnostic> &loopDiagnostic() const noexcept {
    return PendingLoopDiagnostic;
  }
  std::vector<LoopDiagnostic> &loopHistory() noexcept { return LoopHistory; }
  const std::vector<LoopDiagnostic> &loopHistory() const noexcept {
    return LoopHistory;
  }

private:
  std::uint64_t Id = 0;
  unsigned Depth = 0;
  std::uint64_t ExecutedInstructions = 0;
  ExecutionStateStatus Status = ExecutionStateStatus::Running;
  ProgramCounter PC;
  std::vector<StackFrame> Frames;
  ConstraintManager Constraints;
  AddressSpace Memory;
  std::vector<PathResult::RegisteredRegion> Regions;
  std::vector<MemoryRead> MemoryReads;
  std::vector<ExecutionEvent> Events;
  IncompletenessReason IncompleteReason = IncompletenessReason::None;
  IRLocation TerminationLocation;
  std::string TerminationDetail;
  bool LoopPolicyError = false;
  bool UnwindingAssumption = false;
  std::optional<LoopDiagnostic> PendingLoopDiagnostic;
  std::vector<LoopDiagnostic> LoopHistory;
};

} // namespace formal

#endif // FORMAL_STATE_H
