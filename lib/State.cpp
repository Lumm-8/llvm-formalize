#include "formal/State.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Value.h"

#include <algorithm>
#include <stdexcept>
#include <utility>

namespace formal {

void ProgramCounter::set(llvm::Function *Function, llvm::BasicBlock *Block,
                         llvm::Instruction *Instruction) noexcept {
  CurrentFunction = Function;
  CurrentBlock = Block;
  CurrentInstruction = Instruction;
}

void ProgramCounter::advance() noexcept {
  CurrentInstruction = CurrentInstruction ? CurrentInstruction->getNextNode()
                                          : nullptr;
}

void StackFrame::bind(const llvm::Value *Value, SymbolicValue Binding) {
  if (!Value)
    throw std::invalid_argument("cannot bind a null LLVM Value");
  Locals.insert_or_assign(Value, std::move(Binding));
}

const SymbolicValue *StackFrame::lookup(const llvm::Value *Value) const noexcept {
  auto Position = Locals.find(Value);
  return Position == Locals.end() ? nullptr : &Position->second;
}

namespace {

constexpr unsigned MaxJoinInputDepth = 128;

bool shallow(const ExprRef &Expression) {
  return !Expression || Expression->depth() <= MaxJoinInputDepth;
}

std::optional<SymbolicValue>
mergeValue(const SymbolicValue &Left, const SymbolicValue &Right,
           const ExprRef &Guard, ExprContext &Context, unsigned &RemainingBits,
           std::size_t MaxPointerCandidates) {
  if (Left.isPointer() != Right.isPointer() ||
      Left.isAggregate() != Right.isAggregate() || Left.width() != Right.width())
    return std::nullopt;
  auto Charge = [&](std::size_t Bits) {
    if (Bits > RemainingBits)
      return false;
    RemainingBits -= Bits;
    return true;
  };
  if (Left.isPointer()) {
    const auto &A = Left.pointer();
    const auto &B = Right.pointer();
    const auto Count = A.Candidates.size() + B.Candidates.size();
    auto Cheap = [](const PointerCandidate &Candidate) {
      return shallow(Candidate.Offset) && shallow(Candidate.Guard);
    };
    // Candidate pairs can become solver requests at later dereferences.
    constexpr std::size_t MaxJoinCandidates = 4;
    if (A.AddressSpace != B.AddressSpace || Count > MaxPointerCandidates ||
        Count > MaxJoinCandidates || A.Width > RemainingBits / Count ||
        !Charge(A.Width * Count) || !shallow(A.Numeric) ||
        !shallow(B.Numeric) ||
        !std::all_of(A.Candidates.begin(), A.Candidates.end(), Cheap) ||
        !std::all_of(B.Candidates.begin(), B.Candidates.end(), Cheap))
      return std::nullopt;
    return SymbolicValue(PointerValue::select(Guard, A, B, Context));
  }
  if (Left.isAggregate()) {
    const auto &A = Left.aggregate();
    const auto &B = Right.aggregate();
    if (A.Type != B.Type || A.Bytes.size() != B.Bytes.size() ||
        A.Bytes.size() > 16 || A.KnownMasks != B.KnownMasks ||
        A.AddressSensitiveBytes != B.AddressSensitiveBytes ||
        A.Shadows != B.Shadows)
      return std::nullopt;
    AggregateValue Result = A;
    for (std::size_t I = 0; I < A.Bytes.size(); ++I) {
      if (A.Bytes[I] == B.Bytes[I])
        continue;
      if (!Charge(8) || !shallow(A.Bytes[I]) || !shallow(B.Bytes[I]))
        return std::nullopt;
      Result.Bytes[I] = Context.select(Guard, A.Bytes[I], B.Bytes[I]);
    }
    return SymbolicValue(std::move(Result));
  }
  const auto &A = Left.poisonCondition();
  const auto &B = Right.poisonCondition();
  if (!Charge(Left.width()) || !shallow(Left.bitVector()) ||
      !shallow(Right.bitVector()) || !shallow(A) || !shallow(B))
    return std::nullopt;
  ExprRef Poison;
  if (A || B)
    Poison = Context.select(Guard, A ? A : Context.boolean(false),
                             B ? B : Context.boolean(false));
  return SymbolicValue(Context.select(Guard, Left.bitVector(),
                                      Right.bitVector()), std::move(Poison));
}

} // namespace

bool StackFrame::mergeLocalsFrom(const StackFrame &Other,
                                 const ProgramCounter &At,
                                 const llvm::DominatorTree &Dominators,
                                 const ExprRef &Guard,
                                 ExprContext &Expressions,
                                 std::size_t MaxPointerCandidates) {
  if (!sameLayout(Other))
    return false;
  for (const llvm::Argument &Argument : CurrentFunction->args()) {
    auto Left = lookup(&Argument);
    auto Right = Other.lookup(&Argument);
    if (static_cast<bool>(Left) != static_cast<bool>(Right) ||
        (Left && *Left != *Right))
      return false;
  }

  constexpr unsigned MaxChangedValues = 16;
  unsigned ChangedValues = 0;
  unsigned RemainingBits = 512;
  // Visit SSA definitions in IR order so building the merged expressions is
  // deterministic. Definitions exclusive to an arm are dead after the PHIs.
  for (const llvm::Instruction &Definition :
       llvm::instructions(*CurrentFunction)) {
    if (&Definition == At.instruction() ||
        !Dominators.dominates(&Definition, At.instruction())) {
      Locals.erase(&Definition);
      continue;
    }
    auto Left = Locals.find(&Definition);
    auto Right = Other.lookup(&Definition);
    if ((Left != Locals.end()) != static_cast<bool>(Right))
      return false;
    if (!Right || Left->second == *Right)
      continue;
    if (++ChangedValues > MaxChangedValues)
      return false;
    auto Merged = mergeValue(Left->second, *Right, Guard, Expressions,
                             RemainingBits, MaxPointerCandidates);
    if (!Merged)
      return false;
    Left->second = std::move(*Merged);
  }
  VisitedBlocks.insert(Other.VisitedBlocks.begin(), Other.VisitedBlocks.end());
  Predecessor = nullptr;
  return true;
}

bool StackFrame::sameLayout(const StackFrame &Other) const {
  return Id == Other.Id && CurrentFunction == Other.CurrentFunction &&
         CallSite == Other.CallSite && Continuation == Other.Continuation &&
         StackObjects == Other.StackObjects &&
         LoopActivations == Other.LoopActivations;
}

ExecutionState::ExecutionState(std::uint64_t Id, llvm::Function *Function)
    : Id(Id), Frames{StackFrame(Function)} {
  if (!Function)
    throw std::invalid_argument("execution requires a function");
  if (Function->empty()) {
    PC.set(Function, nullptr, nullptr);
    return;
  }
  llvm::BasicBlock &Entry = Function->getEntryBlock();
  PC.set(Function, &Entry, Entry.empty() ? nullptr : &Entry.front());
  markVisited(&Entry);
}

bool ExecutionState::canMergeWith(const ExecutionState &Other) const {
  return Status == ExecutionStateStatus::Running && Status == Other.Status &&
         PC.instruction() && PC == Other.PC && Depth == Other.Depth &&
         ExecutedInstructions == Other.ExecutedInstructions &&
         IncompleteReason == Other.IncompleteReason &&
         TerminationLocation == Other.TerminationLocation &&
         TerminationDetail == Other.TerminationDetail &&
         LoopPolicyError == Other.LoopPolicyError &&
         UnwindingAssumption == Other.UnwindingAssumption &&
         PendingLoopDiagnostic == Other.PendingLoopDiagnostic &&
         LoopHistory == Other.LoopHistory && Regions == Other.Regions &&
         MemoryReads == Other.MemoryReads && Events == Other.Events &&
         Frames.size() == Other.Frames.size() && !Frames.empty() &&
         frame().sameLayout(Other.frame()) &&
         // Suspended callers cannot change in a straight-line sibling arm.
         std::equal(Frames.begin(), Frames.end() - 1, Other.Frames.begin());
}

bool ExecutionState::hasVisited(const llvm::BasicBlock *Block) const noexcept {
  return frame().hasVisited(Block);
}

void ExecutionState::markVisited(const llvm::BasicBlock *Block) {
  if (!Block)
    throw std::invalid_argument("cannot mark a null basic block visited");
  frame().markVisited(Block);
}

} // namespace formal
