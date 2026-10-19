#include "formal/State.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Value.h"

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

bool ExecutionState::hasVisited(const llvm::BasicBlock *Block) const noexcept {
  return VisitedBlocks.find(Block) != VisitedBlocks.end();
}

void ExecutionState::markVisited(const llvm::BasicBlock *Block) {
  if (!Block)
    throw std::invalid_argument("cannot mark a null basic block visited");
  VisitedBlocks.insert(Block);
}

} // namespace formal
