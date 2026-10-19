#include "formal/State.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <chrono>
#include <cstdint>
#include <iostream>
#include <memory>
#include <vector>
#include <sys/resource.h>

int main() {
  constexpr std::uint64_t ObjectCount = 128;
  constexpr std::uint64_t ForkCount = 64;
  llvm::LLVMContext Context;
  llvm::Module Module("memory-benchmark", Context);
  auto *Type = llvm::FunctionType::get(llvm::Type::getVoidTy(Context), false);
  llvm::Function *Function = llvm::Function::Create(
      Type, llvm::GlobalValue::ExternalLinkage, "bench", Module);
  formal::ExprContext Expressions;
  formal::ExecutionState Parent(1, Function);
  for (std::uint64_t Id = 1; Id <= ObjectCount; ++Id) {
    auto Object = std::make_shared<formal::MemoryObject>();
    Object->Id = Id;
    Object->Kind = formal::AllocationKind::Stack;
    Object->Size = 16;
    Object->Alignment = 16;
    Object->LogicalBase = 0x10000 + Id * 32;
    Object->AllocationSite = "bench";
    if (!Parent.addressSpace().bind(
            Object, std::make_shared<formal::ObjectState>(Object, Expressions,
                                                          64)))
      return 1;
    Parent.frame().addStackObject(Id);
  }

  auto Start = std::chrono::steady_clock::now();
  std::vector<formal::ExecutionState> Forks;
  Forks.reserve(ForkCount);
  for (std::uint64_t Fork = 0; Fork < ForkCount; ++Fork)
    Forks.push_back(Parent);
  std::uint64_t Copies = 0;
  for (std::uint64_t Fork = 0; Fork < ForkCount; ++Fork) {
    auto &Child = Forks[static_cast<std::size_t>(Fork)];
    for (std::uint64_t Id = 1; Id <= ObjectCount; ++Id)
      if (Child.addressSpace().find(Id)->State.get() !=
          Parent.addressSpace().find(Id)->State.get())
        return 2;
    std::uint64_t Id = (Fork % ObjectCount) + 1;
    bool Copied = false;
    auto Writable = Child.addressSpace().getWriteable(Id, Copied);
    if (!Writable || !Copied)
      return 3;
    ++Copies;
    Writable->writeByte(Expressions.constant(64, 0),
                        Expressions.constant(8, Fork), Expressions);
    if (Child.addressSpace().find(Id)->State.get() ==
        Parent.addressSpace().find(Id)->State.get())
      return 4;
    for (std::uint64_t Other = 1; Other <= ObjectCount; ++Other)
      if (Other != Id && Child.addressSpace().find(Other)->State.get() !=
                             Parent.addressSpace().find(Other)->State.get())
        return 5;
  }
  auto End = std::chrono::steady_clock::now();
  struct rusage Usage {};
  getrusage(RUSAGE_SELF, &Usage);
  std::cout << "seed=none stack_objects=" << ObjectCount
            << " forks=" << ForkCount << " cow_copies=" << Copies
            << " elapsed_us="
            << std::chrono::duration_cast<std::chrono::microseconds>(End -
                                                                   Start)
                   .count()
            << " peak_bytes="
            << static_cast<std::uint64_t>(Usage.ru_maxrss) * 1024 << '\n';
  return Copies == ForkCount ? 0 : 6;
}
