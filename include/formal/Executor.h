#ifndef FORMAL_EXECUTOR_H
#define FORMAL_EXECUTOR_H

#include "formal/Result.h"
#include "formal/Solver.h"
#include "formal/State.h"

#include "llvm/ADT/ArrayRef.h"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

namespace llvm {
class Argument;
class Function;
} // namespace llvm

namespace formal {

struct ArgumentBinding {
  const llvm::Argument *Argument = nullptr;
  SymbolicValue Value;
};

enum class LoopLimitPolicy { Unknown, Error, AssumeExit };
const char *toString(LoopLimitPolicy Policy) noexcept;

struct ExecutorOptions {
  std::uint64_t MaxInstructionsPerState = 100000;
  std::size_t MaxStates = 10000;
  std::size_t MaxPointerCandidates = 64;
  std::uint64_t MaxObjectBytes = 65536;
  std::size_t MaxCallDepth = 32;
  std::uint64_t DefaultLoopBound = 32;
  bool RequireLocalLoopBound = false;
  LoopLimitPolicy LoopPolicy = LoopLimitPolicy::Unknown;
  std::uint64_t MaxWallTimeMs = 0; // Zero disables the run-wide deadline.
  std::uint64_t MaxSolverQueries = UINT64_MAX;
  std::uint64_t SolverTimeoutMs = 30000;
  bool EnableMemory = true;
  bool EnableRegistration = false;
  bool ModelAllocationFailure = false;
};

class StateScheduler {
public:
  virtual ~StateScheduler() = default;
  virtual void schedule(ExecutionState State) = 0;
  virtual bool empty() const noexcept = 0;
  virtual ExecutionState takeNext() = 0;
};

class DFSWorklist final : public StateScheduler {
public:
  void schedule(ExecutionState State) override;
  bool empty() const noexcept override { return States.empty(); }
  ExecutionState takeNext() override;
  void clear() noexcept { States.clear(); }

private:
  std::vector<ExecutionState> States;
};

class ScalarExecutor;

class InstructionExecutor final {
public:
  explicit InstructionExecutor(ScalarExecutor &Executor) : Executor(Executor) {}
  void execute(ExecutionState &State);

private:
  ScalarExecutor &Executor;
};

class ScalarExecutor final {
public:
  ScalarExecutor(ExprContext &Expressions, Solver &Solver,
                 ExecutorOptions Options = {});
  ~ScalarExecutor();
  ScalarExecutor(const ScalarExecutor &) = delete;
  ScalarExecutor &operator=(const ScalarExecutor &) = delete;

  ExecutionResult execute(llvm::Function &Function,
                          llvm::ArrayRef<ArgumentBinding> Arguments);

private:
  friend class InstructionExecutor;
  struct Impl;
  std::unique_ptr<Impl> Implementation;
};

} // namespace formal

#endif // FORMAL_EXECUTOR_H
