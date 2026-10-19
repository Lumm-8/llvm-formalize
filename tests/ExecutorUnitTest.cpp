#include "formal/Executor.h"

#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace formal;

namespace {

class ScriptedSolver final : public Solver {
public:
  explicit ScriptedSolver(std::vector<SolverResult> Results)
      : Results(std::move(Results)) {}

  SolverResult check(const ConstraintSet &) override { return unused(); }

  SolverResult mayBeTrue(const ConstraintSet &, const ExprRef &) override {
    ++QueryCount;
    if (Next >= Results.size())
      return unused();
    return Results[Next++];
  }

  SolverResult mustBeTrue(const ConstraintSet &, const ExprRef &) override {
    return unused();
  }

  SolverResult getValue(const ConstraintSet &, const ExprRef &) override {
    return unused();
  }

  SolverResult
  getInitialValues(const ConstraintSet &,
                   const std::vector<ArrayModelRequest> &) override {
    return unused();
  }

  std::size_t QueryCount = 0;

private:
  static SolverResult unused() {
    SolverResult Result;
    Result.Status = SolverStatus::InternalError;
    Result.Metadata.Reason = SolverReason::InvalidQuery;
    Result.Metadata.Detail = "unexpected mock solver query";
    return Result;
  }

  std::vector<SolverResult> Results;
  std::size_t Next = 0;
};

SolverResult queryResult(SolverStatus Status, SolverReason Reason,
                         const char *Detail) {
  SolverResult Result;
  Result.Status = Status;
  Result.Metadata.Reason = Reason;
  Result.Metadata.Detail = Detail;
  if (Status == SolverStatus::Sat)
    Result.Truth = true;
  else if (Status == SolverStatus::Unsat)
    Result.Truth = false;
  return Result;
}

std::unique_ptr<llvm::Module> parseModule(llvm::LLVMContext &Context,
                                          const char *IR) {
  llvm::SMDiagnostic Diagnostic;
  return llvm::parseAssemblyString(IR, Diagnostic, Context);
}

ExecutionResult executeBranch(ScriptedSolver &Solver) {
  static constexpr char IR[] = R"(
    define i8 @branch(i1 %condition) {
    entry:
      br i1 %condition, label %yes, label %no
    yes:
      ret i8 1
    no:
      ret i8 0
    }
  )";
  llvm::LLVMContext LLVMContext;
  std::unique_ptr<llvm::Module> Module = parseModule(LLVMContext, IR);
  EXPECT_NE(Module, nullptr);
  if (!Module)
    return {};
  llvm::Function *Function = Module->getFunction("branch");
  ExprContext Expressions;
  ExprRef Input = Expressions.symbol(1, "condition");
  ScalarExecutor Executor(Expressions, Solver);
  return Executor.execute(
      *Function, {{Function->getArg(0), SymbolicValue(std::move(Input))}});
}

TEST(FormalExecutor, BothInfeasibleSuccessorsAreAnInternalError) {
  ScriptedSolver Solver({queryResult(SolverStatus::Unsat,
                                     SolverReason::Unsatisfiable, "false"),
                         queryResult(SolverStatus::Unsat,
                                     SolverReason::Unsatisfiable, "false")});
  ExecutionResult Result = executeBranch(Solver);
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::InternalError);
  EXPECT_EQ(Result.Paths[0].Status, PathStatus::InternalError);
  EXPECT_EQ(Result.Paths[0].InternalReason,
            InternalErrorReason::NoFeasibleSuccessor);
  EXPECT_EQ(Result.Statistics.PrunedPaths, 2U);
  EXPECT_EQ(Result.Statistics.SolverQueries, 2U);
}

TEST(FormalExecutor, SolverTimeoutMakesBothAffectedPathsIncomplete) {
  ScriptedSolver Solver({queryResult(SolverStatus::Timeout,
                                     SolverReason::Timeout, "deadline"),
                         queryResult(SolverStatus::Timeout,
                                     SolverReason::Timeout, "deadline")});
  ExecutionResult Result = executeBranch(Solver);
  ASSERT_EQ(Result.Paths.size(), 2U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Statistics.PrunedPaths, 0U);
  EXPECT_EQ(Result.Statistics.Forks, 1U);
  for (const PathResult &Path : Result.Paths) {
    EXPECT_EQ(Path.Status, PathStatus::Incomplete);
    EXPECT_EQ(Path.IncompleteReason, IncompletenessReason::SolverTimeout);
    EXPECT_EQ(Path.Constraints.size(), 1U);
  }
}

TEST(FormalExecutor, WorkerFailureIsNotTreatedAsUnsat) {
  ScriptedSolver Solver({queryResult(SolverStatus::Unknown,
                                     SolverReason::WorkerFailure, "crashed"),
                         queryResult(SolverStatus::Unsat,
                                     SolverReason::Unsatisfiable, "false")});
  ExecutionResult Result = executeBranch(Solver);
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::SolverFailure);
  EXPECT_EQ(Result.Statistics.PrunedPaths, 1U);
}

TEST(FormalExecutor, BackendUnknownIsNotTreatedAsUnsat) {
  ScriptedSolver Solver({queryResult(SolverStatus::Unknown,
                                     SolverReason::BackendUnknown, "unknown"),
                         queryResult(SolverStatus::Unsat,
                                     SolverReason::Unsatisfiable, "false")});
  ExecutionResult Result = executeBranch(Solver);
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::SolverUnknown);
}

TEST(FormalExecutor, SolverInternalErrorRemainsAnInternalError) {
  ScriptedSolver Solver({queryResult(SolverStatus::InternalError,
                                     SolverReason::TranslationError, "bad"),
                         queryResult(SolverStatus::Unsat,
                                     SolverReason::Unsatisfiable, "false")});
  ExecutionResult Result = executeBranch(Solver);
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::InternalError);
  EXPECT_EQ(Result.Paths[0].InternalReason,
            InternalErrorReason::SolverInternalError);
}

TEST(FormalExecutor, MissingArgumentBindingNeverCreatesAFreshSymbol) {
  static constexpr char IR[] = R"(
    define i8 @identity(i8 %x) {
    entry:
      ret i8 %x
    }
  )";
  llvm::LLVMContext LLVMContext;
  std::unique_ptr<llvm::Module> Module = parseModule(LLVMContext, IR);
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({});
  ExprContext Expressions;
  ScalarExecutor Executor(Expressions, Solver);
  ExecutionResult Result = Executor.execute(*Module->getFunction("identity"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::InternalError);
  EXPECT_EQ(Result.Paths[0].InternalReason,
            InternalErrorReason::MissingSSABinding);
  EXPECT_EQ(Solver.QueryCount, 0U);
}

TEST(FormalExecutor, DeclarationIsReportedWithoutThrowing) {
  llvm::LLVMContext LLVMContext;
  std::unique_ptr<llvm::Module> Module =
      parseModule(LLVMContext, "declare i8 @external(i8)");
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({});
  ExprContext Expressions;
  ScalarExecutor Executor(Expressions, Solver);
  llvm::Function *Function = Module->getFunction("external");
  ExecutionResult Result = Executor.execute(
      *Function,
      {{Function->getArg(0),
        SymbolicValue(Expressions.symbol(8, "external_argument"))}});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::InternalError);
  EXPECT_EQ(Result.Paths[0].InternalReason,
            InternalErrorReason::InvalidExecutorState);
}

TEST(FormalExecutor, ConstraintManagerAcceptsBoolAndRejectsBV1) {
  ExprContext Expressions;
  ConstraintManager Constraints;
  Constraints.add(Expressions.boolean(true));
  EXPECT_EQ(Constraints.size(), 1U);
  EXPECT_THROW(Constraints.add(Expressions.constant(1, 1)),
               std::invalid_argument);
}

TEST(FormalExecutor, ParallelPHIsReadOnePreEntrySnapshot) {
  static constexpr char IR[] = R"(
    define i8 @swap(i8 %x, i8 %y) {
    entry:
      br label %loop
    loop:
      %a = phi i8 [ %x, %entry ], [ %b, %loop ]
      %b = phi i8 [ %y, %entry ], [ %a, %loop ]
      br label %loop
    }
  )";
  llvm::LLVMContext LLVMContext;
  std::unique_ptr<llvm::Module> Module = parseModule(LLVMContext, IR);
  ASSERT_NE(Module, nullptr);
  llvm::Function *Function = Module->getFunction("swap");
  llvm::BasicBlock *Loop = &Function->back();
  auto *A = llvm::cast<llvm::PHINode>(&Loop->front());
  auto *B = llvm::cast<llvm::PHINode>(A->getNextNode());

  ExprContext Expressions;
  ScriptedSolver Solver({});
  ScalarExecutor Executor(Expressions, Solver);
  InstructionExecutor Instructions(Executor);
  ExecutionState State(1, Function);
  State.frame().bind(A, SymbolicValue(Expressions.constant(8, 17)));
  State.frame().bind(B, SymbolicValue(Expressions.constant(8, 29)));
  State.setPredecessor(Loop);
  State.pc().set(Function, Loop, A);

  Instructions.execute(State);
  const SymbolicValue *NewA = State.frame().lookup(A);
  const SymbolicValue *NewB = State.frame().lookup(B);
  ASSERT_NE(NewA, nullptr);
  ASSERT_NE(NewB, nullptr);
  EXPECT_EQ(NewA->bitVector()->constantValue(), llvm::APInt(8, 29));
  EXPECT_EQ(NewB->bitVector()->constantValue(), llvm::APInt(8, 17));
}

} // namespace
