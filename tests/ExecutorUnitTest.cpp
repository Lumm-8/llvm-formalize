#include "formal/Executor.h"

#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

#include <cstddef>
#include <chrono>
#include <memory>
#include <string>
#include <thread>
#include <utility>
#include <vector>

using namespace formal;

namespace {

class ScriptedSolver final : public Solver {
public:
  explicit ScriptedSolver(std::vector<SolverResult> Results,
                          std::chrono::milliseconds Delay = {})
      : Results(std::move(Results)), Delay(Delay) {}

  SolverResult check(const ConstraintSet &) override { return unused(); }

  SolverResult mayBeTrue(const ConstraintSet &, const ExprRef &) override {
    if (Delay.count())
      std::this_thread::sleep_for(Delay);
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
  std::chrono::milliseconds Delay;
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

TEST(FormalExecutor, UnknownDirectCallKeepsCalleeAndCallSite) {
  static constexpr char IR[] = R"(
    declare i32 @unmodeled(i32)
    define i32 @caller() {
    entry:
      %value = call i32 @unmodeled(i32 5)
      ret i32 %value
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  ScriptedSolver Solver({});
  ScalarExecutor Executor(Expressions, Solver);
  ExecutionResult Result = Executor.execute(*Module->getFunction("caller"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::UnsupportedCall);
  EXPECT_EQ(Result.Paths[0].Location.Function, "caller");
  EXPECT_EQ(Result.Paths[0].Location.BasicBlock, "entry");
  EXPECT_EQ(Result.Paths[0].Location.Opcode, "call");
  EXPECT_EQ(Result.Paths[0].Detail, "unmodeled external call: unmodeled");
}

TEST(FormalExecutor, LoopBackedgeBoundIsCheckedBeforeTraversal) {
  static constexpr char IR[] = R"(
    define i32 @entry() {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %next, %latch ]
      %continue = icmp ult i32 %i, 3
      br i1 %continue, label %latch, label %done
    latch:
      %next = add i32 %i, 1
      br label %header
    done:
      ret i32 %i
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  for (unsigned Bound : {2U, 3U}) {
    ExprContext Expressions;
    STPSolver Solver;
    ExecutorOptions Options;
    Options.DefaultLoopBound = Bound;
    ScalarExecutor Executor(Expressions, Solver, Options);
    ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    if (Bound == 2) {
      EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
      EXPECT_EQ(Result.Paths[0].IncompleteReason,
                IncompletenessReason::LoopBound);
      EXPECT_EQ(Result.Paths[0].Location.BasicBlock, "latch");
      EXPECT_EQ(Result.Paths[0].Location.Opcode, "br");
    } else {
      EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
      EXPECT_EQ(Result.Paths[0].Status, PathStatus::Completed);
      ASSERT_TRUE(Result.Paths[0].ReturnValue.has_value());
      EXPECT_EQ(Result.Paths[0].ReturnValue->bitVector()->constantValue(), 3);
    }
  }
}

TEST(FormalExecutor, ZeroTripNeedsNoBackedgeBudget) {
  static constexpr char IR[] = R"(
    define i32 @entry() {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %next, %latch ]
      %more = icmp ult i32 %i, 0
      br i1 %more, label %latch, label %done
    latch:
      %next = add i32 %i, 1
      br label %header
    done:
      ret i32 %i
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 0;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
}

TEST(FormalExecutor, EarlyBreakAndContinueCountOnlyLatchEdges) {
  static constexpr char IR[] = R"(
    define i32 @entry() {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %next, %latch ]
      %more = icmp ult i32 %i, 4
      br i1 %more, label %body, label %normal_exit
    body:
      %break = icmp eq i32 %i, 1
      br i1 %break, label %early_exit, label %continue
    continue:
      br label %latch
    latch:
      %next = add i32 %i, 1
      br label %header
    early_exit:
      ret i32 %i
    normal_exit:
      ret i32 %i
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 1;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
  EXPECT_EQ(Result.Paths[0].Location.BasicBlock, "early_exit");
}

TEST(FormalExecutor, MultipleLoopExitsKeepBothFeasiblePaths) {
  static constexpr char IR[] = R"(
    define void @entry(i1 %leave) {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %next, %latch ]
      %more = icmp ult i32 %i, 1
      br i1 %more, label %body, label %normal_exit
    body:
      br i1 %leave, label %early_exit, label %latch
    latch:
      %next = add i32 %i, 1
      br label %header
    early_exit:
      ret void
    normal_exit:
      ret void
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 1;
  ScalarExecutor Executor(Expressions, Solver, Options);
  llvm::Function *Entry = Module->getFunction("entry");
  ExecutionResult Result = Executor.execute(
      *Entry, {{Entry->getArg(0), SymbolicValue(Expressions.symbol(1, "leave"))}});
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
  ASSERT_EQ(Result.Paths.size(), 2U);
  EXPECT_EQ(Result.Statistics.CompletedPaths, 2U);
  EXPECT_NE(Result.Paths[0].Location.BasicBlock,
            Result.Paths[1].Location.BasicBlock);
}

TEST(FormalExecutor, StateCapKeepsFeasibleSiblingIncomplete) {
  static constexpr char IR[] = R"(
    define void @entry(i1 %condition) {
    start:
      br i1 %condition, label %yes, label %no
    yes:
      ret void
    no:
      ret void
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({queryResult(SolverStatus::Sat,
                                     SolverReason::Satisfiable, ""),
                         queryResult(SolverStatus::Sat,
                                     SolverReason::Satisfiable, "")});
  ExprContext Expressions;
  ExecutorOptions Options;
  Options.MaxStates = 1;
  ScalarExecutor Executor(Expressions, Solver, Options);
  llvm::Function *Entry = Module->getFunction("entry");
  ExecutionResult Result = Executor.execute(
      *Entry, {{Entry->getArg(0), SymbolicValue(Expressions.symbol(1, "c"))}});
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Statistics.CompletedPaths, 1U);
  EXPECT_EQ(Result.Statistics.IncompletePaths, 1U);
  ASSERT_EQ(Result.Paths.size(), 2U);
  EXPECT_EQ(Result.Paths[1].IncompleteReason,
            IncompletenessReason::StateLimit);
}

TEST(FormalExecutor, RunWideTimeLimitPreventsCompletion) {
  static constexpr char IR[] = R"(
    define void @entry(i1 %condition) {
    start:
      br i1 %condition, label %yes, label %no
    yes:
      ret void
    no:
      ret void
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({queryResult(SolverStatus::Sat, SolverReason::None, ""),
                         queryResult(SolverStatus::Unsat,
                                     SolverReason::Unsatisfiable, "")},
                        std::chrono::milliseconds(3));
  ExprContext Expressions;
  ExecutorOptions Options;
  Options.MaxWallTimeMs = 1;
  ScalarExecutor Executor(Expressions, Solver, Options);
  llvm::Function *Entry = Module->getFunction("entry");
  ExecutionResult Result = Executor.execute(
      *Entry, {{Entry->getArg(0), SymbolicValue(Expressions.symbol(1, "c"))}});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::WallTimeLimit);
}

TEST(FormalExecutor, SolverQueryCapPreventsSafe) {
  static constexpr char IR[] = R"(
    define void @entry(i1 %condition) {
    start:
      br i1 %condition, label %yes, label %no
    yes:
      ret void
    no:
      ret void
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({});
  ExprContext Expressions;
  ExecutorOptions Options;
  Options.MaxSolverQueries = 0;
  ScalarExecutor Executor(Expressions, Solver, Options);
  llvm::Function *Entry = Module->getFunction("entry");
  ExecutionResult Result = Executor.execute(
      *Entry, {{Entry->getArg(0), SymbolicValue(Expressions.symbol(1, "c"))}});
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Statistics.SolverQueries, 0U);
  EXPECT_EQ(Solver.QueryCount, 0U);
  ASSERT_EQ(Result.Paths.size(), 2U);
  for (const PathResult &Path : Result.Paths)
    EXPECT_EQ(Path.IncompleteReason,
              IncompletenessReason::SolverQueryLimit);
}

TEST(FormalExecutor, SolverQueryCapAlsoCoversErrorWitness) {
  llvm::LLVMContext Context;
  auto Module = parseModule(Context,
                            "define void @entry() { start: unreachable }");
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({});
  ExprContext Expressions;
  ExecutorOptions Options;
  Options.EnableRegistration = true;
  Options.MaxSolverQueries = 0;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::SolverQueryLimit);
  EXPECT_EQ(Solver.QueryCount, 0U);
}

TEST(FormalExecutor, LocalLoopMetadataOverridesDefaultBound) {
  static constexpr char IR[] = R"(
    define i32 @entry() {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %next, %latch ]
      %continue = icmp ult i32 %i, 3
      br i1 %continue, label %latch, label %done
    latch:
      %next = add i32 %i, 1
      br label %header, !llvm.loop !0
    done:
      ret i32 %i
    }
    !0 = distinct !{!0, !1}
    !1 = !{!"llvm.loop.unroll.count", i32 3}
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 2;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
}

TEST(FormalExecutor, NestedLoopActivationsResetOnReentry) {
  static constexpr char IR[] = R"(
    define i32 @entry() {
    start:
      br label %outer
    outer:
      %o = phi i32 [ 0, %start ], [ %onext, %outer_latch ]
      %oc = icmp ult i32 %o, 2
      br i1 %oc, label %inner, label %done
    inner:
      %i = phi i32 [ 0, %outer ], [ %inext, %inner_latch ]
      %ic = icmp ult i32 %i, 2
      br i1 %ic, label %inner_latch, label %outer_latch
    inner_latch:
      %inext = add i32 %i, 1
      br label %inner
    outer_latch:
      %onext = add i32 %o, 1
      br label %outer
    done:
      ret i32 %o
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 2;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
}

TEST(FormalExecutor, MultipleLatchesShareOneActivationCounter) {
  static constexpr char IR[] = R"(
    define i32 @entry() {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %left_next, %left ], [ %right_next, %right ]
      %more = icmp ult i32 %i, 2
      br i1 %more, label %choose, label %done
    choose:
      %even = icmp eq i32 %i, 0
      br i1 %even, label %left, label %right
    left:
      %left_next = add i32 %i, 1
      br label %header
    right:
      %right_next = add i32 %i, 1
      br label %header
    done:
      ret i32 %i
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  for (unsigned Bound : {1U, 2U}) {
    ExprContext Expressions;
    STPSolver Solver;
    ExecutorOptions Options;
    Options.DefaultLoopBound = Bound;
    ScalarExecutor Executor(Expressions, Solver, Options);
    ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    EXPECT_EQ(Result.Status, Bound == 1 ? ExecutionStatus::Unknown
                                       : ExecutionStatus::Complete);
    if (Bound == 1) {
      EXPECT_EQ(Result.Paths[0].IncompleteReason,
                IncompletenessReason::LoopBound);
    }
  }
}

TEST(FormalExecutor, ReenteredCalleeHasFreshLoopCounter) {
  static constexpr char IR[] = R"(
    define i32 @callee() {
    start:
      br label %header
    header:
      %i = phi i32 [ 0, %start ], [ %next, %latch ]
      %more = icmp ult i32 %i, 2
      br i1 %more, label %latch, label %done
    latch:
      %next = add i32 %i, 1
      br label %header
    done:
      ret i32 %i
    }
    define i32 @entry() {
    start:
      %a = call i32 @callee()
      %b = call i32 @callee()
      %sum = add i32 %a, %b
      ret i32 %sum
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 2;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
}

TEST(FormalExecutor, IrreducibleCycleCannotReturnSafe) {
  static constexpr char IR[] = R"(
    define void @entry() {
    start:
      br i1 true, label %left, label %right
    left:
      br label %right
    right:
      br label %left
    }
  )";
  llvm::LLVMContext Context;
  auto Module = parseModule(Context, IR);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ExecutorOptions Options;
  Options.DefaultLoopBound = 2;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(*Module->getFunction("entry"), {});
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  ASSERT_FALSE(Result.Paths.empty());
  for (const PathResult &Path : Result.Paths)
    EXPECT_EQ(Path.IncompleteReason, IncompletenessReason::IrreducibleCycle);
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

TEST(FormalExecutor, AssumeBackendUnknownKeepsThePathIncomplete) {
  static constexpr char IR[] = R"(
    declare void @__formal_assume(i32)
    define void @entry(i32 %condition) {
    start:
      call void @__formal_assume(i32 %condition)
      ret void
    }
  )";
  llvm::LLVMContext LLVMContext;
  std::unique_ptr<llvm::Module> Module = parseModule(LLVMContext, IR);
  ASSERT_NE(Module, nullptr);
  ScriptedSolver Solver({queryResult(SolverStatus::Unknown,
                                     SolverReason::BackendUnknown, "unknown")});
  ExprContext Expressions;
  llvm::Function *Entry = Module->getFunction("entry");
  ExecutorOptions Options;
  Options.EnableRegistration = true;
  ScalarExecutor Executor(Expressions, Solver, Options);
  ExecutionResult Result = Executor.execute(
      *Entry,
      {{Entry->getArg(0), SymbolicValue(Expressions.symbol(32, "condition"))}});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].Status, PathStatus::Incomplete);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::SolverUnknown);
  EXPECT_EQ(Result.Statistics.AssumptionPrunedPaths, 0U);
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
