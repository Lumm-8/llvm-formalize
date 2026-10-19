#include "formal/Memory.h"
#include "formal/Executor.h"

#include "llvm/AsmParser/Parser.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"

#include "gtest/gtest.h"

#include <array>
#include <cstdint>
#include <fstream>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <random>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <utility>

using namespace formal;

namespace {

std::shared_ptr<MemoryObject> object(std::uint64_t Id, std::uint64_t Size,
                                     bool ReadOnly = false) {
  auto Result = std::make_shared<MemoryObject>();
  Result->Id = Id;
  Result->Size = Size;
  Result->Alignment = 8;
  Result->LogicalBase = 0x10000 + Id * 0x100;
  Result->ReadOnly = ReadOnly;
  return Result;
}

class ScriptedSolver final : public Solver {
public:
  explicit ScriptedSolver(std::vector<SolverStatus> Script,
                          SolverStatus CheckStatus = SolverStatus::InternalError)
      : Script(std::move(Script)), CheckStatus(CheckStatus) {}
  SolverResult check(const ConstraintSet &) override {
    SolverResult Result;
    Result.Status = CheckStatus;
    return Result;
  }
  SolverResult mayBeTrue(const ConstraintSet &, const ExprRef &) override {
    if (Next >= Script.size())
      return unexpected();
    SolverResult Result;
    Result.Status = Script[Next++];
    if (Result.Status == SolverStatus::Sat)
      Result.Truth = true;
    else if (Result.Status == SolverStatus::Unsat)
      Result.Truth = false;
    return Result;
  }
  SolverResult mustBeTrue(const ConstraintSet &, const ExprRef &) override {
    return unexpected();
  }
  SolverResult getValue(const ConstraintSet &, const ExprRef &) override {
    return unexpected();
  }
  SolverResult getInitialValues(
      const ConstraintSet &,
      const std::vector<ArrayModelRequest> &) override {
    return unexpected();
  }

private:
  SolverResult unexpected() {
    SolverResult Result;
    Result.Status = SolverStatus::InternalError;
    return Result;
  }
  std::vector<SolverStatus> Script;
  SolverStatus CheckStatus;
  std::size_t Next = 0;
};

std::unique_ptr<llvm::Module> pointerSelectModule(llvm::LLVMContext &Context) {
  llvm::SMDiagnostic Diagnostic;
  return llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    define i8 @run(i1 %choose) {
    entry:
      %a = alloca i8, align 1
      %b = alloca i8, align 1
      store i8 7, ptr %a, align 1
      store i8 9, ptr %b, align 1
      %p = select i1 %choose, ptr %a, ptr %b
      %v = load i8, ptr %p, align 1
      ret i8 %v
    }
  )", Diagnostic, Context);
}

std::string decimal(const llvm::APInt &Value) {
  llvm::SmallString<64> Buffer;
  Value.toString(Buffer, 10, false);
  return Buffer.str().str();
}

bool replayNativeMemoryError(const char *Mode, std::int64_t Input,
                             const char *ExpectedDiagnostic,
                             std::string &Diagnostic) {
  int Pipe[2];
  if (::pipe(Pipe) != 0)
    return false;
  const std::string Text = std::to_string(Input);
  const pid_t Child = ::fork();
  if (Child < 0) {
    ::close(Pipe[0]);
    ::close(Pipe[1]);
    return false;
  }
  if (Child == 0) {
    ::close(Pipe[0]);
    ::dup2(Pipe[1], STDERR_FILENO);
    ::close(Pipe[1]);
    ::execl(FORMAL_M4_REPLAY_BINARY, FORMAL_M4_REPLAY_BINARY,
            Mode, Text.c_str(), static_cast<char *>(nullptr));
    ::_exit(127);
  }
  ::close(Pipe[1]);
  char Buffer[4096];
  ssize_t Read = 0;
  while ((Read = ::read(Pipe[0], Buffer, sizeof(Buffer))) > 0)
    Diagnostic.append(Buffer, static_cast<std::size_t>(Read));
  ::close(Pipe[0]);
  int Status = 0;
  if (::waitpid(Child, &Status, 0) != Child)
    return false;
  const bool Failed = WIFSIGNALED(Status) ||
                      (WIFEXITED(Status) && WEXITSTATUS(Status) != 0);
  return Failed &&
         (!ExpectedDiagnostic ||
          Diagnostic.find(ExpectedDiagnostic) != std::string::npos);
}

bool replayNativeSymbolicOOB(std::int64_t Index, std::string &Diagnostic) {
  const bool Failed =
      replayNativeMemoryError("symbolic_oob", Index, nullptr, Diagnostic);
  return Failed &&
         (Diagnostic.find("stack-buffer-overflow") != std::string::npos ||
          Diagnostic.find("out of bounds") != std::string::npos);
}

std::optional<llvm::APInt> executeConstantIR(const std::string &IR) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(IR, Diagnostic, Context);
  if (!Module)
    return std::nullopt;
  ExprContext Expressions;
  ScriptedSolver Solver({});
  ScalarExecutor Executor(Expressions, Solver);
  auto Result = Executor.execute(*Module->getFunction("sample"), {});
  if (Result.Status != ExecutionStatus::Complete || Result.Paths.size() != 1 ||
      !Result.Paths[0].ReturnValue ||
      !Result.Paths[0].ReturnValue->bitVector()->isConstant())
    return std::nullopt;
  return Result.Paths[0].ReturnValue->bitVector()->constantValue();
}

TEST(FormalMemory, MergeableConstantGlobalAddressesAreNotFixedDistinct) {
  const auto Execute = [](const std::string &Qualifier) {
    llvm::LLVMContext Context;
    llvm::SMDiagnostic Diagnostic;
    auto Module = llvm::parseAssemblyString(
        "target datalayout = \"e-p:64:64-i64:64-n8:16:32:64-S128\"\n"
        "@a = " + Qualifier + " constant i8 7\n"
        "@b = constant i8 7\n"
        "define i1 @sample() {\n"
        "  %same = icmp eq ptr @a, @b\n"
        "  ret i1 %same\n"
        "}\n", Diagnostic, Context);
    EXPECT_NE(Module, nullptr);
    ExprContext Expressions;
    ScriptedSolver Solver({});
    ScalarExecutor Executor(Expressions, Solver);
    return Executor.execute(*Module->getFunction("sample"), {});
  };
  for (const char *Qualifier : {"unnamed_addr", "local_unnamed_addr"}) {
    auto Result = Execute(Qualifier);
    ASSERT_EQ(Result.Paths.size(), 1U);
    EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
    EXPECT_EQ(Result.Paths[0].IncompleteReason,
              IncompletenessReason::PointerComparisonIncomplete);
  }
  auto Significant = Execute("");
  ASSERT_EQ(Significant.Paths.size(), 1U);
  ASSERT_EQ(Significant.Status, ExecutionStatus::Complete);
  ASSERT_TRUE(Significant.Paths[0].ReturnValue.has_value());
  EXPECT_EQ(Significant.Paths[0].ReturnValue->bitVector()->constantValue(),
            llvm::APInt(1, 0));
}

TEST(FormalMemory, ExternallyInitializedGlobalDoesNotUseIRInitializer) {
  const std::string Prefix = R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    @g = externally_initialized global i8 0
  )";
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(
      Prefix + R"(
        define i8 @sample() {
        entry:
          %value = load i8, ptr @g, align 1
          ret i8 %value
        }
      )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  ScriptedSolver Solver({});
  ScalarExecutor Executor(Expressions, Solver);
  auto Unknown = Executor.execute(*Module->getFunction("sample"), {});
  ASSERT_EQ(Unknown.Paths.size(), 1U);
  EXPECT_EQ(Unknown.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Unknown.Paths[0].IncompleteReason,
            IncompletenessReason::UnsupportedGlobal);

  auto Overwritten = executeConstantIR(Prefix + R"(
    define i8 @sample() {
    entry:
      store i8 13, ptr @g, align 1
      %value = load i8, ptr @g, align 1
      ret i8 %value
    }
  )");
  ASSERT_TRUE(Overwritten.has_value());
  EXPECT_EQ(*Overwritten, llvm::APInt(8, 13));
}

TEST(FormalMemory, MemoryErrorRequiresValidatedWitness) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    define i8 @sample() {
    entry:
      %value = load i8, ptr null, align 1
      ret i8 %value
    }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  for (SolverStatus Status : {SolverStatus::Sat, SolverStatus::Unsat,
                              SolverStatus::InternalError,
                              SolverStatus::Timeout, SolverStatus::Unknown}) {
    ExprContext Expressions;
    ScriptedSolver Solver({}, Status);
    ScalarExecutor Executor(Expressions, Solver);
    auto Result = Executor.execute(*Module->getFunction("sample"), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    EXPECT_NE(Result.Paths[0].Status, PathStatus::Error);
    EXPECT_FALSE(Result.Paths[0].ErrorModel.has_value());
    if (Status == SolverStatus::Timeout || Status == SolverStatus::Unknown)
      EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
    else
      EXPECT_EQ(Result.Status, ExecutionStatus::InternalError);
  }
}

TEST(FormalMemory, NullPointerIsValidDoesNotInventNullDereferences) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

    define i8 @valid_load() #0 {
    entry:
      %value = load i8, ptr null, align 1
      ret i8 %value
    }
    define void @valid_store() #0 {
    entry:
      store i8 1, ptr null, align 1
      ret void
    }
    define void @valid_memcpy_dest() #0 {
    entry:
      %source = alloca i8, align 1
      store i8 1, ptr %source, align 1
      call void @llvm.memcpy.p0.p0.i64(ptr null, ptr %source, i64 1, i1 false)
      ret void
    }
    define void @valid_memcpy_source() #0 {
    entry:
      %dest = alloca i8, align 1
      call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr null, i64 1, i1 false)
      ret void
    }
    define void @valid_zero_length_memcpy() #0 {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr null, ptr null, i64 0, i1 false)
      ret void
    }
    define i8 @ordinary_load() {
    entry:
      %value = load i8, ptr null, align 1
      ret i8 %value
    }
    define void @ordinary_store() {
    entry:
      store i8 1, ptr null, align 1
      ret void
    }
    define void @ordinary_memcpy() {
    entry:
      %source = alloca i8, align 1
      store i8 1, ptr %source, align 1
      call void @llvm.memcpy.p0.p0.i64(ptr null, ptr %source, i64 1, i1 false)
      ret void
    }
    attributes #0 = { null_pointer_is_valid }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  for (const char *Name : {"valid_load", "valid_store", "valid_memcpy_dest",
                           "valid_memcpy_source"}) {
    SCOPED_TRACE(Name);
    ExprContext Expressions;
    ScriptedSolver Solver({});
    ScalarExecutor Executor(Expressions, Solver);
    auto Result = Executor.execute(*Module->getFunction(Name), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
    EXPECT_EQ(Result.Paths[0].Status, PathStatus::Incomplete);
    EXPECT_EQ(Result.Paths[0].IncompleteReason,
              IncompletenessReason::PointerProvenanceLost);
    EXPECT_FALSE(Result.Paths[0].ErrorModel.has_value());
  }
  {
    ExprContext Expressions;
    ScriptedSolver Solver({});
    ScalarExecutor Executor(Expressions, Solver);
    auto Result = Executor.execute(
        *Module->getFunction("valid_zero_length_memcpy"), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
  }
  for (const auto &[Name, NativeMode] :
       std::array<std::pair<const char *, const char *>, 3>{{
           {"ordinary_load", "null"},
           {"ordinary_store", "null_store"},
           {"ordinary_memcpy", "null_memcpy"},
       }}) {
    SCOPED_TRACE(Name);
    ExprContext Expressions;
    STPSolver Solver;
    ScalarExecutor Executor(Expressions, Solver);
    auto Result = Executor.execute(*Module->getFunction(Name), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    EXPECT_EQ(Result.Status, ExecutionStatus::Error);
    EXPECT_EQ(Result.Paths[0].Status, PathStatus::Error);
    EXPECT_EQ(Result.Paths[0].ErrorReason,
              PathErrorReason::NullDereference);
    ASSERT_TRUE(Result.Paths[0].ErrorModel.has_value());
    std::string ValidationError;
    EXPECT_TRUE(validateModel(Result.Paths[0].Constraints,
                              *Result.Paths[0].ErrorModel,
                              ValidationError)) << ValidationError;
    std::string NativeDiagnostic;
    EXPECT_TRUE(replayNativeMemoryError(NativeMode, 0, "null pointer",
                                        NativeDiagnostic)) << NativeDiagnostic;
  }
}

TEST(FormalMemory, MemcpyCallParameterAttributesGateZeroLength) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

    define void @bad_dest() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull null, ptr null, i64 0, i1 false)
      ret void
    }
    define void @bad_source() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr null, ptr noundef nonnull null, i64 0, i1 false)
      ret void
    }
    define void @nonnull_only() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr nonnull null, ptr null, i64 0, i1 false)
      ret void
    }
    define void @dereferenceable() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr noundef dereferenceable(1) null, ptr null, i64 0, i1 false)
      ret void
    }
    define void @noundef_only() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr noundef null, ptr null, i64 0, i1 false)
      ret void
    }
    define void @noundef_undef() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr noundef undef, ptr null, i64 0, i1 false)
      ret void
    }
    define void @noundef_poison() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr noundef poison, ptr null, i64 0, i1 false)
      ret void
    }
    define void @aligned_null() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr align 8 null, ptr align 8 null, i64 0, i1 false)
      ret void
    }
    define void @misaligned_pointer() {
    entry:
      %offset = getelementptr i8, ptr null, i64 1
      call void @llvm.memcpy.p0.p0.i64(ptr noundef align 8 %offset, ptr null, i64 0, i1 false)
      ret void
    }
    define void @unannotated() {
    entry:
      call void @llvm.memcpy.p0.p0.i64(ptr null, ptr null, i64 0, i1 false)
      ret void
    }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  struct Case {
    const char *Name;
    IncompletenessReason Reason;
  };
  constexpr std::array<Case, 10> Cases = {{
      {"bad_dest", IncompletenessReason::PoisonGeneratingAnnotation},
      {"bad_source", IncompletenessReason::PoisonGeneratingAnnotation},
      {"nonnull_only", IncompletenessReason::PoisonGeneratingAnnotation},
      {"dereferenceable", IncompletenessReason::PoisonGeneratingAnnotation},
      {"noundef_only", IncompletenessReason::None},
      {"noundef_undef", IncompletenessReason::UndefValue},
      {"noundef_poison", IncompletenessReason::PoisonValue},
      {"aligned_null", IncompletenessReason::None},
      {"misaligned_pointer", IncompletenessReason::PoisonGeneratingAnnotation},
      {"unannotated", IncompletenessReason::None},
  }};
  for (const Case &TestCase : Cases) {
    SCOPED_TRACE(TestCase.Name);
    ExprContext Expressions;
    ScriptedSolver Solver({});
    ScalarExecutor Executor(Expressions, Solver);
    auto Result = Executor.execute(*Module->getFunction(TestCase.Name), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    if (TestCase.Reason != IncompletenessReason::None) {
      EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
      EXPECT_EQ(Result.Paths[0].Status, PathStatus::Incomplete);
      EXPECT_EQ(Result.Paths[0].IncompleteReason, TestCase.Reason);
    } else {
      EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
      EXPECT_EQ(Result.Paths[0].Status, PathStatus::Completed);
    }
  }
}

TEST(FormalMemory, EveryExecutableMemoryErrorHasReplayableModel) {
  struct Case {
    const char *Name;
    const char *Body;
    std::int64_t Trigger;
    PathErrorReason Reason;
    const char *NativeMode;
    const char *Diagnostic;
  };
  const std::array<Case, 4> Cases = {{
      {"null", "%value = load i8, ptr null, align 1\n"
               "  ret i8 %value", 0, PathErrorReason::NullDereference,
       "null", "null pointer"},
      {"bounds", "%array = alloca [1 x i8], align 1\n"
                 "  %at = getelementptr [1 x i8], ptr %array, i64 0, i64 %input\n"
                 "  %value = load i8, ptr %at, align 1\n"
                 "  ret i8 %value", 1, PathErrorReason::OutOfBounds,
       "oob", "out of bounds"},
      {"alignment", "%array = alloca [8 x i8], align 4\n"
                    "  %at = getelementptr i8, ptr %array, i64 %input\n"
                    "  %value = load i32, ptr %at, align 4\n"
                    "  %result = trunc i32 %value to i8\n"
                    "  ret i8 %result", 1, PathErrorReason::MisalignedAccess,
       "misaligned", "misaligned address"},
      {"readonly", "store i8 2, ptr @readonly_replay, align 1\n"
                   "  ret i8 0", 2, PathErrorReason::ReadOnlyWrite,
       "readonly", "WRITE memory access"},
  }};
  for (const Case &TestCase : Cases) {
    SCOPED_TRACE(TestCase.Name);
    llvm::LLVMContext Context;
    llvm::SMDiagnostic ParseError;
    std::string IR =
        "target datalayout = \"e-p:64:64-i64:64-n8:16:32:64-S128\"\n"
        "@readonly_replay = constant i8 42\n"
        "define i8 @run(i64 %input) {\n"
        "entry:\n"
        "  %trigger = icmp eq i64 %input, " +
        std::to_string(TestCase.Trigger) + "\n"
        "  br i1 %trigger, label %bad, label %safe\n"
        "bad:\n  " + TestCase.Body + "\n"
        "safe:\n  ret i8 0\n}\n";
    auto Module = llvm::parseAssemblyString(IR, ParseError, Context);
    ASSERT_NE(Module, nullptr) << IR;
    ExprContext Expressions;
    STPSolver Solver;
    ScalarExecutor Executor(Expressions, Solver);
    llvm::Function *Function = Module->getFunction("run");
    ExprRef Input = Expressions.symbol(64, "replay_input");
    auto Result = Executor.execute(
        *Function, {{Function->getArg(0), SymbolicValue(Input)}});
    ASSERT_EQ(Result.Status, ExecutionStatus::Error);
    bool FoundError = false;
    for (const PathResult &Path : Result.Paths) {
      if (Path.Status != PathStatus::Error)
        continue;
      FoundError = true;
      ASSERT_EQ(Path.ErrorReason, TestCase.Reason);
      ASSERT_TRUE(Path.ErrorModel.has_value());
      std::string EvaluationError;
      ASSERT_TRUE(validateModel(Path.Constraints, *Path.ErrorModel,
                                EvaluationError))
          << EvaluationError;
      auto Value = evaluate(Input, *Path.ErrorModel, EvaluationError);
      ASSERT_TRUE(Value.has_value()) << EvaluationError;
      ASSERT_EQ(Value->getSExtValue(), TestCase.Trigger);
      std::string NativeDiagnostic;
      EXPECT_TRUE(replayNativeMemoryError(
          TestCase.NativeMode, Value->getSExtValue(),
          TestCase.Diagnostic, NativeDiagnostic))
          << NativeDiagnostic;
    }
    EXPECT_TRUE(FoundError);
  }
}

TEST(FormalMemory, EveryReportedM4IRMemoryErrorModelIsNativelyReplayed) {
  struct ReplayCase {
    const char *File;
    const char *Entry;
    PathErrorReason Reason;
    const char *NativeMode;
    const char *Diagnostic;
    std::int64_t FixedInput;
  };
  // Keep this ledger in lockstep with every M4 lit expectation that reports a
  // memory error. The discovery check below fails when a new one lacks replay.
  constexpr std::array<ReplayCase, 12> Cases = {{
      {"basic.ll", "null_access", PathErrorReason::NullDereference,
       "null", "null pointer", 0},
      {"basic.ll", "one_past_access", PathErrorReason::OutOfBounds,
       "oob", "out of bounds", 1},
      {"basic.ll", "readonly_write", PathErrorReason::ReadOnlyWrite,
       "readonly", "WRITE memory access", 0},
      {"null-pointer-valid.ll", "ordinary_load",
       PathErrorReason::NullDereference, "null", "null pointer", 0},
      {"scalars.ll", "negative_bad", PathErrorReason::OutOfBounds,
       "oob", "out of bounds", -1},
      {"scalars.ll", "zero_size", PathErrorReason::OutOfBounds,
       "zero_size", "insufficient space", 0},
      {"scalars.ll", "misaligned", PathErrorReason::MisalignedAccess,
       "misaligned_i16", "misaligned address", 1},
      {"scalars.ll", "null_roundtrip", PathErrorReason::NullDereference,
       "null", "null pointer", 0},
      {"scalars.ll", "crosses_end", PathErrorReason::OutOfBounds,
       "crosses_end", "AddressSanitizer", 0},
      {"scalars.ll", "offset_plus_size_overflow",
       PathErrorReason::OutOfBounds, "offset_plus_size_overflow",
       "AddressSanitizer: SEGV", INT64_MAX},
      {"symbolic.ll", "symbolic_index", PathErrorReason::OutOfBounds,
       "symbolic_oob_load", "out of bounds", 0},
      {"symbolic.ll", "nullable_select",
       PathErrorReason::NullDereference, "nullable_select", "null pointer", 0},
  }};

  using EntryKey = std::pair<std::string, std::string>;
  std::set<EntryKey> Covered;
  for (const ReplayCase &TestCase : Cases)
    ASSERT_TRUE(Covered.emplace(TestCase.File, TestCase.Entry).second);

  const std::filesystem::path Directory(FORMAL_M4_IR_DIRECTORY);
  const std::regex Run(
      R"(^; RUN: %scalar-executor %s --m4 --entry ([A-Za-z_][A-Za-z_0-9]*).*--check-prefix=([A-Za-z_0-9-]+).*$)");
  const std::regex ErrorReason(
      R"(^; ([A-Za-z_0-9-]+): reason=(NULL_DEREFERENCE|OUT_OF_BOUNDS|MISALIGNED_ACCESS|READ_ONLY_WRITE|DEAD_OBJECT_ACCESS).*$)");
  const std::regex ErrorStatus(
      R"(^; ([A-Za-z_0-9-]+): M4 status ERROR.*$)");
  std::set<EntryKey> Reported;
  for (const auto &File : std::filesystem::directory_iterator(Directory)) {
    if (File.path().extension() != ".ll")
      continue;
    std::ifstream Source(File.path());
    ASSERT_TRUE(Source.is_open()) << File.path();
    std::map<std::string, std::string> EntryForPrefix;
    std::string Line;
    std::smatch Match;
    while (std::getline(Source, Line)) {
      if (std::regex_match(Line, Match, Run))
        EntryForPrefix[Match[2].str()] = Match[1].str();
      if (std::regex_match(Line, Match, ErrorReason) ||
          std::regex_match(Line, Match, ErrorStatus)) {
        auto It = EntryForPrefix.find(Match[1].str());
        ASSERT_NE(It, EntryForPrefix.end()) << File.path() << ':' << Line;
        Reported.emplace(File.path().filename().string(), It->second);
      }
    }
  }
  for (const EntryKey &Entry : Reported)
    EXPECT_TRUE(Covered.count(Entry)) << Entry.first << ':' << Entry.second;
  for (const EntryKey &Entry : Covered)
    EXPECT_TRUE(Reported.count(Entry)) << Entry.first << ':' << Entry.second;
  ASSERT_EQ(Reported.size(), Covered.size());

  std::size_t ReplayedPaths = 0;
  for (const ReplayCase &TestCase : Cases) {
    SCOPED_TRACE(std::string(TestCase.File) + ':' + TestCase.Entry);
    llvm::LLVMContext Context;
    llvm::SMDiagnostic ParseError;
    auto Module = llvm::parseAssemblyFile(
        (Directory / TestCase.File).string(), ParseError, Context);
    ASSERT_NE(Module, nullptr);
    llvm::Function *Function = Module->getFunction(TestCase.Entry);
    ASSERT_NE(Function, nullptr);
    ExprContext Expressions;
    STPSolver Solver;
    ScalarExecutor Executor(Expressions, Solver);
    std::vector<ArgumentBinding> Bindings;
    ExprRef Input;
    for (llvm::Argument &Argument : Function->args()) {
      ASSERT_TRUE(Argument.getType()->isIntegerTy());
      ASSERT_FALSE(Input) << "replay fixture has more than one input";
      Input = Expressions.symbol(Argument.getType()->getIntegerBitWidth(),
                                 std::string(TestCase.Entry) + "_input");
      Bindings.push_back({&Argument, SymbolicValue(Input)});
    }
    auto Result = Executor.execute(*Function, Bindings);
    ASSERT_EQ(Result.Status, ExecutionStatus::Error);
    std::size_t EntryErrors = 0;
    for (const PathResult &Path : Result.Paths) {
      if (Path.Status != PathStatus::Error)
        continue;
      ++EntryErrors;
      ++ReplayedPaths;
      ASSERT_EQ(Path.ErrorReason, TestCase.Reason);
      ASSERT_TRUE(Path.ErrorModel.has_value());
      std::string ModelError;
      ASSERT_TRUE(validateModel(Path.Constraints, *Path.ErrorModel,
                                ModelError)) << ModelError;
      std::int64_t Concrete = TestCase.FixedInput;
      if (Input) {
        auto Value = evaluate(Input, *Path.ErrorModel, ModelError);
        ASSERT_TRUE(Value.has_value()) << ModelError;
        Concrete = Value->getSExtValue();
      }
      std::string NativeDiagnostic;
      EXPECT_TRUE(replayNativeMemoryError(
          TestCase.NativeMode, Concrete, TestCase.Diagnostic,
          NativeDiagnostic)) << "witness=" << Concrete << '\n'
                             << NativeDiagnostic;
      if (std::string(TestCase.NativeMode) == "offset_plus_size_overflow") {
        EXPECT_NE(NativeDiagnostic.find("READ memory access"),
                  std::string::npos) << NativeDiagnostic;
        EXPECT_EQ(NativeDiagnostic.find("misaligned address"),
                  std::string::npos) << NativeDiagnostic;
      }
    }
    EXPECT_GT(EntryErrors, 0U);
  }
  EXPECT_GE(ReplayedPaths, Cases.size());
}

TEST(FormalMemory, FixedSeedIRRoundTripAndOverlappingWriteReference) {
  constexpr std::uint64_t Seed = 0x4d340002U;
  std::filesystem::create_directories("artifacts");
  std::mt19937_64 Random(Seed);
  constexpr unsigned Widths[] = {1, 8, 16, 24, 32, 64, 65, 128};
  for (unsigned Iteration = 0; Iteration < 128; ++Iteration) {
    const bool Little = (Iteration & 1U) == 0;
    const unsigned Width = Widths[Iteration % 8];
    llvm::APInt Value(128, Random());
    Value |= llvm::APInt(128, Random()).shl(64);
    Value = Value.trunc(Width);
    std::ostringstream IR;
    IR << "target datalayout = \"" << (Little ? 'e' : 'E')
       << "-p:64:64-i64:64-n8:16:32:64-S128\"\n"
       << "define i" << Width << " @sample() {\n"
       << "entry:\n"
       << "  %slot = alloca i" << Width << ", align 1\n"
       << "  store i" << Width << ' ' << decimal(Value)
       << ", ptr %slot, align 1\n"
       << "  %result = load i" << Width
       << ", ptr %slot, align 1\n"
       << "  ret i" << Width << " %result\n}\n";
    auto Actual = executeConstantIR(IR.str());
    if (!Actual || *Actual != Value) {
      std::ofstream Failure("artifacts/m4-property-failure.ll");
      Failure << "; seed=" << Seed << " iteration=" << Iteration << '\n'
              << IR.str();
    }
    ASSERT_TRUE(Actual.has_value()) << "seed=" << Seed
                                    << " iteration=" << Iteration;
    EXPECT_EQ(*Actual, Value) << "seed=" << Seed
                              << " iteration=" << Iteration;

    std::uint32_t Base = static_cast<std::uint32_t>(Random());
    std::uint16_t Overlay = static_cast<std::uint16_t>(Random());
    std::array<std::uint8_t, 4> Bytes{};
    for (unsigned Byte = 0; Byte < 4; ++Byte) {
      unsigned Shift = (Little ? Byte : 3 - Byte) * 8;
      Bytes[Byte] = static_cast<std::uint8_t>(Base >> Shift);
    }
    Bytes[1] = static_cast<std::uint8_t>(
        Overlay >> (Little ? 0 : 8));
    Bytes[2] = static_cast<std::uint8_t>(
        Overlay >> (Little ? 8 : 0));
    std::uint32_t Expected = 0;
    for (unsigned Byte = 0; Byte < 4; ++Byte) {
      unsigned Shift = (Little ? Byte : 3 - Byte) * 8;
      Expected |= static_cast<std::uint32_t>(Bytes[Byte]) << Shift;
    }
    std::ostringstream OverlapIR;
    OverlapIR << "target datalayout = \"" << (Little ? 'e' : 'E')
              << "-p:64:64-i64:64-n8:16:32:64-S128\"\n"
              << "define i32 @sample() {\nentry:\n"
              << "  %slot = alloca i32, align 1\n"
              << "  store i32 " << Base << ", ptr %slot, align 1\n"
              << "  %middle = getelementptr i8, ptr %slot, i64 1\n"
              << "  store i16 " << Overlay << ", ptr %middle, align 1\n"
              << "  %result = load i32, ptr %slot, align 1\n"
              << "  ret i32 %result\n}\n";
    auto OverlapActual = executeConstantIR(OverlapIR.str());
    if (!OverlapActual || *OverlapActual != llvm::APInt(32, Expected)) {
      std::ofstream Failure("artifacts/m4-property-failure.ll");
      Failure << "; seed=" << Seed << " iteration=" << Iteration
              << " overlap reference=" << Expected << '\n'
              << OverlapIR.str();
    }
    ASSERT_TRUE(OverlapActual.has_value()) << "seed=" << Seed
                                           << " iteration=" << Iteration;
    EXPECT_EQ(*OverlapActual, llvm::APInt(32, Expected));
  }
}

TEST(FormalMemory, FixedSeedStructGEPMatchesDataLayout) {
  constexpr std::uint64_t Seed = 0x4d340003U;
  std::filesystem::create_directories("artifacts");
  std::mt19937_64 Random(Seed);
  constexpr unsigned Widths[] = {8, 16, 24, 32, 64};
  for (unsigned Iteration = 0; Iteration < 80; ++Iteration) {
    unsigned Width = Widths[Random() % 5];
    bool Packed = (Random() & 1U) != 0;
    llvm::LLVMContext Context;
    const llvm::DataLayout Layout("e-p:64:64-i64:64-n8:16:32:64-S128");
    auto *Structure = llvm::StructType::get(
        Context, {llvm::Type::getInt8Ty(Context),
                  llvm::IntegerType::get(Context, Width)},
        Packed);
    std::uint64_t ExpectedOffset =
        Layout.getStructLayout(Structure)->getElementOffset(1);
    unsigned Alignment = Layout.getABITypeAlign(Structure).value();
    std::string Type = Packed ? "<{ i8, i" : "{ i8, i";
    Type += std::to_string(Width);
    Type += Packed ? " }>" : " }";
    std::ostringstream IR;
    IR << "target datalayout = \"e-p:64:64-i64:64-n8:16:32:64-S128\"\n"
       << "define i8 @sample() {\nentry:\n"
       << "  %slot = alloca " << Type << ", align " << Alignment << '\n'
       << "  %field = getelementptr " << Type
       << ", ptr %slot, i64 0, i32 1\n"
       << "  store i8 91, ptr %field, align 1\n"
       << "  %raw = getelementptr i8, ptr %slot, i64 "
       << ExpectedOffset << '\n'
       << "  %value = load i8, ptr %raw, align 1\n"
       << "  ret i8 %value\n}\n";
    auto Actual = executeConstantIR(IR.str());
    if (!Actual || *Actual != llvm::APInt(8, 91)) {
      std::ofstream Failure("artifacts/m4-property-failure.ll");
      Failure << "; seed=" << Seed << " iteration=" << Iteration
              << " datalayout_offset=" << ExpectedOffset << '\n'
              << IR.str();
    }
    ASSERT_TRUE(Actual.has_value()) << "seed=" << Seed
                                    << " iteration=" << Iteration << '\n'
                                    << IR.str();
    EXPECT_EQ(*Actual, llvm::APInt(8, 91));
  }
}

TEST(FormalMemory, CandidateCapIsIncomplete) {
  llvm::LLVMContext Context;
  auto Module = pointerSelectModule(Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  ScriptedSolver Solver({});
  ExecutorOptions Options;
  Options.MaxPointerCandidates = 1;
  ScalarExecutor Executor(Expressions, Solver, Options);
  llvm::Function *Function = Module->getFunction("run");
  auto Result = Executor.execute(
      *Function,
      {{Function->getArg(0),
        SymbolicValue(Expressions.symbol(1, "choose"))}});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  EXPECT_EQ(Result.Paths[0].IncompleteReason,
            IncompletenessReason::PointerResolutionIncomplete);
  EXPECT_EQ(Result.Statistics.PointerCapHits, 1U);
}

TEST(FormalMemory, ResolutionTimeoutNeverPrunesCandidate) {
  llvm::LLVMContext Context;
  auto Module = pointerSelectModule(Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  ScriptedSolver Solver({SolverStatus::Timeout, SolverStatus::Timeout});
  ScalarExecutor Executor(Expressions, Solver);
  llvm::Function *Function = Module->getFunction("run");
  auto Result = Executor.execute(
      *Function,
      {{Function->getArg(0),
        SymbolicValue(Expressions.symbol(1, "choose"))}});
  ASSERT_EQ(Result.Paths.size(), 2U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
  for (const PathResult &Path : Result.Paths)
    EXPECT_EQ(Path.IncompleteReason, IncompletenessReason::SolverTimeout);
  EXPECT_EQ(Result.Statistics.PointerCandidates, 4U);
}

TEST(FormalMemory, OnlySatPointerCandidateContinues) {
  llvm::LLVMContext Context;
  auto Module = pointerSelectModule(Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  ScriptedSolver Solver({SolverStatus::Sat, SolverStatus::Unsat});
  ScalarExecutor Executor(Expressions, Solver);
  llvm::Function *Function = Module->getFunction("run");
  auto Result = Executor.execute(
      *Function,
      {{Function->getArg(0),
        SymbolicValue(Expressions.symbol(1, "choose"))}});
  ASSERT_EQ(Result.Paths.size(), 1U);
  EXPECT_EQ(Result.Status, ExecutionStatus::Complete);
  ASSERT_TRUE(Result.Paths[0].ReturnValue);
  EXPECT_EQ(Result.Paths[0].ReturnValue->bitVector()->constantValue(),
            llvm::APInt(8, 7));
  EXPECT_GE(Result.Statistics.PrunedPaths, 1U);
  EXPECT_EQ(Result.Statistics.ResolutionSolverQueries, 2U);
}

TEST(FormalMemory, SuccessfulSymbolicAccessModelSatisfiesBounds) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    define i8 @run(i8 %index) {
    entry:
      %array = alloca [4 x i8], align 1
      %p = getelementptr [4 x i8], ptr %array, i64 0, i8 %index
      store i8 7, ptr %p, align 1
      ret i8 1
    }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ScalarExecutor Executor(Expressions, Solver);
  llvm::Function *Function = Module->getFunction("run");
  ExprRef Input = Expressions.symbol(8, "m4_index");
  auto Result = Executor.execute(
      *Function,
      {{Function->getArg(0), SymbolicValue(Input)}});
  ASSERT_EQ(Result.Status, ExecutionStatus::Error);
  const PathResult *Completed = nullptr;
  for (const PathResult &Path : Result.Paths)
    if (Path.Status == PathStatus::Completed)
      Completed = &Path;
  for (const PathResult &Path : Result.Paths) {
    if (Path.Status != PathStatus::Error)
      continue;
    ASSERT_EQ(Path.ErrorReason, PathErrorReason::OutOfBounds);
    ASSERT_TRUE(Path.ErrorModel.has_value());
    std::string ReplayError;
    ASSERT_TRUE(validateModel(Path.Constraints, *Path.ErrorModel, ReplayError))
        << ReplayError;
    auto Witness = evaluate(Input, *Path.ErrorModel, ReplayError);
    ASSERT_TRUE(Witness.has_value()) << ReplayError;
    const std::int64_t Concrete = Witness->getSExtValue();
    EXPECT_TRUE(Concrete < 0 || Concrete >= 4);
    std::string NativeDiagnostic;
    EXPECT_TRUE(replayNativeSymbolicOOB(Concrete, NativeDiagnostic))
        << NativeDiagnostic;
  }
  ASSERT_NE(Completed, nullptr);
  ExprRef Index = Expressions.sext(Input, 64);
  ExprRef Nonnegative = Expressions.logicalNot(
      Expressions.slt(Index, Expressions.constant(64, 0)));
  ExprRef AtMostThree =
      Expressions.ule(Index, Expressions.constant(64, 3));
  ExprRef InBounds = Expressions.select(
      Nonnegative, AtMostThree, Expressions.boolean(false));
  SolverResult Check = Solver.check(Completed->Constraints);
  ASSERT_EQ(Check.Status, SolverStatus::Sat);
  ASSERT_TRUE(Check.SatisfyingModel.has_value());
  std::string Error;
  auto Evaluated = evaluate(InBounds, *Check.SatisfyingModel, Error);
  ASSERT_TRUE(Evaluated.has_value()) << Error;
  EXPECT_TRUE(Evaluated->getBoolValue());
  SolverResult Counterexample = Solver.mayBeTrue(
      Completed->Constraints, Expressions.logicalNot(InBounds));
  EXPECT_EQ(Counterexample.Status, SolverStatus::Unsat);
}

TEST(FormalMemory, RepeatedExecutionHasStableObjectAndStateCounts) {
  llvm::LLVMContext Context;
  auto Module = pointerSelectModule(Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  ScriptedSolver FirstSolver({SolverStatus::Sat, SolverStatus::Unsat});
  ScriptedSolver SecondSolver({SolverStatus::Sat, SolverStatus::Unsat});
  ScalarExecutor First(Expressions, FirstSolver);
  ScalarExecutor Second(Expressions, SecondSolver);
  llvm::Function *Function = Module->getFunction("run");
  auto Binding = ArgumentBinding{
      Function->getArg(0),
      SymbolicValue(Expressions.symbol(1, "choose"))};
  auto A = First.execute(*Function, {Binding});
  auto B = Second.execute(*Function, {Binding});
  ASSERT_EQ(A.Paths.size(), B.Paths.size());
  EXPECT_EQ(A.Paths[0].StateId, B.Paths[0].StateId);
  EXPECT_EQ(A.Paths[0].Status, B.Paths[0].Status);
  EXPECT_EQ(A.Statistics.MemoryObjects, B.Statistics.MemoryObjects);
  EXPECT_EQ(A.Statistics.AllocatedObjectIds,
            (std::vector<std::uint64_t>{1, 2}));
  EXPECT_EQ(A.Statistics.AllocatedObjectIds,
            B.Statistics.AllocatedObjectIds);
  EXPECT_EQ(A.Statistics.ObjectStates, B.Statistics.ObjectStates);
}

TEST(FormalMemory, AddressSpaceSharesUntilFirstWrite) {
  ExprContext Expressions;
  auto Object = object(1, 8);
  AddressSpace Parent;
  ASSERT_TRUE(Parent.bind(Object,
                          std::make_shared<ObjectState>(Object, Expressions,
                                                        64)));
  AddressSpace Child = Parent;
  ASSERT_EQ(Parent.find(1)->State.get(), Child.find(1)->State.get());
  bool Copied = false;
  auto Writable = Child.getWriteable(1, Copied);
  ASSERT_TRUE(Writable);
  EXPECT_TRUE(Copied);
  EXPECT_NE(Parent.find(1)->State.get(), Child.find(1)->State.get());
  Writable->writeByte(Expressions.constant(64, 0),
                      Expressions.constant(8, 42), Expressions);
  EXPECT_FALSE(Parent.find(1)->State->initialization()[0]
                   ->constantValue()
                   .getBoolValue());
  EXPECT_TRUE(Child.find(1)->State->initialization()[0]
                  ->constantValue()
                  .getBoolValue());
  auto Again = Child.getWriteable(1, Copied);
  EXPECT_EQ(&*Again, &*Writable);
  EXPECT_FALSE(Copied);
}

TEST(FormalMemory, ConcreteLookupNeverMergesAdjacentObjects) {
  ExprContext Expressions;
  auto A = object(1, 4);
  auto B = object(2, 4);
  AddressSpace Space;
  ASSERT_TRUE(Space.bind(A, std::make_shared<ObjectState>(A, Expressions,
                                                          64)));
  ASSERT_TRUE(Space.bind(B, std::make_shared<ObjectState>(B, Expressions,
                                                          64)));
  ASSERT_NE(Space.findConcrete(A->LogicalBase + 3), nullptr);
  EXPECT_EQ(Space.findConcrete(A->LogicalBase + 4), nullptr);
  ASSERT_NE(Space.findConcrete(B->LogicalBase), nullptr);
  EXPECT_EQ(Space.findConcrete(B->LogicalBase)->Object->Id, B->Id);
  EXPECT_TRUE(Space.unbind(A->Id));
  EXPECT_EQ(Space.findConcrete(A->LogicalBase), nullptr);
}

TEST(FormalMemory, AddressSpaceRejectsOverlappingObjectsAndMismatchedState) {
  ExprContext Expressions;
  auto First = object(1, 32);
  auto Overlap = object(2, 8);
  Overlap->LogicalBase = First->LogicalBase + 16;
  AddressSpace Forward;
  ASSERT_TRUE(Forward.bind(
      First, std::make_shared<ObjectState>(First, Expressions, 64)));
  EXPECT_THROW(Forward.bind(
                   Overlap,
                   std::make_shared<ObjectState>(Overlap, Expressions, 64)),
               MemoryModelException);
  ASSERT_EQ(Forward.find(First->Id)->Object.get(), First.get());
  EXPECT_EQ(Forward.find(Overlap->Id), nullptr);

  AddressSpace Reverse;
  ASSERT_TRUE(Reverse.bind(
      Overlap, std::make_shared<ObjectState>(Overlap, Expressions, 64)));
  EXPECT_THROW(Reverse.bind(
                   First,
                   std::make_shared<ObjectState>(First, Expressions, 64)),
               MemoryModelException);

  auto SameIdDifferentObject = object(1, 32);
  AddressSpace Mismatched;
  EXPECT_THROW(Mismatched.bind(
                   First, std::make_shared<ObjectState>(
                              SameIdDifferentObject, Expressions, 64)),
               MemoryModelException);
}

TEST(FormalMemory, ForeignPointerWithCollidingObjectIdNeverCompletes) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    @g = global i8 42, align 1
    define i8 @read(ptr %p) {
    entry:
      %global = load i8, ptr @g, align 1
      %value = load i8, ptr %p, align 1
      %sum = add i8 %global, %value
      ret i8 %sum
    }
    define i1 @compare(ptr %p) {
    entry:
      %equal = icmp eq ptr %p, @g
      ret i1 %equal
    }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ScalarExecutor Executor(Expressions, Solver);
  auto Foreign = object(1, 1);
  Foreign->LogicalBase = 0x10000;
  PointerValue Pointer = PointerValue::object(
      Foreign, Expressions.constant(64, 0), Expressions);
  for (const char *Name : {"read", "compare"}) {
    llvm::Function *Function = Module->getFunction(Name);
    auto Result = Executor.execute(
        *Function, {{Function->getArg(0), SymbolicValue(Pointer)}});
    ASSERT_EQ(Result.Paths.size(), 1U) << Name;
    EXPECT_EQ(Result.Status, ExecutionStatus::Unknown) << Name;
    EXPECT_EQ(Result.Paths[0].Status, PathStatus::Incomplete) << Name;
    EXPECT_EQ(Result.Paths[0].IncompleteReason,
              IncompletenessReason::PointerProvenanceLost) << Name;
    EXPECT_EQ(Result.Statistics.AllocatedObjectIds,
              (std::vector<std::uint64_t>{1})) << Name;
  }
}

TEST(FormalMemory, SymbolicInboundsGEPSeparatesDefinedAndUndefinedIndices) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    define void @run(i8 %index) {
    entry:
      %array = alloca [4 x i8], align 1
      %p = getelementptr inbounds [4 x i8], ptr %array, i64 0, i8 %index
      store i8 7, ptr %p, align 1
      ret void
    }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  ExprContext Expressions;
  STPSolver Solver;
  ScalarExecutor Executor(Expressions, Solver);
  llvm::Function *Function = Module->getFunction("run");
  ExprRef Index = Expressions.symbol(8, "symbolic_inbounds_index");
  auto Result = Executor.execute(
      *Function, {{Function->getArg(0), SymbolicValue(Index)}});

  const PathResult *Completed = nullptr;
  const PathResult *Undefined = nullptr;
  const PathResult *OnePast = nullptr;
  for (const PathResult &Path : Result.Paths) {
    if (Path.Status == PathStatus::Completed)
      Completed = &Path;
    else if (Path.IncompleteReason == IncompletenessReason::InboundsGEP)
      Undefined = &Path;
    else if (Path.ErrorReason == PathErrorReason::OutOfBounds)
      OnePast = &Path;
  }
  ASSERT_NE(Completed, nullptr);
  ASSERT_NE(Undefined, nullptr);
  ASSERT_NE(OnePast, nullptr);
  EXPECT_EQ(Result.Status, ExecutionStatus::Error);
  for (const PathResult &Path : Result.Paths) {
    if (Path.Status != PathStatus::Error)
      continue;
    ASSERT_EQ(Path.ErrorReason, PathErrorReason::OutOfBounds);
    ASSERT_TRUE(Path.ErrorModel.has_value());
    std::string ReplayError;
    ASSERT_TRUE(validateModel(Path.Constraints, *Path.ErrorModel,
                              ReplayError)) << ReplayError;
    auto Witness = evaluate(Index, *Path.ErrorModel, ReplayError);
    ASSERT_TRUE(Witness.has_value()) << ReplayError;
    EXPECT_EQ(Witness->getSExtValue(), 4);
    std::string NativeDiagnostic;
    EXPECT_TRUE(replayNativeSymbolicOOB(Witness->getSExtValue(),
                                       NativeDiagnostic))
        << NativeDiagnostic;
  }
  for (const auto &[Path, Value] :
       std::array<std::pair<const PathResult *, unsigned>, 3>{
           {{Completed, 3}, {Undefined, 5}, {OnePast, 4}}}) {
    SolverResult Check = Solver.mayBeTrue(
        Path->Constraints,
        Expressions.eq(Index, Expressions.constant(8, Value)));
    EXPECT_EQ(Check.Status, SolverStatus::Sat) << Value;
  }
}

TEST(FormalMemory, PointerShadowRoundTripAndOverlapInvalidation) {
  ExprContext Expressions;
  auto Target = object(1, 1);
  auto Slot = object(2, 8);
  AddressSpace Parent;
  ASSERT_TRUE(Parent.bind(Target,
                          std::make_shared<ObjectState>(Target, Expressions,
                                                        64)));
  ASSERT_TRUE(Parent.bind(Slot,
                          std::make_shared<ObjectState>(Slot, Expressions,
                                                        64)));
  PointerValue Pointer = PointerValue::object(
      Target, Expressions.constant(64, 0), Expressions);
  bool Copied = false;
  auto SlotState = Parent.getWriteable(2, Copied);
  for (unsigned Byte = 0; Byte < 8; ++Byte)
    SlotState->writeByte(Expressions.constant(64, Byte),
                         Expressions.extract(Pointer.Numeric, Byte * 8, 8),
                         Expressions);
  SlotState->installShadow(
      {0, 8, 64, 0, Pointer, Pointer.Numeric});
  SlotState = {};
  AddressSpace Child = Parent;
  ASSERT_NE(Parent.find(2)->State->findShadow(0, 8, 64, 0), nullptr);
  SlotState = Child.getWriteable(2, Copied);
  ASSERT_TRUE(Copied);
  SlotState->writeByte(Expressions.constant(64, 3),
                       Expressions.constant(8, 0), Expressions);
  EXPECT_EQ(Child.find(2)->State->findShadow(0, 8, 64, 0), nullptr);
  EXPECT_NE(Parent.find(2)->State->findShadow(0, 8, 64, 0), nullptr);
}

TEST(FormalMemory, AddressSensitiveByteMarkersSurviveForkAndPartialWrite) {
  ExprContext Expressions;
  auto Slot = object(1, 8);
  AddressSpace Parent;
  ASSERT_TRUE(Parent.bind(
      Slot, std::make_shared<ObjectState>(Slot, Expressions, 64)));
  bool Copied = false;
  auto Writable = Parent.getWriteable(1, Copied);
  ASSERT_TRUE(Writable);
  for (unsigned Byte = 0; Byte < 8; ++Byte)
    Writable->writeByte(Expressions.constant(64, Byte),
                        Expressions.constant(8, Byte), Expressions, false,
                        Expressions.boolean(true));
  Writable = {};

  AddressSpace Child = Parent;
  Writable = Child.getWriteable(1, Copied);
  ASSERT_TRUE(Writable);
  ASSERT_TRUE(Copied);
  Writable->writeByte(Expressions.constant(64, 0),
                      Expressions.constant(8, 5), Expressions);
  Writable = {};
  for (unsigned Byte = 0; Byte < 8; ++Byte) {
    ExprRef Offset = Expressions.constant(64, Byte);
    ExprRef ParentMarker =
        Parent.find(1)->State->addressSensitiveAt(Offset, Expressions);
    ExprRef ChildMarker =
        Child.find(1)->State->addressSensitiveAt(Offset, Expressions);
    ASSERT_TRUE(ParentMarker->isConstant());
    ASSERT_TRUE(ChildMarker->isConstant());
    EXPECT_TRUE(ParentMarker->constantValue().getBoolValue()) << Byte;
    EXPECT_EQ(ChildMarker->constantValue().getBoolValue(), Byte != 0)
        << Byte;
  }
}

TEST(FormalMemory, MemcpyRetainsAddressSensitiveBytesUntilFullyOverwritten) {
  llvm::LLVMContext Context;
  llvm::SMDiagnostic Diagnostic;
  auto Module = llvm::parseAssemblyString(R"(
    target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
    @target = global i8 7
    declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

    define i64 @copied() {
    entry:
      %source = alloca ptr, align 8
      %dest = alloca ptr, align 8
      store ptr @target, ptr %source, align 8
      call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %source, i64 8, i1 false)
      %bits = load i64, ptr %dest, align 8
      ret i64 %bits
    }
    define i64 @partial() {
    entry:
      %source = alloca ptr, align 8
      %dest = alloca ptr, align 8
      store ptr @target, ptr %source, align 8
      call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %source, i64 8, i1 false)
      store i8 0, ptr %dest, align 1
      %bits = load i64, ptr %dest, align 8
      ret i64 %bits
    }
    define i64 @overwritten() {
    entry:
      %source = alloca ptr, align 8
      %dest = alloca ptr, align 8
      store ptr @target, ptr %source, align 8
      call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %source, i64 8, i1 false)
      store i64 5, ptr %dest, align 8
      %bits = load i64, ptr %dest, align 8
      ret i64 %bits
    }
  )", Diagnostic, Context);
  ASSERT_NE(Module, nullptr);
  for (const char *Name : {"copied", "partial", "overwritten"}) {
    SCOPED_TRACE(Name);
    ExprContext Expressions;
    STPSolver Solver;
    ScalarExecutor Executor(Expressions, Solver);
    auto Result = Executor.execute(*Module->getFunction(Name), {});
    ASSERT_EQ(Result.Paths.size(), 1U);
    if (std::string(Name) == "overwritten") {
      ASSERT_EQ(Result.Status, ExecutionStatus::Complete);
      ASSERT_EQ(Result.Paths[0].Status, PathStatus::Completed);
      ASSERT_TRUE(Result.Paths[0].ReturnValue.has_value());
      EXPECT_EQ(Result.Paths[0].ReturnValue->bitVector()->constantValue(),
                llvm::APInt(64, 5));
    } else {
      EXPECT_EQ(Result.Status, ExecutionStatus::Unknown);
      EXPECT_EQ(Result.Paths[0].Status, PathStatus::Incomplete);
      EXPECT_EQ(Result.Paths[0].IncompleteReason,
                IncompletenessReason::PointerProvenanceLost);
    }
  }
}

TEST(FormalMemory, ActiveWriteHandleCannotMutateAFork) {
  ExprContext Expressions;
  auto Object = object(1, 8);
  auto Untouched = object(2, 8);
  AddressSpace Parent;
  ASSERT_TRUE(Parent.bind(Object, std::make_shared<ObjectState>(
                                      Object, Expressions, 64)));
  ASSERT_TRUE(Parent.bind(Untouched, std::make_shared<ObjectState>(
                                         Untouched, Expressions, 64)));
  bool Copied = false;
  auto ExistingWrite = Parent.getWriteable(1, Copied);
  ASSERT_TRUE(ExistingWrite);
  AddressSpace Child = Parent;
  EXPECT_NE(Parent.find(1)->State.get(), Child.find(1)->State.get());
  EXPECT_EQ(Parent.find(2)->State.get(), Child.find(2)->State.get());
  ExistingWrite->writeByte(Expressions.constant(64, 0),
                           Expressions.constant(8, 99), Expressions);
  EXPECT_TRUE(Parent.find(1)->State->initialization()[0]->constantValue()
                  .getBoolValue());
  EXPECT_FALSE(Child.find(1)->State->initialization()[0]->constantValue()
                   .getBoolValue());
}

TEST(FormalMemory, BindDoesNotRetainCallerMutableAlias) {
  ExprContext Expressions;
  auto Object = object(1, 8);
  auto CallerState = std::make_shared<ObjectState>(Object, Expressions, 64);
  AddressSpace Parent;
  ASSERT_TRUE(Parent.bind(Object, CallerState));
  AddressSpace Child = Parent;
  CallerState->writeByte(Expressions.constant(64, 0),
                         Expressions.constant(8, 7), Expressions);
  EXPECT_FALSE(Parent.find(1)->State->initialization()[0]->constantValue()
                   .getBoolValue());
  EXPECT_FALSE(Child.find(1)->State->initialization()[0]->constantValue()
                   .getBoolValue());
}

TEST(FormalMemory, FixedSeedReferenceBytesAndForkIsolation) {
  constexpr std::uint64_t Seed = 0x4d340001U;
  std::mt19937_64 Random(Seed);
  ExprContext Expressions;
  auto Object = object(1, 16);
  AddressSpace Space;
  ASSERT_TRUE(Space.bind(Object,
                         std::make_shared<ObjectState>(Object, Expressions,
                                                       64)));
  std::array<std::uint8_t, 16> Reference{};
  std::array<bool, 16> Initialized{};
  for (unsigned Iteration = 0; Iteration < 256; ++Iteration) {
    unsigned Start = static_cast<unsigned>(Random() % 16);
    unsigned Width = 1 + static_cast<unsigned>(Random() % (16 - Start));
    AddressSpace Sibling = Space;
    auto Before = Space.find(1)->State;
    bool Copied = false;
    auto Writable = Space.getWriteable(1, Copied);
    ASSERT_TRUE(Copied);
    for (unsigned Byte = 0; Byte < Width; ++Byte) {
      std::uint8_t Value = static_cast<std::uint8_t>(Random());
      Reference[Start + Byte] = Value;
      Initialized[Start + Byte] = true;
      Writable->writeByte(Expressions.constant(64, Start + Byte),
                          Expressions.constant(8, Value), Expressions);
    }
    for (unsigned Byte = 0; Byte < 16; ++Byte) {
      if (!Initialized[Byte])
        continue;
      ExprRef Actual = Writable->readByte(Expressions.constant(64, Byte),
                                         Expressions);
      if (!Actual->isConstant() ||
          Actual->constantValue().getZExtValue() != Reference[Byte]) {
        std::ofstream Failure("artifacts/m4-property-failure.txt");
        Failure << "seed=" << Seed << " iteration=" << Iteration
                << " minimal_byte=" << Byte << " expected="
                << static_cast<unsigned>(Reference[Byte]) << '\n';
      }
      ASSERT_TRUE(Actual->isConstant());
      EXPECT_EQ(Actual->constantValue().getZExtValue(), Reference[Byte]);
    }
    EXPECT_EQ(Sibling.find(1)->State.get(), Before.get());
    EXPECT_NE(Space.find(1)->State.get(), Before.get());
  }
}

} // namespace
