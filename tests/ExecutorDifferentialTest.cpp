#include "formal/Executor.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

#include <cstdint>
#include <cstring>
#include <dlfcn.h>
#include <filesystem>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

using namespace formal;

namespace {

STPSolverOptions differentialSolverOptions() {
  STPSolverOptions Options;
  Options.FailureQueryDirectory = "m3-differential-query-artifacts";
  return Options;
}

struct SymbolicFunction {
  ExprContext Expressions;
  STPSolver Solver;
  ExecutionResult Result;

  SymbolicFunction(llvm::Function &Function,
                   const std::vector<std::string> &Names)
      : Solver(differentialSolverOptions()) {
    std::vector<ArgumentBinding> Bindings;
    std::size_t Index = 0;
    for (llvm::Argument &Argument : Function.args()) {
      unsigned Width =
          llvm::cast<llvm::IntegerType>(Argument.getType())->getBitWidth();
      Bindings.push_back(
          {&Argument, SymbolicValue(Expressions.symbol(Width, Names[Index]))});
      ++Index;
    }
    ScalarExecutor Executor(Expressions, Solver);
    Result = Executor.execute(Function, Bindings);
  }
};

class NativeLibrary final {
public:
  NativeLibrary()
      : Handle(
            dlopen(FORMAL_M3_NATIVE_LIBRARY, RTLD_NOW | RTLD_LOCAL)) {
    if (!Handle)
      throw std::runtime_error(std::string("dlopen failed: ") + dlerror());
  }

  ~NativeLibrary() {
    if (Handle)
      dlclose(Handle);
  }

  NativeLibrary(const NativeLibrary &) = delete;
  NativeLibrary &operator=(const NativeLibrary &) = delete;

  template <typename Function> Function *lookup(const char *Name) const {
    dlerror();
    void *Address = dlsym(Handle, Name);
    if (const char *Error = dlerror())
      throw std::runtime_error(std::string("dlsym failed for ") + Name +
                               ": " + Error);
    static_assert(sizeof(Address) == sizeof(Function *));
    Function *Result = nullptr;
    std::memcpy(&Result, &Address, sizeof(Result));
    return Result;
  }

private:
  void *Handle = nullptr;
};

Model assignment(std::initializer_list<ScalarModelValue> Values) {
  Model Result;
  Result.Scalars.assign(Values.begin(), Values.end());
  return Result;
}

bool pathMatches(const PathResult &Path, const Model &Input) {
  std::string Error;
  for (const ExprRef &Constraint : Path.Constraints) {
    std::optional<llvm::APInt> Value = evaluate(Constraint, Input, Error);
    EXPECT_TRUE(Value.has_value()) << Error;
    if (!Value || Value->isZero())
      return false;
  }
  return true;
}

void expectExactReturn(const ExecutionResult &Result, const Model &Input,
                       const llvm::APInt &Expected) {
  ASSERT_EQ(Result.Status, ExecutionStatus::Complete);
  std::size_t Matches = 0;
  for (const PathResult &Path : Result.Paths) {
    ASSERT_EQ(Path.Status, PathStatus::Completed);
    ASSERT_TRUE(Path.ReturnValue.has_value());
    if (!pathMatches(Path, Input))
      continue;
    ++Matches;
    std::string Error;
    std::optional<llvm::APInt> Actual =
        evaluate(Path.ReturnValue->bitVector(), Input, Error);
    ASSERT_TRUE(Actual.has_value()) << Error;
    EXPECT_EQ(*Actual, Expected);
  }
  EXPECT_EQ(Matches, 1U);
}

class ExecutorDifferential : public testing::Test {
protected:
  void SetUp() override {
    llvm::SMDiagnostic Diagnostic;
    const std::string Input =
        (std::filesystem::path(FORMAL_SOURCE_DIR) /
         "tests/ir/m3/differential.ll")
            .string();
    Module = llvm::parseIRFile(Input, Diagnostic, LLVMContext);
    ASSERT_NE(Module, nullptr);
    Native = std::make_unique<NativeLibrary>();
  }

  llvm::LLVMContext LLVMContext;
  std::unique_ptr<llvm::Module> Module;
  std::unique_ptr<NativeLibrary> Native;
};

TEST_F(ExecutorDifferential, ExhaustiveSingleI8SignedUnsignedAndBranches) {
  SymbolicFunction Execution(*Module->getFunction("diff_signed_unsigned"),
                             {"x"});
  using NativeFunction = std::uint8_t(std::uint8_t);
  NativeFunction *Reference =
      Native->lookup<NativeFunction>("diff_signed_unsigned");
  for (unsigned X = 0; X < 256; ++X) {
    std::uint8_t Expected = Reference(static_cast<std::uint8_t>(X));
    expectExactReturn(Execution.Result,
                      assignment({{"x", Sort::bitVector(8), llvm::APInt(8, X)}}),
                      llvm::APInt(8, Expected));
  }
}

TEST_F(ExecutorDifferential, ExhaustiveSingleI8Switch) {
  SymbolicFunction Execution(*Module->getFunction("diff_switch"), {"x"});
  using NativeFunction = std::uint8_t(std::uint8_t);
  NativeFunction *Reference = Native->lookup<NativeFunction>("diff_switch");
  for (unsigned X = 0; X < 256; ++X) {
    std::uint8_t Expected = Reference(static_cast<std::uint8_t>(X));
    expectExactReturn(Execution.Result,
                      assignment({{"x", Sort::bitVector(8), llvm::APInt(8, X)}}),
                      llvm::APInt(8, Expected));
  }
}

TEST_F(ExecutorDifferential, ExhaustiveSingleI8Casts) {
  SymbolicFunction Execution(*Module->getFunction("diff_casts"), {"x"});
  using NativeFunction = std::uint8_t(std::uint8_t);
  NativeFunction *Reference = Native->lookup<NativeFunction>("diff_casts");
  for (unsigned X = 0; X < 256; ++X) {
    std::uint8_t Expected = Reference(static_cast<std::uint8_t>(X));
    expectExactReturn(Execution.Result,
                      assignment({{"x", Sort::bitVector(8), llvm::APInt(8, X)}}),
                      llvm::APInt(8, Expected));
  }
}

TEST_F(ExecutorDifferential, ExhaustiveTwoI8Inputs) {
  SymbolicFunction Execution(*Module->getFunction("diff_two_i8"), {"x", "y"});
  using NativeFunction = std::uint8_t(std::uint8_t, std::uint8_t);
  NativeFunction *Reference = Native->lookup<NativeFunction>("diff_two_i8");
  for (unsigned X = 0; X < 256; ++X) {
    for (unsigned Y = 0; Y < 256; ++Y) {
      std::uint8_t Expected = Reference(static_cast<std::uint8_t>(X),
                                        static_cast<std::uint8_t>(Y));
      expectExactReturn(
          Execution.Result,
          assignment({{"x", Sort::bitVector(8), llvm::APInt(8, X)},
                      {"y", Sort::bitVector(8), llvm::APInt(8, Y)}}),
          llvm::APInt(8, Expected));
    }
  }
}

} // namespace
