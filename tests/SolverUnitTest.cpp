#include "formal/Solver.h"

#include "llvm/ADT/APInt.h"
#include "gtest/gtest.h"

#include <array>
#include <atomic>
#include <climits>
#include <fcntl.h>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <memory>
#include <random>
#include <stdexcept>
#include <string>
#include <thread>
#include <unistd.h>
#include <vector>

using namespace formal;

namespace {

STPSolver makeSolver(bool ExportAll = false) {
  STPSolverOptions Options;
  Options.FailureQueryDirectory = "m2-query-artifacts";
  Options.ExportAllQueries = ExportAll;
  return STPSolver(Options);
}

void expectSTPValue(STPSolver &Solver, ExprContext &Context,
                    const ConstraintSet &Constraints, const ExprRef &Expression,
                    const llvm::APInt &Expected) {
  ASSERT_TRUE(Expression->sort().isBitVector());
  ASSERT_EQ(Expression->width(), Expected.getBitWidth());

  // The validity query makes the translated STP term part of the formula.  It
  // therefore detects mistranslations independently of getValue/model code.
  SolverResult Proof = Solver.mustBeTrue(
      Constraints, Context.eq(Expression, Context.constant(Expected)));
  ASSERT_EQ(Proof.Status, SolverStatus::Unsat) << Proof.Metadata.Detail;
  ASSERT_TRUE(Proof.Truth);
  EXPECT_TRUE(*Proof.Truth);

  // Also cover the public value path, which must read the target from STP and
  // audit it against the backend-neutral evaluator.
  SolverResult Value = Solver.getValue(Constraints, Expression);
  ASSERT_EQ(Value.Status, SolverStatus::Sat) << Value.Metadata.Detail;
  ASSERT_TRUE(Value.Value);
  EXPECT_EQ(*Value.Value, Expected);
}

llvm::APInt randomAPInt(unsigned Width, std::mt19937_64 &Random) {
  llvm::APInt Value(Width, 0);
  for (unsigned Offset = 0; Offset < Width; Offset += 64)
    Value |= llvm::APInt(Width, Random()).shl(Offset);
  return Value;
}

struct ConcreteExpr {
  ExprRef Term;
  llvm::APInt Value;
};

ConcreteExpr randomConcreteExpr(ExprContext &Context, unsigned Width,
                                const ExprRef &X, const llvm::APInt &XValue,
                                const ExprRef &Y, const llvm::APInt &YValue,
                                std::mt19937_64 &Random, unsigned Depth) {
  if (Depth == 0) {
    switch (Random() % 3) {
    case 0:
      return {X, XValue};
    case 1:
      return {Y, YValue};
    default: {
      llvm::APInt Value = randomAPInt(Width, Random);
      return {Context.constant(Value), Value};
    }
    }
  }
  ConcreteExpr Left = randomConcreteExpr(Context, Width, X, XValue, Y,
                                         YValue, Random, Depth - 1);
  ConcreteExpr Right = randomConcreteExpr(Context, Width, X, XValue, Y,
                                          YValue, Random, Depth - 1);
  switch (Random() % 13) {
  case 0:
    return {Context.add(Left.Term, Right.Term), Left.Value + Right.Value};
  case 1:
    return {Context.sub(Left.Term, Right.Term), Left.Value - Right.Value};
  case 2:
    return {Context.mul(Left.Term, Right.Term), Left.Value * Right.Value};
  case 3:
    return {Context.bitAnd(Left.Term, Right.Term),
            Left.Value & Right.Value};
  case 4:
    return {Context.bitOr(Left.Term, Right.Term), Left.Value | Right.Value};
  case 5:
    return {Context.bitXor(Left.Term, Right.Term),
            Left.Value ^ Right.Value};
  case 6:
  case 7:
  case 8: {
    unsigned Amount = static_cast<unsigned>(Random() % (Width + 1));
    ExprRef Shift = Context.constant(Width, Amount);
    switch (Random() % 3) {
    case 0:
      return {Context.shl(Left.Term, Shift), Left.Value.shl(Amount)};
    case 1:
      return {Context.lshr(Left.Term, Shift), Left.Value.lshr(Amount)};
    default:
      return {Context.ashr(Left.Term, Shift), Left.Value.ashr(Amount)};
    }
  }
  case 9: {
    bool Condition = Left.Value.ult(Right.Value);
    return {Context.select(Context.ult(Left.Term, Right.Term), Left.Term,
                           Right.Term),
            Condition ? Left.Value : Right.Value};
  }
  case 10: {
    bool Condition = Left.Value.slt(Right.Value);
    return {Context.select(Context.slt(Left.Term, Right.Term), Right.Term,
                           Left.Term),
            Condition ? Right.Value : Left.Value};
  }
  case 11: {
    unsigned Offset = static_cast<unsigned>(Random() % (Width + 1));
    llvm::APInt Concatenated = Left.Value.zext(Width * 2);
    Concatenated = Concatenated.shl(Width) | Right.Value.zext(Width * 2);
    return {Context.extract(Context.concat(Left.Term, Right.Term), Offset,
                            Width),
            Concatenated.lshr(Offset).trunc(Width)};
  }
  default: {
    bool Condition = Left.Value == Right.Value;
    return {Context.select(Context.eq(Left.Term, Right.Term), Left.Term,
                           Right.Term),
            Condition ? Left.Value : Right.Value};
  }
  }
}

TEST(FormalSolver, RandomASTsMatchConcreteAPIntAtMultipleWidths) {
  STPSolver Solver = makeSolver();
  for (unsigned Width : {1U, 8U, 32U, 65U, 128U}) {
    for (unsigned Case = 0; Case < 12; ++Case) {
      const std::uint64_t Seed = 0x7ad8b9632e1f4c05ULL ^
                                 (static_cast<std::uint64_t>(Width) << 32) ^
                                 Case;
      SCOPED_TRACE("seed=" + std::to_string(Seed) +
                   " width=" + std::to_string(Width));
      std::mt19937_64 Random(Seed);
      ExprContext Context;
      ExprRef X = Context.symbol(Width, "random_ast_x");
      ExprRef Y = Context.symbol(Width, "random_ast_y");
      llvm::APInt XValue = randomAPInt(Width, Random);
      llvm::APInt YValue = randomAPInt(Width, Random);
      ConcreteExpr Root = randomConcreteExpr(Context, Width, X, XValue, Y,
                                             YValue, Random, 3);
      ConstraintSet Assignments = {
          Context.eq(X, Context.constant(XValue)),
          Context.eq(Y, Context.constant(YValue))};
      expectSTPValue(Solver, Context, Assignments, Root.Term, Root.Value);
    }
  }
}

TEST(FormalSolver, RandomSymbolicArrayUpdatesMatchConcreteWriteOrder) {
  STPSolver Solver = makeSolver();
  for (unsigned IndexWidth : {1U, 8U, 16U}) {
    for (unsigned Case = 0; Case < 12; ++Case) {
      const std::uint64_t Seed = 0x1c90652ab7f3d841ULL ^
                                 (static_cast<std::uint64_t>(IndexWidth) << 32) ^
                                 Case;
      SCOPED_TRACE("seed=" + std::to_string(Seed) +
                   " index-width=" + std::to_string(IndexWidth));
      std::mt19937_64 Random(Seed);
      ExprContext Context;
      ArrayRef Bytes = Context.array(IndexWidth, "random_update_array");
      ExprRef ReadIndex = Context.symbol(IndexWidth, "random_read_index");
      ExprRef OtherIndex = Context.symbol(IndexWidth, "random_other_index");
      llvm::APInt ReadValue = randomAPInt(IndexWidth, Random);
      llvm::APInt OtherValue = randomAPInt(IndexWidth, Random);
      ConstraintSet Assignments = {
          Context.eq(ReadIndex, Context.constant(ReadValue)),
          Context.eq(OtherIndex, Context.constant(OtherValue))};
      llvm::APInt Expected(8, Random());
      UpdateList Writes = Context.write(Context.updates(Bytes), ReadIndex,
                                        Context.constant(Expected));
      for (unsigned Step = 0; Step < 5; ++Step) {
        const bool UseOther = (Random() & 1) != 0;
        llvm::APInt Value(8, Random());
        Writes = Context.write(Writes, UseOther ? OtherIndex : ReadIndex,
                               Context.constant(Value));
        if (!UseOther || ReadValue == OtherValue)
          Expected = Value;
      }
      expectSTPValue(Solver, Context, Assignments,
                     Context.read(Writes, ReadIndex), Expected);
    }
  }
}

TEST(FormalSolver, UsesPinnedSTPAndMiniSat) {
  STPSolver Solver = makeSolver();
  EXPECT_NE(Solver.backendVersion().find("2.3.3"), std::string::npos);
  EXPECT_NE(Solver.backendVersion().find(
                "1bdfe5076c3f78e2bb7fccc5efb1a2a36c4068e6"),
            std::string::npos);
  const std::string Profile = Solver.backendCompilationProfile();
  EXPECT_TRUE(isPinnedSTPCompilationProfileForTest(Profile));
  EXPECT_EQ(Profile.find("-DNDEBUG"), std::string::npos);
  EXPECT_NE(Profile.find("ENABLE_TESTING = ON"), std::string::npos);
  EXPECT_NE(Profile.find("ONLY_SIMPLE = OFF"), std::string::npos);
  EXPECT_NE(Profile.find("STATICCOMPILE = OFF"), std::string::npos);
  EXPECT_NE(Profile.find("FORCE_CMS = OFF"), std::string::npos);
  EXPECT_NE(Profile.find("ENABLE_PYTHON_INTERFACE = OFF"), std::string::npos);
  EXPECT_NE(Profile.find("libminisat"), std::string::npos);
  EXPECT_EQ(Profile.find("USE_CRYPTOMINISAT"), std::string::npos);
  EXPECT_EQ(Solver.satBackend(), "MiniSat");

  ExprContext Context;
  ExprRef X = Context.symbol(8, "pinned_profile_x");
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 0x5a))});
  EXPECT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
  EXPECT_EQ(Result.Metadata.SatBackend, "MiniSat");

  for (const char *Required :
       {"CMAKE_CXX_COMPILER = /usr/bin/c++", "ONLY_SIMPLE = OFF",
        "STATICCOMPILE = OFF", "FORCE_CMS = OFF", "ENABLE_TESTING = ON",
        "ENABLE_PYTHON_INTERFACE = OFF", "libminisat"}) {
    std::string Missing = Profile;
    const std::size_t Position = Missing.find(Required);
    ASSERT_NE(Position, std::string::npos) << Required;
    Missing.erase(Position, std::string(Required).size());
    EXPECT_FALSE(isPinnedSTPCompilationProfileForTest(Missing)) << Required;
  }
  EXPECT_FALSE(isPinnedSTPCompilationProfileForTest(Profile + " -DNDEBUG"));
  EXPECT_FALSE(isPinnedSTPCompilationProfileForTest(
      Profile + " USE_CRYPTOMINISAT"));
}

TEST(FormalSolver, StatusAndMachineReasonCodesAreDistinct) {
  EXPECT_STREQ(toString(SolverStatus::Sat), "SAT");
  EXPECT_STREQ(toString(SolverStatus::Unsat), "UNSAT");
  EXPECT_STREQ(toString(SolverStatus::Unknown), "UNKNOWN");
  EXPECT_STREQ(toString(SolverStatus::Timeout), "TIMEOUT");
  EXPECT_STREQ(toString(SolverStatus::InternalError), "INTERNAL_ERROR");
  EXPECT_STRNE(toReasonCode(SolverReason::BackendUnknown),
               toReasonCode(SolverReason::Timeout));
  EXPECT_STRNE(toReasonCode(SolverReason::BackendFailure),
               toReasonCode(SolverReason::Unsatisfiable));
}

TEST(FormalSolver, DistinguishesSatUnsatAndTruthQueries) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "sat_x");
  ExprRef IsOne = Context.eq(X, Context.constant(8, 1));
  ExprRef IsTwo = Context.eq(X, Context.constant(8, 2));
  STPSolver Solver = makeSolver();

  SolverResult Sat = Solver.check({IsOne});
  ASSERT_EQ(Sat.Status, SolverStatus::Sat);
  EXPECT_EQ(Sat.Metadata.Reason, SolverReason::Satisfiable);
  EXPECT_EQ(Sat.Metadata.BackendName, "STP");
  EXPECT_EQ(Sat.Metadata.SatBackend, "MiniSat");
  EXPECT_GT(Sat.Metadata.QueryId, 0U);
  EXPECT_GE(Sat.Metadata.Elapsed.count(), 0);
  ASSERT_TRUE(Sat.SatisfyingModel);
  std::string Error;
  EXPECT_TRUE(validateModel({IsOne}, *Sat.SatisfyingModel, Error)) << Error;

  SolverResult Unsat = Solver.check({IsOne, IsTwo});
  EXPECT_EQ(Unsat.Status, SolverStatus::Unsat);
  EXPECT_EQ(Unsat.Metadata.Reason, SolverReason::Unsatisfiable);
  EXPECT_NE(Unsat.Metadata.QueryId, Sat.Metadata.QueryId);

  SolverResult May = Solver.mayBeTrue({IsOne}, IsOne);
  ASSERT_EQ(May.Status, SolverStatus::Sat);
  ASSERT_TRUE(May.Truth);
  EXPECT_TRUE(*May.Truth);

  SolverResult Must = Solver.mustBeTrue({IsOne}, IsOne);
  ASSERT_EQ(Must.Status, SolverStatus::Unsat);
  ASSERT_TRUE(Must.Truth);
  EXPECT_TRUE(*Must.Truth);

  SolverResult NotMust = Solver.mustBeTrue({}, IsOne);
  ASSERT_EQ(NotMust.Status, SolverStatus::Sat);
  ASSERT_TRUE(NotMust.Truth);
  EXPECT_FALSE(*NotMust.Truth);
}

TEST(FormalSolver, KeepsBoolAndBV1Distinct) {
  ExprContext Context;
  ExprRef Bit = Context.symbol(1, "one_bit_value");
  ExprRef Boolean = Context.toBool(Bit);
  ASSERT_TRUE(Boolean->sort().isBool());
  STPSolver Solver = makeSolver();
  EXPECT_EQ(Solver.check({Boolean}).Status, SolverStatus::Sat);
  EXPECT_EQ(Solver.check({Boolean, Context.logicalNot(Boolean)}).Status,
            SolverStatus::Unsat);

  SolverResult Invalid = Solver.check({Bit});
  EXPECT_EQ(Invalid.Status, SolverStatus::InternalError);
  EXPECT_EQ(Invalid.Metadata.Reason, SolverReason::InvalidQuery);
  EXPECT_NE(std::string(toReasonCode(Invalid.Metadata.Reason)).find("INVALID"),
            std::string::npos);

  SolverResult FalseBoolean =
      Solver.getValue({Context.logicalNot(Boolean)}, Boolean);
  ASSERT_EQ(FalseBoolean.Status, SolverStatus::Sat)
      << FalseBoolean.Metadata.Detail;
  ASSERT_TRUE(FalseBoolean.Value);
  EXPECT_TRUE(FalseBoolean.Value->isZero());
  EXPECT_THROW(Context.bitAnd(Boolean, Boolean), std::invalid_argument);
}

TEST(FormalSolver, SignedAndUnsignedComparisonsUseDifferentBoundaries) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "signed_x");
  ExprRef MinusOne = Context.constant(8, 0xff);
  ExprRef One = Context.constant(8, 1);
  ExprRef IsMinusOne = Context.eq(X, MinusOne);
  STPSolver Solver = makeSolver();
  EXPECT_EQ(Solver.mustBeTrue({IsMinusOne}, Context.slt(X, One)).Status,
            SolverStatus::Unsat);
  EXPECT_EQ(Solver.mustBeTrue({IsMinusOne}, Context.ult(X, One)).Status,
            SolverStatus::Sat);
  EXPECT_EQ(*Solver.mustBeTrue({IsMinusOne}, Context.slt(X, One)).Truth,
            true);
  EXPECT_EQ(*Solver.mustBeTrue({IsMinusOne}, Context.ult(X, One)).Truth,
            false);
}

TEST(FormalSolver, SupportsRequiredWidthsAndModularOverflow) {
  STPSolver Solver = makeSolver();
  for (unsigned Width : {1U, 8U, 24U, 32U, 64U, 65U, 128U}) {
    ExprContext Context;
    ExprRef X = Context.symbol(Width, "width_x_" + std::to_string(Width));
    llvm::APInt Maximum = llvm::APInt::getAllOnes(Width);
    ExprRef IsMaximum = Context.eq(X, Context.constant(Maximum));
    expectSTPValue(Solver, Context, {IsMaximum},
                   Context.add(X, Context.constant(Width, 1)),
                   llvm::APInt(Width, 0));
  }
}

TEST(FormalSolver, TranslatesConcatExtractAndCasts) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "cast_x");
  ExprRef Y = Context.symbol(8, "cast_y");
  ExprRef XY = Context.concat(X, Y);
  ConstraintSet Constraints{
      Context.eq(X, Context.constant(8, 0x80)),
      Context.eq(Y, Context.constant(8, 0x34)),
  };
  STPSolver Solver = makeSolver();

  auto ExpectValue = [&](ExprRef Expression, const llvm::APInt &Expected) {
    expectSTPValue(Solver, Context, Constraints, Expression, Expected);
  };
  ExpectValue(XY, llvm::APInt(16, 0x8034));
  ExpectValue(Context.extract(XY, 4, 8), llvm::APInt(8, 0x03));
  ExpectValue(Context.zext(X, 24), llvm::APInt(24, 0x80));
  ExpectValue(Context.sext(X, 24), llvm::APInt(24, 0xffff80));
  ExpectValue(Context.trunc(XY, 8), llvm::APInt(8, 0x34));
}

TEST(FormalSolver, TranslatesShiftDivisionAndRemainderNormalInputs) {
  ExprContext Context;
  ExprRef X = Context.symbol(24, "arithmetic_x");
  ExprRef Y = Context.symbol(24, "arithmetic_y");
  ConstraintSet Constraints{
      Context.eq(X, Context.constant(24, 0xfffff0)),
      Context.eq(Y, Context.constant(24, 4)),
  };
  STPSolver Solver = makeSolver();
  const std::array<std::pair<ExprRef, llvm::APInt>, 7> Cases{{
      {Context.shl(Y, Context.constant(24, 2)), llvm::APInt(24, 16)},
      {Context.lshr(X, Context.constant(24, 4)), llvm::APInt(24, 0x0fffff)},
      {Context.ashr(X, Context.constant(24, 4)), llvm::APInt(24, 0xffffff)},
      {Context.udiv(X, Y), llvm::APInt(24, 0x3ffffc)},
      {Context.urem(X, Y), llvm::APInt(24, 0)},
      {Context.sdiv(X, Y), llvm::APInt(24, 0xfffffc)},
      {Context.srem(X, Y), llvm::APInt(24, 0)},
  }};
  for (const auto &[Expression, Expected] : Cases) {
    expectSTPValue(Solver, Context, Constraints, Expression, Expected);
  }
}

TEST(FormalSolver, TranslatesArithmeticBitwiseBooleanAndSelectNodes) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "nodes_x");
  ExprRef Y = Context.symbol(8, "nodes_y");
  ExprRef Flag = Context.booleanSymbol("nodes_flag");
  ConstraintSet Constraints{
      Context.eq(X, Context.constant(8, 6)),
      Context.eq(Y, Context.constant(8, 3)), Flag,
  };
  STPSolver Solver = makeSolver();
  const std::array<std::pair<ExprRef, llvm::APInt>, 8> Cases{{
      {Context.add(X, Y), llvm::APInt(8, 9)},
      {Context.sub(X, Y), llvm::APInt(8, 3)},
      {Context.mul(X, Y), llvm::APInt(8, 18)},
      {Context.bitAnd(X, Y), llvm::APInt(8, 2)},
      {Context.bitOr(X, Y), llvm::APInt(8, 7)},
      {Context.bitXor(X, Y), llvm::APInt(8, 5)},
      {Context.select(Flag, X, Y), llvm::APInt(8, 6)},
      {Context.select(Context.logicalNot(Flag), X, Y), llvm::APInt(8, 3)},
  }};
  for (const auto &[Expression, Expected] : Cases) {
    expectSTPValue(Solver, Context, Constraints, Expression, Expected);
  }
}

TEST(FormalSolver, DirectSTPEdgeCaseSemantics) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "edge_x");
  ExprRef Y = Context.symbol(8, "edge_y");
  STPSolver Solver = makeSolver();

  auto Check = [&](std::uint8_t XValue, std::uint8_t YValue,
                   const ExprRef &Expression, std::uint8_t Expected) {
    ConstraintSet Constraints{
        Context.eq(X, Context.constant(8, XValue)),
        Context.eq(Y, Context.constant(8, YValue)),
    };
    expectSTPValue(Solver, Context, Constraints, Expression,
                   llvm::APInt(8, Expected));
  };

  Check(0x80, 0xff, Context.sdiv(X, Y), 0x80); // INT_MIN / -1
  Check(0xf9, 0x03, Context.srem(X, Y), 0xff); // -7 % 3 == -1
  Check(0x80, 0x00, Context.sdiv(X, Y), 0x01);
  Check(0x07, 0x00, Context.sdiv(X, Y), 0xff);
  Check(0x37, 0x00, Context.udiv(X, Y), 0xff);
  Check(0x37, 0x00, Context.urem(X, Y), 0x37);
  Check(0x81, 0x08, Context.shl(X, Y), 0x00);
  Check(0x81, 0x08, Context.lshr(X, Y), 0x00);
  Check(0x81, 0x08, Context.ashr(X, Y), 0xff);

  ConstraintSet Ordered{Context.eq(X, Context.constant(8, 0x80)),
                        Context.eq(Y, Context.constant(8, 0x7f))};
  EXPECT_TRUE(*Solver.mustBeTrue(Ordered, Context.ule(X, X)).Truth);
  EXPECT_TRUE(*Solver.mustBeTrue(Ordered, Context.sle(X, Y)).Truth);
  EXPECT_FALSE(*Solver.mustBeTrue(Ordered, Context.ule(X, Y)).Truth);
}

TEST(FormalSolver, ArrayReadWriteAndPersistentWriteOrderAreSemantic) {
  ExprContext Context;
  ArrayRef Bytes = Context.array(8, "solver_bytes");
  UpdateList Base = Context.updates(Bytes);
  ExprRef I = Context.symbol(8, "array_i");
  ExprRef J = Context.symbol(8, "array_j");
  UpdateList First = Context.write(Base, I, Context.constant(8, 0x11));
  UpdateList Second = Context.write(First, J, Context.constant(8, 0x22));
  STPSolver Solver = makeSolver();

  EXPECT_TRUE(*Solver.mustBeTrue({},
      Context.eq(Context.read(First, I), Context.constant(8, 0x11))).Truth);
  EXPECT_TRUE(*Solver.mustBeTrue({Context.eq(I, J)},
      Context.eq(Context.read(Second, I), Context.constant(8, 0x22))).Truth);

  ExprRef RootRead = Context.read(Base, Context.constant(8, 2));
  SolverResult Initial = Solver.getInitialValues(
      {Context.eq(RootRead, Context.constant(8, 0xa5))}, {{Bytes, 4}});
  ASSERT_EQ(Initial.Status, SolverStatus::Sat);
  ASSERT_EQ(Initial.InitialValues.size(), 1U);
  ASSERT_EQ(Initial.InitialValues[0].Bytes.size(), 4U);
  EXPECT_EQ(Initial.InitialValues[0].Bytes[2], 0xa5);
  EXPECT_EQ(Initial.Metadata.Translation.ArrayTranslations, 1U);
  EXPECT_GT(Initial.Metadata.Translation.ArrayCacheHits, 0U);
  ASSERT_TRUE(Initial.SatisfyingModel);
  std::string Error;
  EXPECT_TRUE(validateModel(
      {Context.eq(RootRead, Context.constant(8, 0xa5))},
      *Initial.SatisfyingModel, Error)) << Error;
}

TEST(FormalSolver, ExtractsInitialModelThroughSymbolicUpdateChains) {
  ExprContext Context;
  ArrayRef Bytes = Context.array(8, "symbolic_update_bytes");
  UpdateList Base = Context.updates(Bytes);
  ExprRef I = Context.symbol(8, "symbolic_update_i");
  ExprRef J = Context.symbol(8, "symbolic_update_j");
  ExprRef K = Context.symbol(8, "symbolic_update_k");
  UpdateList Updates = Context.write(Base, I, Context.constant(8, 0x11));
  Updates = Context.write(Updates, Context.constant(8, 2),
                          Context.constant(8, 0x44));
  Updates = Context.write(Updates, J, Context.constant(8, 0x22));
  Updates = Context.write(Updates, K, Context.constant(8, 0x55));

  ConstraintSet Constraints{
      Context.eq(I, Context.constant(8, 1)),
      Context.eq(J, Context.constant(8, 1)),
      Context.eq(K, Context.constant(8, 3)),
      Context.eq(Context.read(Base, Context.constant(8, 0)),
                 Context.constant(8, 0xa0)),
      Context.eq(Context.read(Base, Context.constant(8, 1)),
                 Context.constant(8, 0xb1)),
      Context.eq(Context.read(Base, Context.constant(8, 2)),
                 Context.constant(8, 0xb2)),
      Context.eq(Context.read(Base, Context.constant(8, 3)),
                 Context.constant(8, 0xb3)),
      Context.eq(Context.read(Updates, I), Context.constant(8, 0x22)),
      Context.eq(Context.read(Updates, Context.constant(8, 2)),
                 Context.constant(8, 0x44)),
      Context.eq(Context.read(Updates, K), Context.constant(8, 0x55)),
      Context.eq(Context.read(Updates, Context.constant(8, 0)),
                 Context.constant(8, 0xa0)),
  };

  STPSolver Solver = makeSolver();
  SolverResult Result = Solver.getInitialValues(Constraints, {{Bytes, 4}});
  ASSERT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
  ASSERT_EQ(Result.InitialValues.size(), 1U);
  EXPECT_EQ(Result.InitialValues[0].Bytes,
            (std::vector<std::uint8_t>{0xa0, 0xb1, 0xb2, 0xb3}));
  ASSERT_TRUE(Result.SatisfyingModel);
  std::string Error;
  EXPECT_TRUE(validateModel(Constraints, *Result.SatisfyingModel, Error))
      << Error;
}

TEST(FormalSolver, QueriesAndTranslationCachesAreContextIsolated) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "isolation_x");
  ExprRef IsOne = Context.eq(X, Context.constant(8, 1));
  ExprRef IsTwo = Context.eq(X, Context.constant(8, 2));
  STPSolver Solver = makeSolver();

  SolverResult First = Solver.check({IsOne, IsOne, IsTwo});
  EXPECT_EQ(First.Status, SolverStatus::Unsat);
  EXPECT_GT(First.Metadata.Translation.ExpressionCacheHits, 0U);
  SolverResult Second = Solver.check({IsOne});
  EXPECT_EQ(Second.Status, SolverStatus::Sat);
  EXPECT_NE(First.Metadata.Translation.ContextId,
            Second.Metadata.Translation.ContextId);
  EXPECT_GT(Second.Metadata.Translation.ContextId, 0U);
}

TEST(FormalSolver, ExecWorkerDoesNotInheritUnrelatedHostDescriptors) {
  int Descriptor = ::open("/dev/null", O_RDONLY);
  ASSERT_GE(Descriptor, 0);

  ExprContext Context;
  ExprRef X = Context.symbol(8, "descriptor_isolation_x");
  STPSolver Solver = makeSolver();
  setSTPSolverWorkerTestMode(1000 + Descriptor);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  setSTPSolverWorkerTestMode(0);
  ::close(Descriptor);

  EXPECT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
}

TEST(FormalSolver, RejectsAmbiguousScalarIdentityAcrossExpressionContexts) {
  ExprContext FirstContext;
  ExprContext SecondContext;
  ExprRef First = FirstContext.symbol(8, "shared_backend_scalar");
  ExprRef Second = SecondContext.symbol(8, "shared_backend_scalar");
  ExprRef FirstIsOne =
      FirstContext.eq(First, FirstContext.constant(8, 1));
  ExprRef SecondIsTwo =
      SecondContext.eq(Second, SecondContext.constant(8, 2));
  STPSolver Solver = makeSolver();

  EXPECT_EQ(Solver.check({FirstIsOne}).Status, SolverStatus::Sat);
  EXPECT_EQ(Solver.check({SecondIsTwo}).Status, SolverStatus::Sat);
  SolverResult Mixed = Solver.check({FirstIsOne, SecondIsTwo});
  EXPECT_EQ(Mixed.Status, SolverStatus::InternalError)
      << Mixed.Metadata.Detail;
  EXPECT_EQ(Mixed.Metadata.Reason, SolverReason::InvalidQuery);
  EXPECT_NE(Mixed.Metadata.Detail.find("distinct scalar"), std::string::npos);
}

TEST(FormalSolver, RejectsInvalidArrayRequestsBeforeUnsatSolving) {
  ExprContext Context;
  ArrayRef Narrow = Context.array(1, "request_narrow_index");
  ArrayRef Wide = Context.array(8, "request_byte_limit");
  STPSolverOptions Options;
  Options.MaxArrayBytes = 4;
  Options.FailureQueryDirectory = "m2-query-artifacts";
  STPSolver Solver(Options);
  ConstraintSet Unsatisfiable{Context.boolean(false)};

  for (const ArrayModelRequest &Request :
       {ArrayModelRequest{Narrow, 3}, ArrayModelRequest{Wide, 5}}) {
    SolverResult Result = Solver.getInitialValues(Unsatisfiable, {Request});
    EXPECT_EQ(Result.Status, SolverStatus::InternalError)
        << Result.Metadata.Detail;
    EXPECT_EQ(Result.Metadata.Reason, SolverReason::InvalidQuery);
    EXPECT_NE(Result.Status, SolverStatus::Unsat);
    EXPECT_NE(Result.Metadata.Detail.find(
                  Request.Root == Narrow ? "wraps its index sort"
                                         : "exceeds byte limit"),
              std::string::npos);
  }
}

TEST(FormalSolver, RejectsWidthsThatNarrowAtTheSTPBoundary) {
  ExprContextOptions ContextOptions;
  ContextOptions.MaxBitWidth = static_cast<unsigned>(INT_MAX) + 1U;
  ExprContext Context(ContextOptions);
  ExprRef TooWide =
      Context.symbol(static_cast<unsigned>(INT_MAX) + 1U, "too_wide_for_stp");
  STPSolver Solver = makeSolver();
  SolverResult Result = Solver.getValue({}, TooWide);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::TranslationError);
  EXPECT_NE(Result.Metadata.Detail.find("INT_MAX"), std::string::npos);
}

TEST(FormalSolver, ReturnedModelCanBeIndependentlyEvaluatedAndRejected) {
  ExprContext Context;
  ExprRef X = Context.symbol(65, "model_x");
  ExprRef Constraint = Context.eq(X, Context.constant(65, 17));
  STPSolver Solver = makeSolver();
  SolverResult Result = Solver.check({Constraint});
  ASSERT_EQ(Result.Status, SolverStatus::Sat);
  ASSERT_TRUE(Result.SatisfyingModel);
  std::string Error;
  EXPECT_TRUE(validateModel({Constraint}, *Result.SatisfyingModel, Error));

  Model Corrupt = *Result.SatisfyingModel;
  ASSERT_FALSE(Corrupt.Scalars.empty());
  Corrupt.Scalars[0].Value = llvm::APInt(65, 18);
  EXPECT_FALSE(validateModel({Constraint}, Corrupt, Error));
  EXPECT_NE(Error.find("false"), std::string::npos);
}

TEST(FormalSolver, ModelAuditFailureIsInternalAndExportsQuery) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "rejected_model_x");
  STPSolverOptions Options;
  Options.Timeout = STPSolverOptions::UnlimitedTimeout;
  Options.FailureQueryDirectory = "m2-model-failure-artifacts";
  Options.AdditionalModelAudit =
      [](const ConstraintSet &, const Model &) -> std::optional<std::string> {
    return "independent model audit rejected the assignment";
  };
  STPSolver Solver(Options);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 3))});
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::ModelValidationFailed);
  EXPECT_NE(Result.Metadata.Detail.find("audit rejected"), std::string::npos);
  EXPECT_TRUE(std::filesystem::is_regular_file(Result.Metadata.SavedQuery));
}

TEST(FormalSolver, TimeoutAndFailuresNeverBecomeUnsat) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "timeout_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(0);
  Options.FailureQueryDirectory = "m2-timeout-artifacts";
  STPSolver Solver(Options);
  SolverResult TimedOut =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  EXPECT_EQ(TimedOut.Status, SolverStatus::Timeout);
  EXPECT_NE(TimedOut.Status, SolverStatus::Unsat);
  EXPECT_EQ(TimedOut.Metadata.Reason, SolverReason::Timeout);
  EXPECT_TRUE(std::filesystem::is_regular_file(TimedOut.Metadata.SavedQuery));
  EXPECT_EQ(TimedOut.Metadata.Translation.ContextId, 0U);
  EXPECT_EQ(TimedOut.Metadata.Translation.ExpressionTranslations, 0U);
  EXPECT_EQ(TimedOut.Metadata.Translation.ArrayTranslations, 0U);
  std::ifstream TimeoutArtifact(TimedOut.Metadata.SavedQuery);
  std::string TimeoutText((std::istreambuf_iterator<char>(TimeoutArtifact)),
                          std::istreambuf_iterator<char>());
  EXPECT_EQ(TimeoutText.find("timeout_x"), std::string::npos);

  STPSolver InvalidSolver = makeSolver();
  SolverResult Invalid = InvalidSolver.check({X});
  EXPECT_EQ(Invalid.Status, SolverStatus::InternalError);
  EXPECT_NE(Invalid.Status, SolverStatus::Unsat);
  EXPECT_EQ(Invalid.Metadata.Reason, SolverReason::InvalidQuery);
  EXPECT_TRUE(std::filesystem::is_regular_file(Invalid.Metadata.SavedQuery));
}

TEST(FormalSolver, STPConflictTimeoutMapsToTimeout) {
  ExprContext Context;
  std::vector<ExprRef> Values;
  for (unsigned Index = 0; Index != 33; ++Index)
    Values.push_back(Context.symbol(5, "pigeon_" + std::to_string(Index)));
  ConstraintSet Pigeonhole;
  for (std::size_t Left = 0; Left < Values.size(); ++Left)
    for (std::size_t Right = Left + 1; Right < Values.size(); ++Right)
      Pigeonhole.push_back(
          Context.logicalNot(Context.eq(Values[Left], Values[Right])));

  STPSolverOptions Options;
  Options.MaxConflicts = 1;
  Options.FailureQueryDirectory = "m2-conflict-timeout-artifacts";
  STPSolver Solver(Options);
  SolverResult Result = Solver.check(Pigeonhole);
  EXPECT_EQ(Result.Status, SolverStatus::Timeout) << Result.Metadata.Detail;
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::Timeout);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);
  EXPECT_TRUE(std::filesystem::is_regular_file(Result.Metadata.SavedQuery));
}

TEST(FormalSolver, WallClockDeadlineKillsTheSTPWorker) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "wall_timeout_x");

  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory = "m2-wall-timeout-artifacts";
  STPSolver Solver(Options);
  setSTPSolverWorkerTestMode(23);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::Timeout) << Result.Metadata.Detail;
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::Timeout);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);
  EXPECT_NE(Result.Metadata.Detail.find("worker was killed"), std::string::npos);
  EXPECT_TRUE(std::filesystem::is_regular_file(Result.Metadata.SavedQuery));
}

TEST(FormalSolver, WorkerCompletionAtDeadlineCannotBeAccepted) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "deadline_completion_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory = "m2-completion-timeout-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(22);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::Timeout) << Result.Metadata.Detail;
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::Timeout);
  EXPECT_NE(Result.Status, SolverStatus::Sat);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);
  EXPECT_NE(Result.Metadata.Detail.find("worker completed"), std::string::npos);
  EXPECT_GE(Result.Metadata.Elapsed, Options.Timeout);
}

TEST(FormalSolver, FiniteHardDeadlineRejectsUncancellableParentAudit) {
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(500);
  Options.AdditionalModelAudit =
      [](const ConstraintSet &, const Model &) -> std::optional<std::string> {
    return std::nullopt;
  };
  EXPECT_THROW((void)STPSolver(Options), std::invalid_argument);
}

TEST(FormalSolver, AuditExceptionIsInternalWithExplicitUnlimitedTimeout) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "throwing_model_audit_x");
  STPSolverOptions Options;
  Options.Timeout = STPSolverOptions::UnlimitedTimeout;
  Options.FailureQueryDirectory = "m2-model-audit-failure-artifacts";
  Options.AdditionalModelAudit =
      [](const ConstraintSet &, const Model &) -> std::optional<std::string> {
    throw std::runtime_error("injected audit exception");
  };
  STPSolver Solver(Options);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::ModelValidationFailed);
  EXPECT_NE(Result.Metadata.Detail.find("injected audit exception"),
            std::string::npos);
  EXPECT_NE(Result.Status, SolverStatus::Timeout);
  EXPECT_TRUE(std::filesystem::is_regular_file(Result.Metadata.SavedQuery));
}

TEST(FormalSolver, MalformedResponseCannotBeMaskedByExpiredDeadline) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "late_malformed_response_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory = "m2-late-malformed-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(24);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::BackendFailure);
  EXPECT_NE(Result.Status, SolverStatus::Timeout);
  EXPECT_GE(Result.Metadata.Elapsed, Options.Timeout);
}

TEST(FormalSolver, InvalidModelCannotBeMaskedByExpiredDeadline) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "late_invalid_model_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory = "m2-late-model-failure-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(25);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 0x5a))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::ModelValidationFailed);
  EXPECT_NE(Result.Status, SolverStatus::Timeout);
  EXPECT_GE(Result.Metadata.Elapsed, Options.Timeout);
}

TEST(FormalSolver, InvalidModelDetectedAfterDeadlineIsStillInternal) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "prevalidation_late_invalid_model_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory =
      "m2-prevalidation-late-model-failure-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(26);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 0x5a))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::ModelValidationFailed);
  EXPECT_NE(Result.Status, SolverStatus::Timeout);
  EXPECT_GE(Result.Metadata.Elapsed, Options.Timeout);
}

TEST(FormalSolver, WorkerInternalErrorAtDeadlineIsStillInternal) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "late_worker_internal_error_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory = "m2-late-worker-internal-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(27);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 0x5a))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::TranslationError);
  EXPECT_NE(Result.Status, SolverStatus::Timeout);
  EXPECT_GE(Result.Metadata.Elapsed, Options.Timeout);
}

TEST(FormalSolver, CompletedWorkerInternalErrorSurvivesLateDeadlinePoll) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "late_poll_worker_internal_error_x");
  STPSolverOptions Options;
  Options.Timeout = std::chrono::milliseconds(250);
  Options.FailureQueryDirectory = "m2-late-poll-worker-internal-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(28);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 0x5a))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::TranslationError);
  EXPECT_NE(Result.Status, SolverStatus::Timeout);
  EXPECT_GE(Result.Metadata.Elapsed, Options.Timeout);
}

TEST(FormalSolver, HugeFiniteTimeoutUsesASaturatingDeadline) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "huge_timeout_x");
  STPSolverOptions Options;
  Options.Timeout = STPSolverOptions::UnlimitedTimeout -
                    std::chrono::milliseconds(1);
  STPSolver Solver(Options);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 7))});
  EXPECT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
}

TEST(FormalSolver, UnknownBackendErrorExceptionAndWorkerCrashFailClosed) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "backend_failure_x");
  ConstraintSet Constraints{Context.eq(X, Context.constant(8, 1))};
  STPSolver Solver = makeSolver();

  setSTPSolverWorkerTestMode(1);
  SolverResult Unknown = Solver.check(Constraints);
  EXPECT_EQ(Unknown.Status, SolverStatus::Unknown);
  EXPECT_EQ(Unknown.Metadata.Reason, SolverReason::BackendUnknown);
  EXPECT_NE(Unknown.Status, SolverStatus::Unsat);

  setSTPSolverWorkerTestMode(3);
  SolverResult Error = Solver.check(Constraints);
  EXPECT_EQ(Error.Status, SolverStatus::InternalError);
  EXPECT_EQ(Error.Metadata.Reason, SolverReason::BackendFailure);
  EXPECT_NE(Error.Status, SolverStatus::Unsat);

  setSTPSolverWorkerTestMode(4);
  SolverResult Exception = Solver.check(Constraints);
  EXPECT_EQ(Exception.Status, SolverStatus::InternalError);
  EXPECT_EQ(Exception.Metadata.Reason, SolverReason::BackendFailure);
  EXPECT_NE(Exception.Metadata.Detail.find("exception"), std::string::npos);

  setSTPSolverWorkerTestMode(2);
  SolverResult Crash = Solver.check(Constraints);
  EXPECT_EQ(Crash.Status, SolverStatus::Unknown);
  EXPECT_EQ(Crash.Metadata.Reason, SolverReason::WorkerFailure);
  EXPECT_STREQ(toReasonCode(Crash.Metadata.Reason),
               "UNKNOWN-SOLVER-FAILURE");
  EXPECT_NE(Crash.Metadata.Detail.find("worker process terminated"),
            std::string::npos);
  EXPECT_NE(Crash.Status, SolverStatus::Unsat);
  setSTPSolverWorkerTestMode(0);
}

TEST(FormalSolver, WorkerInternalExitCodesRemainInternalErrors) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "worker_internal_exit_x");
  ConstraintSet Constraints{Context.eq(X, Context.constant(8, 1))};
  STPSolver Solver = makeSolver();

  for (const auto &[Mode, ExitCode] :
       std::array<std::pair<int, int>, 3>{{{29, 123}, {30, 124}, {31, 125}}}) {
    setSTPSolverWorkerTestMode(Mode);
    SolverResult Result = Solver.check(Constraints);
    EXPECT_EQ(Result.Status, SolverStatus::InternalError) << Mode;
    EXPECT_EQ(Result.Metadata.Reason, SolverReason::BackendFailure) << Mode;
    EXPECT_NE(Result.Metadata.Detail.find("exit code " +
                                          std::to_string(ExitCode)),
              std::string::npos)
        << Mode;
    EXPECT_NE(Result.Status, SolverStatus::Unknown) << Mode;
    EXPECT_NE(Result.Status, SolverStatus::Unsat) << Mode;
  }
  setSTPSolverWorkerTestMode(0);
}

TEST(FormalSolver, MalformedWorkerEnvelopesAndAPIntsFailClosed) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "malformed_wire_x");
  ConstraintSet Constraints{Context.eq(X, Context.constant(8, 1))};
  STPSolver Solver = makeSolver();

  for (int Mode : {5, 6, 8, 11, 12, 13, 14, 15, 16, 17, 18, 19}) {
    setSTPSolverWorkerTestMode(Mode);
    SolverResult Result = Solver.check(Constraints);
    EXPECT_EQ(Result.Status, SolverStatus::InternalError) << Mode;
    EXPECT_EQ(Result.Metadata.Reason, SolverReason::BackendFailure) << Mode;
    EXPECT_NE(Result.Status, SolverStatus::Unsat) << Mode;
  }
  setSTPSolverWorkerTestMode(0);
}

TEST(FormalSolver, WorkerWireAndFileLimitsFailClosed) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "bounded_worker_x");
  STPSolverOptions Options;
  Options.MaxWorkerResponseBytes = 64;
  Options.MaxWorkerFileBytes = 64;
  Options.FailureQueryDirectory = "m2-worker-limit-artifacts";
  STPSolver Solver(Options);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::BackendFailure);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);

  STPSolverOptions Invalid;
  Invalid.MaxWorkerResponseBytes = 1024;
  Invalid.MaxWorkerFileBytes = 512;
  EXPECT_THROW(STPSolver Rejected(Invalid), std::invalid_argument);
}

TEST(FormalSolver, WorkerResourceTerminationIsUnknown) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "resource_terminated_worker_x");
  STPSolverOptions Options;
  Options.MaxWorkerResponseBytes = 4096;
  Options.MaxWorkerFileBytes = 4096;
  Options.FailureQueryDirectory = "m2-worker-resource-artifacts";
  STPSolver Solver(Options);

  setSTPSolverWorkerTestMode(20);
  SolverResult Result =
      Solver.check({Context.eq(X, Context.constant(8, 1))});
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::Unknown);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::WorkerFailure);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);
}

TEST(FormalSolver, RejectsSemanticallyCorruptScalarModelFromValidWire) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "valid_wire_corrupt_model_x");
  ConstraintSet Constraints{Context.eq(X, Context.constant(8, 0x5a))};
  STPSolver Solver = makeSolver();

  setSTPSolverWorkerTestMode(21);
  SolverResult Result = Solver.check(Constraints);
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::ModelValidationFailed);
  EXPECT_NE(Result.Metadata.Detail.find("false"), std::string::npos);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);
}

TEST(FormalSolver, RejectsScalarModelMissingOnlyAnInactiveSelectBranch) {
  ExprContext Context;
  ExprRef Flag = Context.booleanSymbol("model_a_flag");
  ExprRef Active = Context.symbol(8, "model_b_active");
  ExprRef Inactive = Context.symbol(8, "model_z_inactive");
  ConstraintSet Constraints{
      Flag,
      Context.eq(Context.select(Flag, Active, Inactive),
                 Context.constant(8, 0x5a)),
  };
  STPSolver Solver = makeSolver();

  setSTPSolverWorkerTestMode(32);
  SolverResult Result = Solver.check(Constraints);
  setSTPSolverWorkerTestMode(0);
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::BackendFailure);
  EXPECT_NE(Result.Metadata.Detail.find("malformed"), std::string::npos);
  EXPECT_NE(Result.Status, SolverStatus::Sat);
  EXPECT_NE(Result.Status, SolverStatus::Unsat);
}

TEST(FormalSolver, RejectsAmbiguousArrayIdentityAcrossExpressionContexts) {
  ExprContext FirstContext;
  ExprContext SecondContext;
  ArrayRef First = FirstContext.array(8, "shared_backend_array");
  ArrayRef Second = SecondContext.array(8, "shared_backend_array");
  ExprRef FirstRead = FirstContext.read(
      FirstContext.updates(First), FirstContext.constant(8, 0));
  ConstraintSet Constraints{
      FirstContext.eq(FirstRead, FirstContext.constant(8, 0xa5))};

  STPSolver Solver = makeSolver();
  SolverResult Result = Solver.getInitialValues(Constraints, {{Second, 1}});
  EXPECT_EQ(Result.Status, SolverStatus::InternalError);
  EXPECT_EQ(Result.Metadata.Reason, SolverReason::InvalidQuery);
  EXPECT_NE(Result.Metadata.Detail.find("distinct arrays"), std::string::npos);
  EXPECT_TRUE(Result.InitialValues.empty());
}

TEST(FormalSolver, PreservesInitialArrayIdentityAndRequestOrder) {
  ExprContext Context;
  ArrayRef First = Context.array(8, "ordered_array_first");
  ArrayRef Second = Context.array(8, "ordered_array_second");
  UpdateList FirstBase = Context.updates(First);
  UpdateList SecondBase = Context.updates(Second);
  ConstraintSet Constraints{
      Context.eq(Context.read(FirstBase, Context.constant(8, 0)),
                 Context.constant(8, 0xa5)),
      Context.eq(Context.read(SecondBase, Context.constant(8, 0)),
                 Context.constant(8, 0x5a))};

  STPSolver Solver = makeSolver();
  SolverResult Result =
      Solver.getInitialValues(Constraints, {{Second, 1}, {First, 1}});
  ASSERT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
  ASSERT_EQ(Result.InitialValues.size(), 2U);
  EXPECT_EQ(Result.InitialValues[0].Root.get(), Second.get());
  EXPECT_EQ(Result.InitialValues[0].Bytes,
            (std::vector<std::uint8_t>{0x5a}));
  EXPECT_EQ(Result.InitialValues[1].Root.get(), First.get());
  EXPECT_EQ(Result.InitialValues[1].Bytes,
            (std::vector<std::uint8_t>{0xa5}));
}

TEST(FormalSolver, PreservesDuplicateConstraintsAndArrayRequests) {
  ExprContext Context;
  ExprRef Flag = Context.booleanSymbol("duplicate_request_flag");
  ArrayRef Bytes = Context.array(8, "duplicate_request_bytes");
  UpdateList Base = Context.updates(Bytes);
  ExprRef ByteZero = Context.read(Base, Context.constant(8, 0));
  ConstraintSet Constraints{
      Flag, Flag, Flag,
      Context.eq(ByteZero, Context.constant(8, 0x3c))};

  STPSolver Solver = makeSolver();
  SolverResult Result =
      Solver.getInitialValues(Constraints, {{Bytes, 1}, {Bytes, 2}});
  ASSERT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
  ASSERT_EQ(Result.InitialValues.size(), 2U);
  EXPECT_EQ(Result.InitialValues[0].Root.get(), Bytes.get());
  EXPECT_EQ(Result.InitialValues[0].Bytes,
            (std::vector<std::uint8_t>{0x3c}));
  EXPECT_EQ(Result.InitialValues[1].Root.get(), Bytes.get());
  ASSERT_EQ(Result.InitialValues[1].Bytes.size(), 2U);
  EXPECT_EQ(Result.InitialValues[1].Bytes[0], 0x3c);
}

TEST(FormalSolver, RejectsCorruptInitialValuePayloads) {
  ExprContext FirstContext;
  ExprContext SecondContext;
  ArrayRef First = FirstContext.array(8, "corrupt_initial_array_first");
  ArrayRef Second = SecondContext.array(8, "corrupt_initial_array_second");
  ExprRef FirstRead = FirstContext.read(
      FirstContext.updates(First), FirstContext.constant(8, 0));
  ConstraintSet Constraints{
      FirstContext.eq(FirstRead, FirstContext.constant(8, 0x5a))};
  STPSolver Solver = makeSolver();

  for (int Mode : {7, 9, 10}) {
    setSTPSolverWorkerTestMode(Mode);
    SolverResult Result =
        Solver.getInitialValues(Constraints, {{Second, 1}});
    EXPECT_EQ(Result.Status, SolverStatus::InternalError) << Mode;
    EXPECT_NE(Result.Status, SolverStatus::Unsat) << Mode;
  }
  setSTPSolverWorkerTestMode(0);
}

TEST(FormalSolver, ConcurrentWorkerQueriesDoNotDeadlock) {
  constexpr unsigned ThreadCount = 8;
  constexpr unsigned QueriesPerThread = 8;
  std::atomic<unsigned> Failures{0};
  std::vector<std::thread> Threads;
  Threads.reserve(ThreadCount);
  for (unsigned Thread = 0; Thread < ThreadCount; ++Thread) {
    Threads.emplace_back([Thread, &Failures] {
      ExprContext Context;
      ExprRef X = Context.symbol(16, "concurrent_x_" + std::to_string(Thread));
      ExprRef Constraint =
          Context.eq(X, Context.constant(16, Thread + 1));
      STPSolver Solver = makeSolver();
      for (unsigned Query = 0; Query < QueriesPerThread; ++Query) {
        SolverResult Result = Solver.getValue(
            {Constraint}, Context.add(X, Context.constant(16, Query)));
        if (Result.Status != SolverStatus::Sat || !Result.Value ||
            *Result.Value != llvm::APInt(16, Thread + Query + 1))
          ++Failures;
      }
    });
  }
  for (std::thread &Thread : Threads)
    Thread.join();
  EXPECT_EQ(Failures.load(), 0U);
}

TEST(FormalSolver, QueryExportContainsAuditableMetadataAndTarget) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "export_x");
  STPSolver Solver = makeSolver(true);
  SolverResult Result =
      Solver.mayBeTrue({}, Context.eq(X, Context.constant(8, 7)));
  ASSERT_EQ(Result.Status, SolverStatus::Sat);
  ASSERT_TRUE(std::filesystem::is_regular_file(Result.Metadata.SavedQuery));
  std::ifstream Input(Result.Metadata.SavedQuery);
  std::string Text((std::istreambuf_iterator<char>(Input)),
                   std::istreambuf_iterator<char>());
  EXPECT_NE(Text.find("; backend: STP"), std::string::npos);
  EXPECT_NE(Text.find("; backend-version: 2.3.3"), std::string::npos);
  EXPECT_NE(Text.find("; sat-backend: MiniSat"), std::string::npos);
  EXPECT_NE(Text.find("; timeout-ms:"), std::string::npos);
  EXPECT_NE(Text.find("; query-target: assert"), std::string::npos);
  EXPECT_NE(Text.find("(check-sat)"), std::string::npos);
}

TEST(FormalSolver, FailedInitialValueExportRecordsOrderedArrayRequests) {
  ExprContext Context;
  ArrayRef First = Context.array(8, "request_artifact_first");
  ArrayRef Second = Context.array(16, "request_artifact_second");
  STPSolver Solver = makeSolver();

  setSTPSolverWorkerTestMode(1);
  SolverResult Result =
      Solver.getInitialValues({}, {{Second, 2}, {First, 1}});
  setSTPSolverWorkerTestMode(0);
  ASSERT_EQ(Result.Status, SolverStatus::Unknown) << Result.Metadata.Detail;
  ASSERT_TRUE(std::filesystem::is_regular_file(Result.Metadata.SavedQuery));
  std::ifstream Input(Result.Metadata.SavedQuery);
  std::string Text((std::istreambuf_iterator<char>(Input)),
                   std::istreambuf_iterator<char>());
  std::size_t SecondRequest = Text.find(
      "; array-request-0: name=request_artifact_second index-width=16 "
      "byte-count=2");
  std::size_t FirstRequest = Text.find(
      "; array-request-1: name=request_artifact_first index-width=8 "
      "byte-count=1");
  EXPECT_NE(SecondRequest, std::string::npos);
  EXPECT_NE(FirstRequest, std::string::npos);
  EXPECT_LT(SecondRequest, FirstRequest);
  EXPECT_NE(Text.find("(declare-fun |request_artifact_first| () "
                      "(Array (_ BitVec 8) (_ BitVec 8)))"),
            std::string::npos);
  EXPECT_NE(Text.find("(declare-fun |request_artifact_second| () "
                      "(Array (_ BitVec 16) (_ BitVec 8)))"),
            std::string::npos);
}

TEST(FormalSolver, GetValueExportContainsTheValueTargetTerm) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "value_target_x");
  STPSolver Solver = makeSolver(true);
  SolverResult Result = Solver.getValue({}, Context.add(X, Context.constant(8, 1)));
  ASSERT_EQ(Result.Status, SolverStatus::Sat) << Result.Metadata.Detail;
  std::ifstream Input(Result.Metadata.SavedQuery);
  std::string Text((std::istreambuf_iterator<char>(Input)),
                   std::istreambuf_iterator<char>());
  EXPECT_NE(Text.find("; query-target: value-only"), std::string::npos);
  EXPECT_NE(Text.find("(define-fun |__formal_query_target|"),
            std::string::npos);
  EXPECT_NE(Text.find("(bvadd |value_target_x| (_ bv1 8))"),
            std::string::npos);
}

} // namespace
