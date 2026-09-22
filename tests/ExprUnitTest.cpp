#include "formal/Expr.h"
#include "formal/SMTLibWriter.h"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "gtest/gtest.h"

#include <algorithm>
#include <array>
#include <limits>
#include <optional>
#include <random>
#include <string>
#include <type_traits>

using namespace formal;

namespace {

TEST(FormalSort, ExplicitBoolBitVectorAndArraySorts) {
  EXPECT_TRUE(Sort::boolean().isBool());
  EXPECT_EQ(Sort::bitVector(32).bitWidth(), 32U);
  EXPECT_EQ(Sort::byteArray(64).indexWidth(), 64U);
  EXPECT_EQ(Sort::byteArray(64).rangeWidth(), 8U);
  EXPECT_THROW(Sort::bitVector(0), std::invalid_argument);
  EXPECT_THROW(Sort::byteArray(0), std::invalid_argument);
  EXPECT_THROW(Sort::boolean().bitWidth(), std::logic_error);
  EXPECT_THROW(Sort::bitVector(8).indexWidth(), std::logic_error);
}

TEST(FormalExpr, IsImmutableAndHashConsed) {
  static_assert(!std::is_copy_assignable_v<Expr>);
  ExprContext Context;
  ExprRef X = Context.symbol(8, "x");
  ExprRef Y = Context.symbol(8, "y");
  EXPECT_EQ(X, Context.symbol(8, "x"));
  EXPECT_EQ(Context.add(X, Y), Context.add(X, Y));
  EXPECT_NE(Context.add(X, Y), Context.sub(X, Y));
  EXPECT_EQ(Context.constant(llvm::APInt(8, 7)), Context.constant(8, 7));
}

TEST(FormalExpr, RejectsMalformedSortsWidthsNamesAndContexts) {
  ExprContext Context;
  ExprContext Other;
  ExprRef X8 = Context.symbol(8, "x");
  ExprRef X16 = Context.symbol(16, "x16");
  EXPECT_THROW(Context.constant(0, 0), std::invalid_argument);
  EXPECT_THROW(Context.add(X8, X16), std::invalid_argument);
  EXPECT_THROW(Context.add(X8, Other.symbol(8, "other")),
               std::invalid_argument);
  EXPECT_THROW(Context.logicalNot(X8), std::invalid_argument);
  EXPECT_THROW(Context.select(Context.boolean(true), X8, X16),
               std::invalid_argument);
  EXPECT_THROW(Context.extract(X8, 7, 2), std::invalid_argument);
  EXPECT_THROW(Context.zext(X16, 8), std::invalid_argument);
  EXPECT_THROW(Context.sext(X16, 8), std::invalid_argument);
  EXPECT_THROW(Context.trunc(X8, 16), std::invalid_argument);
  EXPECT_THROW(Context.symbol(8, "bad name"), std::invalid_argument);
  EXPECT_THROW(Context.array(32, "bad|array"), std::invalid_argument);
  EXPECT_THROW(Context.booleanSymbol("x"), std::invalid_argument);
  EXPECT_THROW(Context.symbol(8, std::string(1025, 'x')), std::length_error);
  EXPECT_THROW(Context.add(nullptr, X8), std::invalid_argument);
  EXPECT_THROW(Context.concat(Context.boolean(true), X8),
               std::invalid_argument);
  EXPECT_THROW(Context.select(X8, X8, X8), std::invalid_argument);
}

TEST(FormalExpr, CoversEveryScalarNodeAndResultSort) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "x");
  ExprRef Y = Context.symbol(8, "y");
  const std::array<ExprRef, 13> Binary{
      Context.add(X, Y),    Context.sub(X, Y),    Context.mul(X, Y),
      Context.udiv(X, Y),   Context.sdiv(X, Y),   Context.urem(X, Y),
      Context.srem(X, Y),   Context.bitAnd(X, Y), Context.bitOr(X, Y),
      Context.bitXor(X, Y), Context.shl(X, Y),    Context.lshr(X, Y),
      Context.ashr(X, Y),
  };
  for (const ExprRef &Expression : Binary)
    EXPECT_EQ(Expression->sort(), Sort::bitVector(8));

  const std::array<ExprRef, 5> Comparisons{
      Context.eq(X, Y),  Context.ult(X, Y), Context.ule(X, Y),
      Context.slt(X, Y), Context.sle(X, Y),
  };
  for (const ExprRef &Expression : Comparisons)
    EXPECT_TRUE(Expression->sort().isBool());

  ExprRef Condition = Context.booleanSymbol("condition");
  EXPECT_TRUE(Context.logicalNot(Condition)->sort().isBool());
  EXPECT_EQ(Context.select(Condition, X, Y)->sort(), Sort::bitVector(8));
  EXPECT_EQ(Context.concat(X, Y)->width(), 16U);
  EXPECT_EQ(Context.extract(Context.concat(X, Y), 3, 9)->width(), 9U);
  EXPECT_EQ(Context.zext(X, 16)->width(), 16U);
  EXPECT_EQ(Context.sext(X, 16)->width(), 16U);
  EXPECT_EQ(Context.trunc(Context.zext(X, 16), 4)->width(), 4U);
  EXPECT_TRUE(Context.toBool(Context.symbol(1, "i1"))->sort().isBool());
}

TEST(FormalExpr, APIntConstantFoldingAtRequiredWidths) {
  ExprContext Context;
  for (unsigned Width : {1U, 8U, 32U, 65U, 128U}) {
    llvm::APInt Max = llvm::APInt::getAllOnes(Width);
    ExprRef MaxValue = Context.constant(Max);
    ExprRef One = Context.constant(llvm::APInt(Width, 1));
    EXPECT_TRUE(Context.add(MaxValue, One)->constantValue().isZero());
    EXPECT_EQ(Context.sub(Context.constant(llvm::APInt(Width, 0)), One)
                  ->constantValue(),
              Max);
    EXPECT_EQ(Context.bitAnd(MaxValue, One)->constantValue(),
              llvm::APInt(Width, 1));
    EXPECT_EQ(Context.udiv(MaxValue, One)->constantValue(), Max);
    EXPECT_TRUE(Context.urem(MaxValue, One)->constantValue().isZero());
    EXPECT_TRUE(Context.eq(MaxValue, Context.constant(Max))
                    ->constantValue()
                    .getBoolValue());
  }
  EXPECT_EQ(Context.sext(Context.constant(8, 0x80), 16)->constantValue(),
            llvm::APInt(16, 0xff80));
  EXPECT_EQ(Context.zext(Context.constant(8, 0x80), 16)->constantValue(),
            llvm::APInt(16, 0x80));
  EXPECT_EQ(Context.trunc(Context.constant(16, 0x1234), 8)->constantValue(),
            llvm::APInt(8, 0x34));
  EXPECT_EQ(
      Context.concat(Context.constant(8, 0x12), Context.constant(8, 0x34))
          ->constantValue(),
      llvm::APInt(16, 0x1234));
}

TEST(FormalExpr, ExtractConcatBoundariesAndReconstruction) {
  ExprContext Context;
  ExprRef X = Context.symbol(16, "x16");
  EXPECT_EQ(Context.extract(X, 0, 1)->offset(), 0U);
  EXPECT_EQ(Context.extract(X, 15, 1)->offset(), 15U);
  EXPECT_EQ(Context.extract(X, 0, 16), X);

  ExprRef Value = Context.constant(16, 0xabcd);
  ExprRef Reconstructed = Context.concat(Context.extract(Value, 8, 8),
                                         Context.extract(Value, 0, 8));
  EXPECT_EQ(Reconstructed->constantValue(), llvm::APInt(16, 0xabcd));
}

TEST(FormalExpr, SignedUnsignedComparisonsDiffer) {
  ExprContext Context;
  ExprRef MinusOne = Context.constant(8, 255);
  ExprRef One = Context.constant(8, 1);
  EXPECT_FALSE(Context.ult(MinusOne, One)->constantValue().getBoolValue());
  EXPECT_TRUE(Context.slt(MinusOne, One)->constantValue().getBoolValue());
  EXPECT_TRUE(Context.ule(One, One)->constantValue().getBoolValue());
  EXPECT_TRUE(Context.sle(One, One)->constantValue().getBoolValue());
}

TEST(FormalExpr, BoundedLocalSimplification) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "x");
  ExprRef C = Context.booleanSymbol("c");
  EXPECT_EQ(Context.add(X, Context.constant(8, 0)), X);
  EXPECT_EQ(Context.sub(X, X)->constantValue(), llvm::APInt(8, 0));
  EXPECT_EQ(Context.bitXor(X, X)->constantValue(), llvm::APInt(8, 0));
  EXPECT_EQ(Context.select(Context.boolean(true), X, Context.constant(8, 2)),
            X);
  EXPECT_EQ(Context.select(C, X, X), X);
  EXPECT_EQ(Context.logicalNot(Context.logicalNot(C)), C);
}

TEST(FormalArray, PersistentReadWriteAndStructuralSharing) {
  ExprContext Context;
  ArrayRef Bytes = Context.array(32, "bytes");
  EXPECT_EQ(Bytes, Context.array(32, "bytes"));
  UpdateList Empty = Context.updates(Bytes);
  ExprRef I = Context.symbol(32, "i");
  ExprRef J = Context.symbol(32, "j");
  ExprRef Eleven = Context.constant(8, 11);
  ExprRef TwentyTwo = Context.constant(8, 22);

  UpdateList First = Context.write(Empty, I, Eleven);
  UpdateList Second = Context.write(First, J, TwentyTwo);
  EXPECT_EQ(Empty.size(), 0U);
  EXPECT_EQ(First.size(), 1U);
  EXPECT_EQ(Second.size(), 2U);
  EXPECT_EQ(Context.read(First, I), Eleven);
  EXPECT_EQ(Context.read(Second, J), TwentyTwo);
  EXPECT_EQ(Context.read(First, I), Context.read(First, I));
  EXPECT_EQ(Context.write(Empty, I, Eleven).head(), First.head());

  ExprRef OldRead = Context.read(Empty, I);
  EXPECT_EQ(OldRead->kind(), ExprKind::Read);
  EXPECT_NE(OldRead, Context.read(First, I));
}

TEST(FormalArray, ConcreteUnequalWriteIsSkippedAndSortsAreChecked) {
  ExprContext Context;
  ArrayRef Bytes = Context.array(16, "bytes");
  UpdateList Updates =
      Context.write(Context.updates(Bytes), Context.constant(16, 1),
                    Context.constant(8, 0xaa));
  ExprRef Read = Context.read(Updates, Context.constant(16, 2));
  EXPECT_EQ(Read->kind(), ExprKind::Read);
  EXPECT_EQ(Read->updates().size(), 0U);
  EXPECT_THROW(Context.read(Updates, Context.constant(8, 2)),
               std::invalid_argument);
  EXPECT_THROW(
      Context.write(Updates, Context.constant(16, 2), Context.constant(16, 3)),
      std::invalid_argument);
}

TEST(FormalArray, ForkedUpdateListsRemainIsolated) {
  ExprContext Context;
  UpdateList Base = Context.updates(Context.array(8, "forked_bytes"));
  ExprRef Index = Context.symbol(8, "fork_index");
  UpdateList Left = Context.write(Base, Index, Context.constant(8, 0x11));
  UpdateList Right = Context.write(Base, Index, Context.constant(8, 0x22));

  EXPECT_EQ(Base.size(), 0U);
  EXPECT_EQ(Context.read(Left, Index)->constantValue(), llvm::APInt(8, 0x11));
  EXPECT_EQ(Context.read(Right, Index)->constantValue(), llvm::APInt(8, 0x22));
  EXPECT_NE(Left.head(), Right.head());
}

TEST(FormalArray, ArrayCountIsHardLimitedBeforeAllocation) {
  ExprContext Context({32, 32, 128, 128, 2});
  ArrayRef First = Context.array(8, "first");
  ArrayRef Second = Context.array(8, "second");
  EXPECT_EQ(Context.arrayCount(), 2U);
  EXPECT_EQ(Context.array(8, "first"), First);
  EXPECT_EQ(Context.array(8, "second"), Second);
  EXPECT_THROW(Context.array(8, "third"), std::length_error);
  EXPECT_EQ(Context.arrayCount(), 2U);
}

TEST(FormalExpr, ReferencesSafelyOutliveTheirCreatingContext) {
  ExprRef Expression;
  ArrayRef Bytes;
  std::optional<UpdateList> Updates;
  {
    ExprContext Context;
    Expression = Context.add(Context.symbol(8, "x"), Context.constant(8, 1));
    Bytes = Context.array(32, "bytes");
    Updates = Context.write(Context.updates(Bytes), Context.constant(32, 3),
                            Context.constant(8, 7));
  }

  ASSERT_TRUE(Expression);
  ASSERT_TRUE(Bytes);
  ASSERT_TRUE(Updates.has_value());
  EXPECT_EQ(Expression->kind(), ExprKind::Add);
  EXPECT_EQ(Expression->operand(0)->symbolName(), "x");
  EXPECT_EQ(Bytes->name(), "bytes");
  ASSERT_TRUE(Updates->head());
  EXPECT_EQ(Updates->head()->value()->constantValue(), llvm::APInt(8, 7));

  ExprContext OtherContext;
  EXPECT_THROW(OtherContext.add(Expression, OtherContext.constant(8, 1)),
               std::invalid_argument);
  EXPECT_THROW(OtherContext.updates(Bytes), std::invalid_argument);
  EXPECT_THROW(OtherContext.read(*Updates, OtherContext.constant(32, 3)),
               std::invalid_argument);
}

TEST(FormalExpr, ResourceLimitsFailClosed) {
  ExprContext WidthLimited({8, 16, 32, 32});
  EXPECT_THROW(WidthLimited.symbol(9, "wide"), std::length_error);
  EXPECT_THROW(WidthLimited.constant(9, 0), std::length_error);
  EXPECT_THROW(WidthLimited.signedConstant(9, -1), std::length_error);
  EXPECT_THROW(
      WidthLimited.constant(std::numeric_limits<unsigned>::max(), 0),
      std::length_error);
  EXPECT_THROW(
      WidthLimited.signedConstant(std::numeric_limits<unsigned>::max(), -1),
      std::length_error);

  ExprContext DepthLimited({32, 2, 32, 32});
  ExprRef X = DepthLimited.symbol(8, "x");
  ExprRef Y = DepthLimited.symbol(8, "y");
  EXPECT_THROW(DepthLimited.add(DepthLimited.add(X, Y), X), std::length_error);

  ExprContext NodeLimited({32, 32, 2, 32});
  NodeLimited.symbol(8, "a");
  NodeLimited.symbol(8, "b");
  EXPECT_THROW(NodeLimited.symbol(8, "c"), std::length_error);

  ExprContext UpdateLimited({32, 32, 32, 1});
  UpdateList Empty = UpdateLimited.updates(UpdateLimited.array(8, "limited"));
  UpdateList First = UpdateLimited.write(Empty, UpdateLimited.constant(8, 0),
                                         UpdateLimited.constant(8, 1));
  EXPECT_THROW(UpdateLimited.write(First, UpdateLimited.constant(8, 1),
                                   UpdateLimited.constant(8, 2)),
               std::length_error);
}

std::string stableSMT(bool ReverseConstruction) {
  ExprContext Context;
  ExprRef Y;
  ExprRef X;
  if (ReverseConstruction) {
    Y = Context.symbol(8, "y");
    X = Context.symbol(8, "x");
  } else {
    X = Context.symbol(8, "x");
    Y = Context.symbol(8, "y");
  }
  ExprRef C = Context.booleanSymbol("condition");
  return SMTLibWriter::write(
      Context.select(C, Context.add(X, Context.constant(8, 1)), Y));
}

TEST(FormalSMTLib, StableAcrossConstructionOrder) {
  const std::string Expected =
      "(set-logic QF_BV)\n"
      "(declare-fun |condition| () Bool)\n"
      "(declare-fun |x| () (_ BitVec 8))\n"
      "(declare-fun |y| () (_ BitVec 8))\n"
      "(define-fun |result| () (_ BitVec 8) (ite |condition| "
      "(bvadd |x| (_ bv1 8)) |y|))\n"
      "(check-sat)\n";
  EXPECT_EQ(stableSMT(false), Expected);
  EXPECT_EQ(stableSMT(true), Expected);

  ExprContext Context;
  EXPECT_THROW(SMTLibWriter::write(Context.symbol(8, "value"), "bad|name"),
               std::invalid_argument);
}

TEST(FormalSMTLib, EmitsPersistentArrayUpdatesInProgramOrder) {
  ExprContext Context;
  ArrayRef Bytes = Context.array(32, "bytes");
  UpdateList Updates = Context.updates(Bytes);
  Updates =
      Context.write(Updates, Context.symbol(32, "i"), Context.constant(8, 1));
  Updates =
      Context.write(Updates, Context.symbol(32, "j"), Context.constant(8, 2));
  std::string Text =
      SMTLibWriter::write(Context.read(Updates, Context.symbol(32, "k")));
  EXPECT_NE(Text.find("(set-logic QF_AUFBV)"), std::string::npos);
  EXPECT_NE(Text.find("(store (store |bytes| |i| (_ bv1 8)) |j| (_ bv2 8))"),
            std::string::npos);
}

TEST(FormalSMTLib, EmitsEveryScalarNodeKind) {
  ExprContext Context;
  ExprRef X = Context.symbol(8, "x");
  ExprRef Y = Context.symbol(8, "y");
  const std::array<std::pair<ExprRef, const char *>, 13> Binary{{
      {Context.add(X, Y), "(bvadd "},
      {Context.sub(X, Y), "(bvsub "},
      {Context.mul(X, Y), "(bvmul "},
      {Context.udiv(X, Y), "(bvudiv "},
      {Context.sdiv(X, Y), "(bvsdiv "},
      {Context.urem(X, Y), "(bvurem "},
      {Context.srem(X, Y), "(bvsrem "},
      {Context.bitAnd(X, Y), "(bvand "},
      {Context.bitOr(X, Y), "(bvor "},
      {Context.bitXor(X, Y), "(bvxor "},
      {Context.shl(X, Y), "(bvshl "},
      {Context.lshr(X, Y), "(bvlshr "},
      {Context.ashr(X, Y), "(bvashr "},
  }};
  for (const auto &[Expression, Token] : Binary)
    EXPECT_NE(SMTLibWriter::write(Expression).find(Token), std::string::npos);

  const std::array<std::pair<ExprRef, const char *>, 5> Comparisons{{
      {Context.eq(X, Y), "(= "},
      {Context.ult(X, Y), "(bvult "},
      {Context.ule(X, Y), "(bvule "},
      {Context.slt(X, Y), "(bvslt "},
      {Context.sle(X, Y), "(bvsle "},
  }};
  for (const auto &[Expression, Token] : Comparisons)
    EXPECT_NE(SMTLibWriter::write(Expression).find(Token), std::string::npos);

  ExprRef Condition = Context.booleanSymbol("condition");
  EXPECT_NE(SMTLibWriter::write(Context.logicalNot(Condition)).find("(not "),
            std::string::npos);
  EXPECT_NE(SMTLibWriter::write(Context.select(Condition, X, Y)).find("(ite "),
            std::string::npos);
  EXPECT_NE(SMTLibWriter::write(Context.concat(X, Y)).find("(concat "),
            std::string::npos);
  EXPECT_NE(SMTLibWriter::write(Context.extract(X, 1, 6)).find("((_ extract "),
            std::string::npos);
  EXPECT_NE(SMTLibWriter::write(Context.zext(X, 16)).find("((_ zero_extend "),
            std::string::npos);
  EXPECT_NE(SMTLibWriter::write(Context.sext(X, 16)).find("((_ sign_extend "),
            std::string::npos);
  EXPECT_NE(SMTLibWriter::write(Context.trunc(Context.symbol(16, "wide"), 8))
                .find("((_ extract 7 0)"),
            std::string::npos);
}

TEST(FormalSMTLib, ResourceBudgetsRejectExpansionAndOversizedOutput) {
  ExprContext Context;
  ExprRef Value = Context.symbol(8, "x");
  for (unsigned Depth = 0; Depth != 10; ++Depth)
    Value = Context.add(Value, Value);
  EXPECT_THROW(SMTLibWriter::write(Value, "result", true, {32, 4096}),
               std::length_error);

  ExprRef LongName = Context.symbol(8, std::string(1000, 'n'));
  EXPECT_THROW(SMTLibWriter::write(LongName, "result", true, {1024, 128}),
               std::length_error);
  EXPECT_THROW(SMTLibWriter::write(LongName, "result", true, {0, 128}),
               std::invalid_argument);
}

TEST(FormalSMTLib, BudgetsRejectLargeDeclarationsAndUpdatesBeforeGrowth) {
  ExprContext DeclarationContext({32, 1024, 4096, 4096});
  ExprRef Sum = DeclarationContext.symbol(8, std::string(900, 'a') + "0");
  for (unsigned Index = 1; Index != 64; ++Index) {
    Sum = DeclarationContext.add(
        Sum, DeclarationContext.symbol(8, std::string(900, 'a') +
                                              std::to_string(Index)));
  }
  EXPECT_THROW(SMTLibWriter::write(Sum, "result", true, {4096, 4096}),
               std::length_error);

  ExprContext UpdateContext({32, 1024, 20000, 20000});
  UpdateList Updates = UpdateContext.updates(UpdateContext.array(32, "bytes"));
  for (unsigned Index = 0; Index != 10000; ++Index) {
    Updates = UpdateContext.write(Updates, UpdateContext.constant(32, Index),
                                  UpdateContext.constant(8, Index));
  }
  ExprRef Read = UpdateContext.read(Updates, UpdateContext.symbol(32, "query"));
  EXPECT_THROW(SMTLibWriter::write(Read, "result", true, {20000, 512}),
               std::length_error);
}

llvm::APInt randomAPInt(unsigned Width, std::mt19937 &Generator) {
  llvm::SmallVector<std::uint64_t, 4> Words((Width + 63) / 64);
  for (std::uint64_t &Word : Words)
    Word = (static_cast<std::uint64_t>(Generator()) << 32) | Generator();
  return llvm::APInt(Width, llvm::ArrayRef<std::uint64_t>(Words));
}

TEST(FormalExprProperty, FixedSeedAPIntArithmeticMatchesFolding) {
  constexpr std::uint32_t Seed = 0x5eed1234U;
  std::mt19937 Generator(Seed);
  for (unsigned Iteration = 0; Iteration < 1000; ++Iteration) {
    unsigned Widths[] = {1, 8, 32, 65, 128};
    unsigned Width = Widths[Generator() % 5];
    llvm::APInt Left = randomAPInt(Width, Generator);
    llvm::APInt Right = randomAPInt(Width, Generator);
    ExprContext Context;
    ExprRef L = Context.constant(Left);
    ExprRef R = Context.constant(Right);
    EXPECT_EQ(Context.add(L, R)->constantValue(), Left + Right)
        << "seed=" << Seed << " iteration=" << Iteration;
    EXPECT_EQ(Context.sub(L, R)->constantValue(), Left - Right)
        << "seed=" << Seed << " iteration=" << Iteration;
    EXPECT_EQ(Context.mul(L, R)->constantValue(), Left * Right)
        << "seed=" << Seed << " iteration=" << Iteration;
    EXPECT_EQ(Context.bitAnd(L, R)->constantValue(), Left & Right);
    EXPECT_EQ(Context.bitOr(L, R)->constantValue(), Left | Right);
    EXPECT_EQ(Context.bitXor(L, R)->constantValue(), Left ^ Right);
    EXPECT_EQ(Context.eq(L, R)->constantValue().getBoolValue(), Left == Right);
    EXPECT_EQ(Context.ult(L, R)->constantValue().getBoolValue(),
              Left.ult(Right));
    EXPECT_EQ(Context.ule(L, R)->constantValue().getBoolValue(),
              Left.ule(Right));
    EXPECT_EQ(Context.slt(L, R)->constantValue().getBoolValue(),
              Left.slt(Right));
    EXPECT_EQ(Context.sle(L, R)->constantValue().getBoolValue(),
              Left.sle(Right));

    if (!Right.isZero()) {
      EXPECT_EQ(Context.udiv(L, R)->constantValue(), Left.udiv(Right));
      EXPECT_EQ(Context.urem(L, R)->constantValue(), Left.urem(Right));
      EXPECT_EQ(Context.srem(L, R)->constantValue(), Left.srem(Right));
      if (!(Left.isMinSignedValue() && Right.isAllOnes())) {
        EXPECT_EQ(Context.sdiv(L, R)->constantValue(), Left.sdiv(Right));
      }
    }

    std::uint64_t Amount = Right.getLimitedValue(Width);
    llvm::APInt ExpectedShl =
        Amount >= Width ? llvm::APInt(Width, 0) : Left.shl(Amount);
    llvm::APInt ExpectedLShr =
        Amount >= Width ? llvm::APInt(Width, 0) : Left.lshr(Amount);
    llvm::APInt ExpectedAShr =
        Amount >= Width ? (Left.isNegative() ? llvm::APInt::getAllOnes(Width)
                                             : llvm::APInt(Width, 0))
                        : Left.ashr(Amount);
    EXPECT_EQ(Context.shl(L, R)->constantValue(), ExpectedShl);
    EXPECT_EQ(Context.lshr(L, R)->constantValue(), ExpectedLShr);
    EXPECT_EQ(Context.ashr(L, R)->constantValue(), ExpectedAShr);

    unsigned ExtractWidth = std::max(1U, Width / 2);
    unsigned Offset = Width - ExtractWidth;
    EXPECT_EQ(Context.extract(L, Offset, ExtractWidth)->constantValue(),
              Left.lshr(Offset).trunc(ExtractWidth));
    EXPECT_EQ(Context.zext(L, Width + 1)->constantValue(),
              Left.zext(Width + 1));
    EXPECT_EQ(Context.sext(L, Width + 1)->constantValue(),
              Left.sext(Width + 1));
    EXPECT_EQ(Context.trunc(Context.zext(L, Width + 1), Width)->constantValue(),
              Left);
  }
}

TEST(FormalExpr, UndefinedDivisionConstantsRemainExplicitNodes) {
  ExprContext Context;
  ExprRef Seven = Context.constant(8, 7);
  ExprRef Zero = Context.constant(8, 0);
  ExprRef SignedMinimum = Context.constant(llvm::APInt::getSignedMinValue(8));
  ExprRef MinusOne = Context.constant(llvm::APInt::getAllOnes(8));

  EXPECT_EQ(Context.udiv(Seven, Zero)->kind(), ExprKind::UDiv);
  EXPECT_EQ(Context.sdiv(Seven, Zero)->kind(), ExprKind::SDiv);
  EXPECT_EQ(Context.urem(Seven, Zero)->kind(), ExprKind::URem);
  EXPECT_EQ(Context.srem(Seven, Zero)->kind(), ExprKind::SRem);
  EXPECT_EQ(Context.sdiv(SignedMinimum, MinusOne)->kind(), ExprKind::SDiv);
  EXPECT_FALSE(Context.sdiv(SignedMinimum, MinusOne)->isConstant());
}

} // namespace
