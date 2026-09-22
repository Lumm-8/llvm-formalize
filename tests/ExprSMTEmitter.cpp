#include "formal/Expr.h"
#include "formal/SMTLibWriter.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include <array>
#include <cstdint>
#include <random>
#include <string>
#include <system_error>

int main(int Argc, char **Argv) {
  if (Argc != 4 || std::string(Argv[1]) != "--emit-smt") {
    llvm::errs() << "usage: FormalExprSMTEmitter --emit-smt "
                    "syntax|semantics|arrays|property|simplification OUTPUT\n";
    return 2;
  }

  formal::ExprContext Context;
  std::string Text;
  std::string Mode = Argv[2];
  if (Mode == "syntax") {
    formal::ArrayRef Bytes = Context.array(32, "bytes");
    formal::UpdateList Updates =
        Context.write(Context.updates(Bytes), Context.symbol(32, "index"),
                      Context.constant(8, 42));
    Text = formal::SMTLibWriter::write(
        Context.read(Updates, Context.symbol(32, "query_index")));
  } else if (Mode == "semantics") {
    formal::ExprRef X = Context.symbol(8, "x");
    formal::ExprRef One = Context.constant(8, 1);
    formal::ExprRef Commutative =
        Context.eq(Context.add(X, One), Context.add(One, X));
    Text = formal::SMTLibWriter::write(Commutative, "result", false);
    Text += "(assert (not |result|))\n(check-sat)\n";
  } else if (Mode == "arrays") {
    formal::ArrayRef Bytes = Context.array(32, "bytes");
    constexpr std::uint32_t Seed = 0xa44a1234U;
    std::mt19937 Generator(Seed);
    formal::UpdateList Updates = Context.updates(Bytes);
    std::array<std::uint8_t, 16> ConcreteBytes{};
    for (unsigned Update = 0; Update != 64; ++Update) {
      unsigned Index = Generator() % ConcreteBytes.size();
      unsigned Value = Generator() & 0xffU;
      ConcreteBytes[Index] = static_cast<std::uint8_t>(Value);
      Updates = Context.write(Updates, Context.constant(32, Index),
                              Context.constant(8, Value));
    }
    formal::ExprRef ReadIndex = Context.symbol(32, "read_index");
    unsigned ConcreteReadIndex = Generator() % ConcreteBytes.size();
    formal::ExprRef SameValue =
        Context.eq(Context.read(Updates, ReadIndex),
                   Context.constant(8, ConcreteBytes[ConcreteReadIndex]));
    Text = formal::SMTLibWriter::write(SameValue, "result", false);
    Text += "(assert (= |read_index| (_ bv" +
            std::to_string(ConcreteReadIndex) + " 32)))\n";
    Text += "(assert (not |result|))\n(check-sat)\n";
  } else if (Mode == "property") {
    constexpr std::uint32_t Seed = 0x5eed1234U;
    std::mt19937 Generator(Seed);
    formal::ExprRef AllCorrect = Context.boolean(true);
    std::string Assignments;
    for (unsigned Case = 0; Case != 8; ++Case) {
      std::string XName = "x" + std::to_string(Case);
      std::string YName = "y" + std::to_string(Case);
      formal::ExprRef X = Context.symbol(8, XName);
      formal::ExprRef Y = Context.symbol(8, YName);
      llvm::APInt XValue(8, Generator());
      llvm::APInt YValue(8, Generator());
      formal::ExprRef Expression = X;
      llvm::APInt Expected = XValue;
      for (unsigned Step = 0; Step != 20; ++Step) {
        unsigned Operation = (Step + Case) % 20;
        llvm::APInt Constant(8, Generator());
        formal::ExprRef C = Context.constant(Constant);
        switch (Operation) {
        case 0:
          Expression = Context.add(Expression, Y);
          Expected += YValue;
          break;
        case 1:
          Expression = Context.sub(Expression, Y);
          Expected -= YValue;
          break;
        case 2:
          Expression = Context.mul(Expression, Y);
          Expected *= YValue;
          break;
        case 3:
          Expression = Context.bitAnd(Expression, Y);
          Expected &= YValue;
          break;
        case 4:
          Expression = Context.bitOr(Expression, Y);
          Expected |= YValue;
          break;
        case 5:
          Expression = Context.bitXor(Expression, Y);
          Expected ^= YValue;
          break;
        case 6: {
          llvm::APInt Divisor =
              Constant.isZero() ? llvm::APInt(8, 1) : Constant;
          Expression = Context.udiv(Expression, Context.constant(Divisor));
          Expected = Expected.udiv(Divisor);
          break;
        }
        case 7: {
          llvm::APInt Divisor = Constant.isZero() || Constant.isAllOnes()
                                    ? llvm::APInt(8, 3)
                                    : Constant;
          Expression = Context.sdiv(Expression, Context.constant(Divisor));
          Expected = Expected.sdiv(Divisor);
          break;
        }
        case 8: {
          llvm::APInt Divisor =
              Constant.isZero() ? llvm::APInt(8, 1) : Constant;
          Expression = Context.urem(Expression, Context.constant(Divisor));
          Expected = Expected.urem(Divisor);
          break;
        }
        case 9: {
          llvm::APInt Divisor =
              Constant.isZero() ? llvm::APInt(8, 1) : Constant;
          Expression = Context.srem(Expression, Context.constant(Divisor));
          Expected = Expected.srem(Divisor);
          break;
        }
        case 10:
        case 11:
        case 12: {
          std::uint64_t Amount = Constant.getLimitedValue(8);
          if (Operation == 10) {
            Expression = Context.shl(Expression, C);
            Expected = Amount >= 8 ? llvm::APInt(8, 0) : Expected.shl(Amount);
          } else if (Operation == 11) {
            Expression = Context.lshr(Expression, C);
            Expected = Amount >= 8 ? llvm::APInt(8, 0) : Expected.lshr(Amount);
          } else {
            Expression = Context.ashr(Expression, C);
            Expected = Amount >= 8
                           ? (Expected.isNegative() ? llvm::APInt::getAllOnes(8)
                                                    : llvm::APInt(8, 0))
                           : Expected.ashr(Amount);
          }
          break;
        }
        case 13: {
          bool Condition = Expected.ult(YValue);
          Expression =
              Context.select(Context.ult(Expression, Y), Y, Expression);
          if (Condition)
            Expected = YValue;
          break;
        }
        case 14: {
          bool Condition = Expected.slt(YValue);
          Expression =
              Context.select(Context.slt(Expression, Y), Y, Expression);
          if (Condition)
            Expected = YValue;
          break;
        }
        case 15: {
          llvm::APInt Wide = Expected.zext(16).shl(8) | YValue.zext(16);
          unsigned Offset = Generator() % 9;
          Expression =
              Context.extract(Context.concat(Expression, Y), Offset, 8);
          Expected = Wide.extractBits(8, Offset);
          break;
        }
        case 16:
          Expression = Context.trunc(Context.zext(Expression, 16), 8);
          break;
        case 17:
          Expression = Context.trunc(Context.sext(Expression, 16), 8);
          break;
        case 18: {
          bool Condition = Expected.ule(YValue);
          formal::ExprRef Guard =
              Context.logicalNot(Context.ule(Expression, Y));
          Expression = Context.select(Guard, Expression, Y);
          if (Condition)
            Expected = YValue;
          break;
        }
        default: {
          bool Condition = Expected.sle(YValue);
          Expression =
              Context.select(Context.sle(Expression, Y), C, Expression);
          if (Condition)
            Expected = Constant;
          break;
        }
        }
      }
      formal::ExprRef Correct =
          Context.eq(Expression, Context.constant(Expected));
      formal::ExprRef BitTruth =
          Context.toBool(Context.extract(Expression, 0, 1));
      bool ExpectedTruth = Expected[0];
      formal::ExprRef TruthCorrect =
          ExpectedTruth ? BitTruth : Context.logicalNot(BitTruth);
      Correct = Context.select(TruthCorrect, Correct, Context.boolean(false));
      AllCorrect = Context.select(Correct, AllCorrect, Context.boolean(false));
      Assignments += "(assert (= |" + XName + "| (_ bv" +
                     std::to_string(XValue.getZExtValue()) + " 8)))\n";
      Assignments += "(assert (= |" + YName + "| (_ bv" +
                     std::to_string(YValue.getZExtValue()) + " 8)))\n";
    }
    Text = formal::SMTLibWriter::write(AllCorrect, "result", false);
    Text += Assignments;
    Text += "(assert (not |result|))\n(check-sat)\n";
  } else if (Mode == "simplification") {
    constexpr std::uint32_t Seed = 0x51a91f1eU;
    std::mt19937 Generator(Seed);
    formal::ExprRef X = Context.symbol(8, "simplify_x");
    formal::ExprRef Expression = X;
    std::string Raw = "|simplify_x|";
    for (unsigned Step = 0; Step != 64; ++Step) {
      switch (Generator() % 8) {
      case 0:
        Expression = Context.add(Expression, Context.constant(8, 0));
        Raw = "(bvadd " + Raw + " (_ bv0 8))";
        break;
      case 1:
        Expression = Context.sub(Expression, Context.constant(8, 0));
        Raw = "(bvsub " + Raw + " (_ bv0 8))";
        break;
      case 2:
        Expression = Context.mul(Expression, Context.constant(8, 1));
        Raw = "(bvmul " + Raw + " (_ bv1 8))";
        break;
      case 3:
        Expression = Context.bitAnd(Expression, Context.constant(8, 0xffU));
        Raw = "(bvand " + Raw + " (_ bv255 8))";
        break;
      case 4:
        Expression = Context.bitOr(Expression, Context.constant(8, 0));
        Raw = "(bvor " + Raw + " (_ bv0 8))";
        break;
      case 5:
        Expression = Context.bitXor(Expression, Context.constant(8, 0));
        Raw = "(bvxor " + Raw + " (_ bv0 8))";
        break;
      case 6: {
        unsigned FalseValue = Generator() & 0xffU;
        Expression = Context.select(Context.boolean(true), Expression,
                                    Context.constant(8, FalseValue));
        Raw =
            "(ite true " + Raw + " (_ bv" + std::to_string(FalseValue) + " 8))";
        break;
      }
      default:
        Expression = Context.select(Context.eq(X, X), Expression, Expression);
        Raw = "(ite (= |simplify_x| |simplify_x|) " + Raw + " " + Raw + ")";
        break;
      }
    }
    Text = formal::SMTLibWriter::write(Expression, "result", false);
    Text += "(assert (not (= |result| " + Raw + ")))\n(check-sat)\n";
  } else {
    llvm::errs() << "unknown SMT fixture mode: " << Mode << '\n';
    return 2;
  }

  std::error_code Error;
  llvm::raw_fd_ostream Output(Argv[3], Error, llvm::sys::fs::OF_Text);
  if (Error) {
    llvm::errs() << "cannot write SMT fixture: " << Error.message() << '\n';
    return 3;
  }
  Output << Text;
  return Output.has_error() ? 3 : 0;
}
