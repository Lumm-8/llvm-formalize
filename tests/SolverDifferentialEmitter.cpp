#include "formal/Solver.h"

#include <filesystem>
#include <iostream>
#include <string>
#include <vector>

namespace {

bool emit(formal::STPSolver &Solver, formal::ExprContext &Context,
          const formal::ConstraintSet &Constraints,
          const formal::ExprRef &Expression, const llvm::APInt &Expected) {
  formal::ExprRef Equality =
      Context.eq(Expression, Context.constant(Expected));
  formal::SolverResult Proof = Solver.mustBeTrue(Constraints, Equality);
  if (Proof.Status != formal::SolverStatus::Unsat || !Proof.Truth ||
      !*Proof.Truth || Proof.Metadata.SavedQuery.empty()) {
    std::cerr << "STP differential query failed: " << Proof.Metadata.Detail
              << '\n';
    return false;
  }
  formal::ConstraintSet WitnessConstraints = Constraints;
  WitnessConstraints.push_back(Equality);
  formal::SolverResult Witness = Solver.check(WitnessConstraints);
  if (Witness.Status != formal::SolverStatus::Sat ||
      Witness.Metadata.SavedQuery.empty()) {
    std::cerr << "STP differential witness failed: "
              << Witness.Metadata.Detail << '\n';
    return false;
  }
  std::cout << formal::toString(Proof.Status) << '|'
            << Proof.Metadata.SavedQuery.string() << '\n';
  std::cout << formal::toString(Witness.Status) << '|'
            << Witness.Metadata.SavedQuery.string() << '\n';
  return true;
}

bool emitTruth(formal::STPSolver &Solver,
               const formal::ConstraintSet &Constraints,
               const formal::ExprRef &Expression) {
  formal::SolverResult Proof = Solver.mustBeTrue(Constraints, Expression);
  if (Proof.Status != formal::SolverStatus::Unsat || !Proof.Truth ||
      !*Proof.Truth || Proof.Metadata.SavedQuery.empty()) {
    std::cerr << "STP differential truth query failed: "
              << Proof.Metadata.Detail << '\n';
    return false;
  }
  formal::ConstraintSet WitnessConstraints = Constraints;
  WitnessConstraints.push_back(Expression);
  formal::SolverResult Witness = Solver.check(WitnessConstraints);
  if (Witness.Status != formal::SolverStatus::Sat ||
      Witness.Metadata.SavedQuery.empty()) {
    std::cerr << "STP differential truth witness failed: "
              << Witness.Metadata.Detail << '\n';
    return false;
  }
  std::cout << formal::toString(Proof.Status) << '|'
            << Proof.Metadata.SavedQuery.string() << '\n';
  std::cout << formal::toString(Witness.Status) << '|'
            << Witness.Metadata.SavedQuery.string() << '\n';
  return true;
}

} // namespace

int main(int ArgumentCount, char **Arguments) {
  if (ArgumentCount != 2)
    return 2;

  formal::ExprContext Context;
  formal::ExprRef X = Context.symbol(128, "differential_x");
  formal::ExprRef Y = Context.symbol(128, "differential_y");
  formal::ExprRef Flag = Context.booleanSymbol("differential_flag");
  formal::ConstraintSet Constraints{
      Context.eq(X, Context.constant(llvm::APInt(128, 0x80))),
      Context.eq(Y, Context.constant(llvm::APInt(128, 3))), Flag};

  formal::STPSolverOptions Options;
  Options.ExportAllQueries = true;
  Options.FailureQueryDirectory = std::filesystem::path(Arguments[1]);
  formal::STPSolver Solver(Options);
  bool Success = true;
  auto Emit = [&](const formal::ExprRef &Expression,
                  const llvm::APInt &Expected) {
    Success &= emit(Solver, Context, Constraints, Expression, Expected);
  };

  Emit(Context.add(X, Y), llvm::APInt(128, 0x83));
  Emit(Context.sub(X, Y), llvm::APInt(128, 0x7d));
  Emit(Context.mul(X, Y), llvm::APInt(128, 0x180));
  Emit(Context.udiv(X, Y), llvm::APInt(128, 0x2a));
  Emit(Context.urem(X, Y), llvm::APInt(128, 2));
  Emit(Context.shl(X, Y), llvm::APInt(128, 0x400));
  Emit(Context.lshr(X, Y), llvm::APInt(128, 0x10));
  Emit(Context.bitAnd(X, Y), llvm::APInt(128, 0));
  Emit(Context.bitOr(X, Y), llvm::APInt(128, 0x83));
  Emit(Context.bitXor(X, Y), llvm::APInt(128, 0x83));
  Emit(Context.select(Flag, X, Y), llvm::APInt(128, 0x80));
  Emit(Context.concat(Context.trunc(X, 8), Context.trunc(Y, 8)),
       llvm::APInt(16, 0x8003));
  Emit(Context.extract(X, 4, 8), llvm::APInt(8, 8));
  Emit(Context.zext(Context.trunc(X, 8), 128), llvm::APInt(128, 0x80));
  Emit(Context.sext(Context.trunc(X, 8), 128),
       llvm::APInt::getAllOnes(128) - llvm::APInt(128, 0x7f));

  formal::ExprRef SignedX = Context.symbol(128, "differential_signed_x");
  formal::ExprRef SignedY = Context.symbol(128, "differential_signed_y");
  formal::ExprRef FalseFlag =
      Context.booleanSymbol("differential_false_flag");
  llvm::APInt MinusSeven(128, static_cast<std::uint64_t>(-7), true);
  llvm::APInt MinusTwo(128, static_cast<std::uint64_t>(-2), true);
  llvm::APInt MinusOne = llvm::APInt::getAllOnes(128);
  formal::ConstraintSet SignedConstraints = Constraints;
  SignedConstraints.push_back(
      Context.eq(SignedX, Context.constant(MinusSeven)));
  SignedConstraints.push_back(
      Context.eq(SignedY, Context.constant(128, 3)));
  SignedConstraints.push_back(Context.logicalNot(FalseFlag));
  Success &= emit(Solver, Context, SignedConstraints,
                  Context.sdiv(SignedX, SignedY), MinusTwo);
  Success &= emit(Solver, Context, SignedConstraints,
                  Context.srem(SignedX, SignedY), MinusOne);
  Success &= emit(Solver, Context, SignedConstraints,
                  Context.ashr(SignedX, SignedY), MinusOne);
  Success &= emitTruth(Solver, SignedConstraints,
                       Context.slt(SignedX, SignedY));
  Success &= emitTruth(Solver, SignedConstraints,
                       Context.sle(SignedX, SignedY));
  Success &= emitTruth(Solver, SignedConstraints,
                       Context.logicalNot(FalseFlag));

  formal::ArrayRef Array = Context.array(8, "differential_array");
  formal::UpdateList Base = Context.updates(Array);
  formal::ExprRef ReadIndex = Context.trunc(Y, 8);
  formal::ExprRef RootRead = Context.read(Base, ReadIndex);
  formal::ConstraintSet ArrayConstraints = Constraints;
  ArrayConstraints.push_back(
      Context.eq(RootRead, Context.constant(8, 0x5a)));
  formal::UpdateList Updates = Context.write(
      Base, Context.trunc(X, 8), Context.constant(8, 0xa5));
  Success &= emit(Solver, Context, ArrayConstraints,
                  Context.read(Updates, ReadIndex), llvm::APInt(8, 0x5a));
  return Success ? 0 : 3;
}
