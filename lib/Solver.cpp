#include "formal/Solver.h"

#include <algorithm>
#include <stdexcept>
#include <unordered_map>

namespace formal {

const char *toString(SolverStatus Status) noexcept {
  switch (Status) {
  case SolverStatus::Sat:
    return "SAT";
  case SolverStatus::Unsat:
    return "UNSAT";
  case SolverStatus::Unknown:
    return "UNKNOWN";
  case SolverStatus::Timeout:
    return "TIMEOUT";
  case SolverStatus::InternalError:
    return "INTERNAL_ERROR";
  }
  return "INTERNAL_ERROR";
}

const char *toReasonCode(SolverReason Reason) noexcept {
  switch (Reason) {
  case SolverReason::None:
    return "NONE";
  case SolverReason::Satisfiable:
    return "SOLVER_SAT";
  case SolverReason::Unsatisfiable:
    return "SOLVER_UNSAT";
  case SolverReason::BackendUnknown:
    return "SOLVER_UNKNOWN";
  case SolverReason::Timeout:
    return "SOLVER_TIMEOUT";
  case SolverReason::BackendFailure:
    return "SOLVER_BACKEND_FAILURE";
  case SolverReason::TranslationError:
    return "SOLVER_TRANSLATION_ERROR";
  case SolverReason::InvalidQuery:
    return "SOLVER_INVALID_QUERY";
  case SolverReason::ModelValidationFailed:
    return "SOLVER_MODEL_VALIDATION_FAILED";
  case SolverReason::ArtifactWriteFailed:
    return "SOLVER_ARTIFACT_WRITE_FAILED";
  case SolverReason::WorkerFailure:
    return "UNKNOWN-SOLVER-FAILURE";
  }
  return "SOLVER_INTERNAL_ERROR";
}

namespace {

class Evaluator final {
public:
  Evaluator(const Model &Assignment, std::string &Error)
      : Assignment(Assignment), Error(Error) {}

  std::optional<llvm::APInt> run(const ExprRef &Expression) {
    if (!Expression) {
      Error = "cannot evaluate a null expression";
      return std::nullopt;
    }
    auto Existing = Cache.find(Expression.get());
    if (Existing != Cache.end())
      return Existing->second;
    std::optional<llvm::APInt> Result = evaluateNode(Expression);
    if (Result)
      Cache.emplace(Expression.get(), *Result);
    return Result;
  }

private:
  std::optional<llvm::APInt> scalar(const ExprRef &Expression) {
    const ScalarModelValue *Found = nullptr;
    for (const ScalarModelValue &Value : Assignment.Scalars) {
      if (Value.Name != Expression->symbolName())
        continue;
      if (Value.ValueSort != Expression->sort()) {
        Error = "model symbol sort mismatch for " + Value.Name;
        return std::nullopt;
      }
      if (Found && Found->Value != Value.Value) {
        Error = "model contains conflicting values for " + Value.Name;
        return std::nullopt;
      }
      Found = &Value;
    }
    if (!Found) {
      Error = "model is missing scalar symbol " + Expression->symbolName();
      return std::nullopt;
    }
    unsigned ExpectedWidth = Expression->sort().isBool()
                                 ? 1
                                 : Expression->sort().bitWidth();
    if (Found->Value.getBitWidth() != ExpectedWidth) {
      Error = "model scalar width mismatch for " + Found->Name;
      return std::nullopt;
    }
    return Found->Value;
  }

  std::optional<llvm::APInt> read(const ExprRef &Expression) {
    std::optional<llvm::APInt> Index = run(Expression->operand(0));
    if (!Index)
      return std::nullopt;
    const UpdateList &Updates = Expression->updates();
    for (UpdateNodeRef Node = Updates.head(); Node; Node = Node->next()) {
      std::optional<llvm::APInt> WriteIndex = run(Node->index());
      if (!WriteIndex)
        return std::nullopt;
      if (*WriteIndex == *Index)
        return run(Node->value());
    }

    const ArrayElementValue *Found = nullptr;
    for (const ArrayElementValue &Element : Assignment.ArrayElements) {
      if (Element.Root.get() != Updates.root().get() || Element.Index != *Index)
        continue;
      if (Found && Found->Value != Element.Value) {
        Error = "model contains conflicting values for array " +
                Updates.root()->name();
        return std::nullopt;
      }
      Found = &Element;
    }
    if (!Found) {
      Error = "model is missing index for array " + Updates.root()->name();
      return std::nullopt;
    }
    return llvm::APInt(8, Found->Value);
  }

  std::optional<llvm::APInt> evaluateNode(const ExprRef &Expression) {
    if (Expression->kind() == ExprKind::Constant)
      return Expression->constantValue();
    if (Expression->kind() == ExprKind::Symbol)
      return scalar(Expression);
    if (Expression->kind() == ExprKind::Read)
      return read(Expression);

    std::optional<llvm::APInt> Left;
    std::optional<llvm::APInt> Right;
    if (!Expression->operands().empty()) {
      Left = run(Expression->operand(0));
      if (!Left)
        return std::nullopt;
    }
    if (Expression->operands().size() > 1) {
      Right = run(Expression->operand(1));
      if (!Right)
        return std::nullopt;
    }

    switch (Expression->kind()) {
    case ExprKind::Add:
      return *Left + *Right;
    case ExprKind::Sub:
      return *Left - *Right;
    case ExprKind::Mul:
      return *Left * *Right;
    case ExprKind::UDiv:
      return Right->isZero() ? llvm::APInt::getAllOnes(Left->getBitWidth())
                             : Left->udiv(*Right);
    case ExprKind::SDiv:
      if (Right->isZero())
        return Left->isNegative() ? llvm::APInt(Left->getBitWidth(), 1)
                                  : llvm::APInt::getAllOnes(
                                        Left->getBitWidth());
      if (Left->isMinSignedValue() && Right->isAllOnes())
        return *Left;
      return Left->sdiv(*Right);
    case ExprKind::URem:
      return Right->isZero() ? *Left : Left->urem(*Right);
    case ExprKind::SRem:
      return Right->isZero() ? *Left : Left->srem(*Right);
    case ExprKind::And:
      return *Left & *Right;
    case ExprKind::Or:
      return *Left | *Right;
    case ExprKind::Xor:
      return *Left ^ *Right;
    case ExprKind::Shl: {
      std::uint64_t Amount = Right->getLimitedValue(Left->getBitWidth());
      return Amount >= Left->getBitWidth()
                 ? llvm::APInt(Left->getBitWidth(), 0)
                 : Left->shl(Amount);
    }
    case ExprKind::LShr: {
      std::uint64_t Amount = Right->getLimitedValue(Left->getBitWidth());
      return Amount >= Left->getBitWidth()
                 ? llvm::APInt(Left->getBitWidth(), 0)
                 : Left->lshr(Amount);
    }
    case ExprKind::AShr: {
      std::uint64_t Amount = Right->getLimitedValue(Left->getBitWidth());
      if (Amount >= Left->getBitWidth())
        return Left->isNegative()
                   ? llvm::APInt::getAllOnes(Left->getBitWidth())
                   : llvm::APInt(Left->getBitWidth(), 0);
      return Left->ashr(Amount);
    }
    case ExprKind::Eq:
      return llvm::APInt(1, *Left == *Right);
    case ExprKind::Ult:
      return llvm::APInt(1, Left->ult(*Right));
    case ExprKind::Ule:
      return llvm::APInt(1, Left->ule(*Right));
    case ExprKind::Slt:
      return llvm::APInt(1, Left->slt(*Right));
    case ExprKind::Sle:
      return llvm::APInt(1, Left->sle(*Right));
    case ExprKind::Not:
      return llvm::APInt(1, Left->isZero());
    case ExprKind::Select: {
      std::optional<llvm::APInt> Selected =
          run(Expression->operand(Left->isZero() ? 2 : 1));
      return Selected;
    }
    case ExprKind::Concat: {
      unsigned Width = Left->getBitWidth() + Right->getBitWidth();
      return Left->zext(Width).shl(Right->getBitWidth()) |
             Right->zext(Width);
    }
    case ExprKind::Extract:
      return Left->lshr(Expression->offset()).trunc(Expression->width());
    case ExprKind::ZExt:
      return Left->zext(Expression->width());
    case ExprKind::SExt:
      return Left->sext(Expression->width());
    case ExprKind::Trunc:
      return Left->trunc(Expression->width());
    case ExprKind::Constant:
    case ExprKind::Symbol:
    case ExprKind::Read:
      break;
    }
    Error = "unsupported Formal Expr kind in evaluator";
    return std::nullopt;
  }

  const Model &Assignment;
  std::string &Error;
  std::unordered_map<const Expr *, llvm::APInt> Cache;
};

} // namespace

std::optional<llvm::APInt> evaluate(const ExprRef &Expression,
                                    const Model &Assignment,
                                    std::string &Error) {
  Error.clear();
  Evaluator Instance(Assignment, Error);
  return Instance.run(Expression);
}

bool validateModel(const ConstraintSet &Constraints, const Model &Assignment,
                   std::string &Error) {
  Error.clear();
  Evaluator Instance(Assignment, Error);
  for (std::size_t Index = 0; Index < Constraints.size(); ++Index) {
    if (!Constraints[Index]) {
      Error = "constraint " + std::to_string(Index) + " is null";
      return false;
    }
    if (!Constraints[Index]->sort().isBool()) {
      Error = "constraint " + std::to_string(Index) + " is not Bool";
      return false;
    }
    std::optional<llvm::APInt> Value = Instance.run(Constraints[Index]);
    if (!Value) {
      Error = "constraint " + std::to_string(Index) + ": " + Error;
      return false;
    }
    if (Value->getBitWidth() != 1 || Value->isZero()) {
      Error = "constraint " + std::to_string(Index) +
              " is false under the returned model";
      return false;
    }
  }
  return true;
}

} // namespace formal
