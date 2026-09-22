#include "formal/Solver.h"

#include <filesystem>
#include <iostream>
#include <optional>

int main(int ArgumentCount, char **Arguments) {
  if (ArgumentCount != 2)
    return 2;
  const std::filesystem::path OutputDirectory(Arguments[1]);
  formal::ExprContext Context;
  formal::ExprRef X = Context.symbol(8, "export_reparse_x");

  formal::STPSolverOptions TimeoutOptions;
  TimeoutOptions.Timeout = std::chrono::milliseconds(0);
  TimeoutOptions.FailureQueryDirectory = OutputDirectory;
  formal::STPSolver TimeoutSolver(TimeoutOptions);
  formal::SolverResult Timeout =
      TimeoutSolver.check({Context.eq(X, Context.constant(8, 9))});
  if (Timeout.Status != formal::SolverStatus::Timeout ||
      Timeout.Metadata.SavedQuery.empty())
    return 3;

  formal::STPSolverOptions ValueOptions;
  ValueOptions.ExportAllQueries = true;
  ValueOptions.FailureQueryDirectory = OutputDirectory;
  formal::STPSolver ValueSolver(ValueOptions);
  formal::ExprRef ValueTarget = Context.concat(
      Context.constant(8, 0), Context.add(X, Context.constant(8, 1)));
  formal::SolverResult Value = ValueSolver.getValue(
      {Context.eq(X, Context.constant(8, 9))}, ValueTarget);
  if (Value.Status != formal::SolverStatus::Sat || !Value.Value ||
      *Value.Value != llvm::APInt(16, 10) ||
      Value.Metadata.SavedQuery.empty())
    return 4;

  formal::ArrayRef Bytes = Context.array(8, "export_reparse_bytes");
  formal::ExprRef Index = Context.symbol(8, "export_reparse_index");
  formal::UpdateList Updates = Context.write(
      Context.updates(Bytes), Index, Context.constant(8, 0x5a));
  Updates = Context.write(Updates, Context.constant(8, 2),
                          Context.constant(8, 0xa5));
  formal::ConstraintSet ArrayConstraints{
      Context.eq(Index, Context.constant(8, 2)),
      Context.eq(Context.read(Updates, Index), Context.constant(8, 0xa5)),
      Context.eq(Context.read(Context.updates(Bytes), Context.constant(8, 3)),
                 Context.constant(8, 0x33))};
  formal::STPSolverOptions ArrayOptions;
  ArrayOptions.Timeout = formal::STPSolverOptions::UnlimitedTimeout;
  ArrayOptions.FailureQueryDirectory = OutputDirectory;
  ArrayOptions.AdditionalModelAudit =
      [](const formal::ConstraintSet &, const formal::Model &)
      -> std::optional<std::string> { return "forced artifact export"; };
  formal::STPSolver ArraySolver(ArrayOptions);
  formal::SolverResult Array = ArraySolver.check(ArrayConstraints);
  if (Array.Status != formal::SolverStatus::InternalError ||
      Array.Metadata.Reason != formal::SolverReason::ModelValidationFailed ||
      Array.Metadata.SavedQuery.empty())
    return 5;

  formal::ArrayRef RequestedFirst =
      Context.array(8, "export_requested_first");
  formal::ArrayRef RequestedSecond =
      Context.array(16, "export_requested_second");
  formal::STPSolverOptions RequestOptions;
  RequestOptions.Timeout = formal::STPSolverOptions::UnlimitedTimeout;
  RequestOptions.FailureQueryDirectory = OutputDirectory;
  RequestOptions.AdditionalModelAudit =
      [](const formal::ConstraintSet &, const formal::Model &)
      -> std::optional<std::string> { return "forced array request export"; };
  formal::STPSolver RequestSolver(RequestOptions);
  formal::SolverResult Request = RequestSolver.getInitialValues(
      {}, {{RequestedSecond, 2}, {RequestedFirst, 1}});
  if (Request.Status != formal::SolverStatus::InternalError ||
      Request.Metadata.Reason != formal::SolverReason::ModelValidationFailed ||
      Request.Metadata.SavedQuery.empty())
    return 6;

  std::cout << "SAT|" << Timeout.Metadata.SavedQuery.string() << '\n';
  std::cout << "VALUE|16|10|" << Value.Metadata.SavedQuery.string() << '\n';
  std::cout << "SAT|" << Array.Metadata.SavedQuery.string() << '\n';
  std::cout << "ARRAY|" << Request.Metadata.SavedQuery.string() << '\n';
  return 0;
}
