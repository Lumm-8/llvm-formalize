#include "formal/M5Run.h"

#include <algorithm>
#include <exception>

namespace formal {

M5Analysis analyzeM5(llvm::Function &Entry, ExprContext &Expressions,
                     Solver &SolverBackend, ExecutorOptions Options) {
  M5Analysis Analysis;
  Options.EnableMemory = true;
  Options.EnableRegistration = true;
  try {
    ScalarExecutor Executor(Expressions, SolverBackend, Options);
    Analysis.Execution = Executor.execute(Entry, {});
    Analysis.Relation = exportRelation(Analysis.Execution, Expressions);
    if (Analysis.Execution.Status == ExecutionStatus::Error &&
        !Analysis.Execution.HasLoopPolicyError)
      Analysis.CounterexampleJSON =
          exportCounterexampleJSON(Analysis.Execution);
  } catch (const std::exception &Error) {
    Analysis.Execution.Status = ExecutionStatus::InternalError;
    Analysis.Relation.Reason =
        std::string("M5 execution failed: ") + Error.what();
  }

  if (Analysis.Execution.Status == ExecutionStatus::InternalError) {
    Analysis.Verdict = "INTERNAL_ERROR";
  } else if (Analysis.Execution.HasLoopPolicyError) {
    Analysis.Verdict = "INVALID_TASK";
  } else if (Analysis.Execution.Status == ExecutionStatus::Error &&
             Analysis.CounterexampleJSON) {
    Analysis.Verdict = "UNSAFE";
  } else if (Analysis.Execution.HasUnwindingAssumption &&
             Analysis.Execution.Status == ExecutionStatus::Unknown &&
             std::all_of(Analysis.Execution.Paths.begin(),
                         Analysis.Execution.Paths.end(),
                         [](const PathResult &Path) {
                           return Path.Status != PathStatus::Incomplete ||
                                  Path.IncompleteReason ==
                                      IncompletenessReason::LoopBound;
                         })) {
    Analysis.Verdict = "CONDITIONAL";
  } else if (Analysis.Execution.Status == ExecutionStatus::Complete &&
             Analysis.Relation.Complete) {
    Analysis.Verdict = "SAFE";
  } else {
    Analysis.Verdict = "UNKNOWN";
  }
  if (Analysis.Verdict == "UNSAFE") {
    for (const PathResult &Path : Analysis.Execution.Paths)
      if (hasClosedRegisteredErrorPath(Path)) {
        Analysis.Reason = toReasonCode(Path.ErrorReason);
        break;
      }
  } else if (Analysis.Verdict == "INVALID_TASK")
    Analysis.Reason = "loop backedge bound reached under error policy";
  else if (Analysis.Verdict == "CONDITIONAL")
    Analysis.Reason = "unwinding assumption: disallowed backedges are excluded";
  else if (Analysis.Execution.Status == ExecutionStatus::Error &&
             !Analysis.CounterexampleJSON)
    Analysis.Reason = "error model is not closed over registered input bytes";
  else if (!Analysis.Relation.Reason.empty())
    Analysis.Reason = Analysis.Relation.Reason;
  else
    Analysis.Reason =
        Analysis.Relation.Complete ? "COMPLETE" : "RELATION_UNAVAILABLE";
  return Analysis;
}

} // namespace formal
