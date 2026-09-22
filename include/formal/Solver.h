#ifndef FORMAL_SOLVER_H
#define FORMAL_SOLVER_H

#include "formal/Expr.h"

#include "llvm/ADT/APInt.h"

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace formal {

namespace detail {
int runSTPWorkerProcess(int RequestFileDescriptor,
                        int ResultFileDescriptor);
}

using ConstraintSet = std::vector<ExprRef>;

enum class SolverStatus { Sat, Unsat, Unknown, Timeout, InternalError };

enum class SolverReason {
  None,
  Satisfiable,
  Unsatisfiable,
  BackendUnknown,
  Timeout,
  BackendFailure,
  TranslationError,
  InvalidQuery,
  ModelValidationFailed,
  ArtifactWriteFailed,
  WorkerFailure,
};

const char *toString(SolverStatus Status) noexcept;
const char *toReasonCode(SolverReason Reason) noexcept;

struct TranslationStatistics {
  std::size_t ExpressionTranslations = 0;
  std::size_t ExpressionCacheHits = 0;
  std::size_t ArrayTranslations = 0;
  std::size_t ArrayCacheHits = 0;
  std::uint64_t ContextId = 0;
};

struct QueryMetadata {
  std::uint64_t QueryId = 0;
  std::chrono::nanoseconds Elapsed{0};
  std::string BackendName;
  std::string BackendVersion;
  std::string SatBackend;
  SolverReason Reason = SolverReason::None;
  std::string Detail;
  std::filesystem::path SavedQuery;
  TranslationStatistics Translation;
};

struct ScalarModelValue {
  std::string Name;
  Sort ValueSort;
  llvm::APInt Value;
};

struct ArrayElementValue {
  ArrayRef Root;
  llvm::APInt Index;
  std::uint8_t Value = 0;
};

struct Model {
  std::vector<ScalarModelValue> Scalars;
  std::vector<ArrayElementValue> ArrayElements;
};

struct ArrayModelRequest {
  ArrayRef Root;
  std::size_t ByteCount = 0;
};

struct InitialArrayValue {
  ArrayRef Root;
  std::vector<std::uint8_t> Bytes;
};

struct SolverResult {
  SolverStatus Status = SolverStatus::InternalError;
  QueryMetadata Metadata;
  std::optional<bool> Truth;
  std::optional<llvm::APInt> Value;
  std::optional<Model> SatisfyingModel;
  std::vector<InitialArrayValue> InitialValues;
};

class Solver {
public:
  virtual ~Solver() = default;

  virtual SolverResult check(const ConstraintSet &Constraints) = 0;
  virtual SolverResult mayBeTrue(const ConstraintSet &Constraints,
                                 const ExprRef &Expression) = 0;
  virtual SolverResult mustBeTrue(const ConstraintSet &Constraints,
                                  const ExprRef &Expression) = 0;
  virtual SolverResult getValue(const ConstraintSet &Constraints,
                                const ExprRef &Expression) = 0;
  virtual SolverResult
  getInitialValues(const ConstraintSet &Constraints,
                   const std::vector<ArrayModelRequest> &Arrays) = 0;
};

struct STPSolverOptions {
  // Explicitly opts out of the per-query wall-clock deadline. This is required
  // for AdditionalModelAudit because an arbitrary in-process callback cannot
  // be cancelled safely.
  static constexpr std::chrono::milliseconds UnlimitedTimeout =
      std::chrono::milliseconds::max();

  std::chrono::milliseconds Timeout{30000};
  unsigned MaxConflicts = 0;
  std::size_t MaxArrayBytes = 1U << 20;
  // Independent caps for the parent-accepted result, the worker address
  // space, and any file the worker can grow. ASan builds omit RLIMIT_AS
  // because the sanitizer reserves a large virtual address range.
  std::size_t MaxWorkerResponseBytes = 64U << 20;
  std::uint64_t MaxWorkerMemoryBytes = std::uint64_t{2} << 30;
  std::uint64_t MaxWorkerFileBytes = std::uint64_t{128} << 20;
  std::filesystem::path FailureQueryDirectory{"solver-queries"};
  bool ExportAllQueries = false;
  // An optional additional audit may reject, but cannot approve or replace,
  // the mandatory built-in Formal Expr model validation. It is accepted only
  // when Timeout is UnlimitedTimeout; finite hard-deadline queries cannot run
  // an uninterruptible parent-process callback.
  std::function<std::optional<std::string>(const ConstraintSet &, const Model &)>
      AdditionalModelAudit;
};

class STPSolver final : public Solver {
public:
  explicit STPSolver(STPSolverOptions Options = {});
  ~STPSolver() override;
  STPSolver(const STPSolver &) = delete;
  STPSolver &operator=(const STPSolver &) = delete;

  SolverResult check(const ConstraintSet &Constraints) override;
  SolverResult mayBeTrue(const ConstraintSet &Constraints,
                         const ExprRef &Expression) override;
  SolverResult mustBeTrue(const ConstraintSet &Constraints,
                          const ExprRef &Expression) override;
  SolverResult getValue(const ConstraintSet &Constraints,
                        const ExprRef &Expression) override;
  SolverResult
  getInitialValues(const ConstraintSet &Constraints,
                   const std::vector<ArrayModelRequest> &Arrays) override;

  std::string backendVersion() const;
  std::string backendCompilationProfile() const;
  std::string satBackend() const;

private:
  friend int detail::runSTPWorkerProcess(int, int);
  struct Impl;
  std::unique_ptr<Impl> Implementation;
};

#ifdef FORMAL_SOLVER_TEST_ACCESS
// Fault injection is declared only for the solver unit-test target.  It is not
// part of the supported solver API.
void setSTPSolverWorkerTestMode(int Mode);
bool isPinnedSTPCompilationProfileForTest(const std::string &Profile);
#endif

// Evaluates one scalar Formal Expr using only the supplied Formal model.  Bool
// values are represented as one-bit APInts.  Failure is deterministic and
// reported through Error; it never guesses a value for a missing assignment.
std::optional<llvm::APInt> evaluate(const ExprRef &Expression,
                                    const Model &Assignment,
                                    std::string &Error);

bool validateModel(const ConstraintSet &Constraints, const Model &Assignment,
                   std::string &Error);

} // namespace formal

#endif // FORMAL_SOLVER_H
