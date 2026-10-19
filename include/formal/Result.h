#ifndef FORMAL_RESULT_H
#define FORMAL_RESULT_H

#include "formal/Solver.h"
#include "formal/Value.h"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace formal {

enum class IncompletenessReason {
  None,
  UnsupportedConstant,
  UndefValue,
  PoisonValue,
  FreezeInstruction,
  NoWrapFlags,
  ExactFlag,
  PoisonGeneratingAnnotation,
  DivideByZero,
  SignedDivisionOverflow,
  Overshift,
  UnsupportedCast,
  UnsupportedMemoryInstruction,
  UnsupportedCall,
  UnsupportedExceptionInstruction,
  UnsupportedAtomicInstruction,
  UnsupportedFloatingPoint,
  UnsupportedVector,
  UnsupportedAggregate,
  UnsupportedInstruction,
  UnsupportedType,
  LoopUnsupported,
  InstructionLimit,
  StateLimit,
  SolverTimeout,
  SolverUnknown,
  SolverFailure,
  UninitializedRead,
  PointerProvenanceLost,
  PointerResolutionIncomplete,
  PointerComparisonIncomplete,
  UnsupportedAddressSpace,
  SymbolicAllocationSize,
  ScalableType,
  UnsupportedGlobal,
  InboundsGEP,
  AllocationLimit,
  AlignmentUndetermined,
};

enum class PathStatus { Completed, Incomplete, Error, InternalError };
enum class ExecutionStatus { Complete, Unknown, Error, InternalError };
enum class PathErrorReason {
  None,
  ReachedUnreachable,
  NullDereference,
  OutOfBounds,
  ReadOnlyWrite,
  DeadObjectAccess,
  MisalignedAccess,
};
enum class InternalErrorReason {
  None,
  MissingSSABinding,
  InvalidPHIPredecessor,
  InvalidSymbolicBinding,
  NoFeasibleSuccessor,
  InvalidExecutorState,
  SolverInternalError,
  ExpressionConstructionFailure,
  MemoryModelError,
};

const char *toReasonCode(IncompletenessReason Reason) noexcept;
const char *toString(PathStatus Status) noexcept;
const char *toString(ExecutionStatus Status) noexcept;
const char *toReasonCode(PathErrorReason Reason) noexcept;
const char *toReasonCode(InternalErrorReason Reason) noexcept;

struct IRLocation {
  std::string Function;
  std::string BasicBlock;
  std::string Opcode;
  std::uint64_t InstructionOrdinal = 0;
  std::string SourceFile;
  unsigned SourceLine = 0;
  unsigned SourceColumn = 0;
};

struct ExecutionStatistics {
  std::uint64_t StatesCreated = 0;
  std::uint64_t CompletedPaths = 0;
  std::uint64_t IncompletePaths = 0;
  std::uint64_t ErrorPaths = 0;
  std::uint64_t InternalErrorPaths = 0;
  std::uint64_t Forks = 0;
  std::uint64_t PrunedPaths = 0;
  std::uint64_t ExecutedInstructions = 0;
  std::uint64_t SolverQueries = 0;
  std::uint64_t MemoryObjects = 0;
  std::vector<std::uint64_t> AllocatedObjectIds;
  std::uint64_t ObjectStates = 0;
  std::uint64_t COWCopies = 0;
  std::uint64_t ByteReadExprs = 0;
  std::uint64_t ByteWriteExprs = 0;
  std::uint64_t PointerResolutions = 0;
  std::uint64_t PointerCandidates = 0;
  std::uint64_t PointerForks = 0;
  std::uint64_t PointerCapHits = 0;
  std::uint64_t ResolutionSolverQueries = 0;
  std::uint64_t ResolutionSolverMicroseconds = 0;
  std::uint64_t ASTNodes = 0;
  std::uint64_t MaxUpdateListLength = 0;
  std::uint64_t PeakResidentBytes = 0;
};

struct PathResult {
  std::uint64_t StateId = 0;
  unsigned Depth = 0;
  std::uint64_t ExecutedInstructions = 0;
  PathStatus Status = PathStatus::InternalError;
  std::optional<SymbolicValue> ReturnValue;
  ConstraintSet Constraints;
  std::optional<Model> ErrorModel;
  IRLocation Location;
  IncompletenessReason IncompleteReason = IncompletenessReason::None;
  PathErrorReason ErrorReason = PathErrorReason::None;
  InternalErrorReason InternalReason = InternalErrorReason::None;
  std::string Detail;
};

struct ExecutionResult {
  ExecutionStatus Status = ExecutionStatus::InternalError;
  std::vector<PathResult> Paths;
  ExecutionStatistics Statistics;
};

} // namespace formal

#endif // FORMAL_RESULT_H
