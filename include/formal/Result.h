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
  IrreducibleCycle,
  LoopMetadataLost,
  LoopBound,
  WallTimeLimit,
  SolverQueryLimit,
  InstructionLimit,
  CallDepthLimit,
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
  AllocationFailureAssumed,
  ScalableType,
  UnsupportedGlobal,
  InboundsGEP,
  AllocationLimit,
  AlignmentUndetermined,
  EntryAttributeConflict,
  RegistrationUnsupported,
  SymbolicRegistrationSize,
  RegistrationName,
  RegistrationOverlap,
  UseBeforeInput,
  RegistrationDescriptorMismatch,
  DeadRegisteredOutput,
};

enum class PathStatus { Completed, Incomplete, Error, InternalError };
enum class ExecutionStatus { Complete, Unknown, Error, InternalError };
enum class PathErrorReason {
  None,
  ReachedUnreachable,
  NoreturnViolation,
  AbortCalled,
  AssertionFailed,
  NullDereference,
  OutOfBounds,
  ReadOnlyWrite,
  DeadObjectAccess,
  UseAfterFree,
  InvalidFree,
  DoubleFree,
  MisalignedAccess,
  OverlappingMemcpy,
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

struct LoopDiagnostic {
  std::uint64_t FrameId = 0;
  std::uint64_t ActivationId = 0;
  std::string StaticLoopId;
  std::string Function;
  std::string Header;
  std::string BoundSource;
  std::uint64_t Bound = 0;
  std::uint64_t TraversedBackedges = 0;
  std::uint64_t AttemptedBackedge = 0;
  std::string SuppressedFrom;
  std::string SuppressedTo;
  IRLocation BoundLocation;
};

struct ExecutionEvent {
  std::string Kind;
  std::string Model;
  std::uint64_t FrameId = 0;
  std::uint64_t ObjectId = 0;
  std::uint64_t PreviousObjectId = 0;
  std::uint64_t Size = 0;
  IRLocation Location;
  std::string Detail;
};

struct CallFrameInfo {
  std::uint64_t FrameId = 0;
  std::string Function;
  IRLocation CallSite;
  IRLocation Continuation;
};

struct ExecutionStatistics {
  std::uint64_t StatesCreated = 0;
  std::uint64_t CompletedPaths = 0;
  std::uint64_t IncompletePaths = 0;
  std::uint64_t ErrorPaths = 0;
  std::uint64_t InternalErrorPaths = 0;
  std::uint64_t Forks = 0;
  std::uint64_t PrunedPaths = 0;
  std::uint64_t AssumptionPrunedPaths = 0;
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
  std::size_t MaximumCallDepthObserved = 0;
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
  bool LoopPolicyError = false;
  bool UnwindingAssumption = false;
  std::optional<LoopDiagnostic> Loop;
  std::vector<LoopDiagnostic> LoopBounds;
  std::string LimitName;
  std::optional<std::uint64_t> LimitValue;
  std::optional<std::uint64_t> ObservedValue;
  std::vector<CallFrameInfo> CallStack;
  std::vector<ExecutionEvent> Events;
  struct RegisteredRegion {
    bool Input = false;
    std::string Name;
    std::uint64_t Size = 0;
    std::uint64_t ObjectId = 0;
    std::uint64_t Offset = 0;
    IRLocation RegistrationLocation;
    ConstraintSet RegistrationGuards;
    ExprRef CandidateGuard;
    std::vector<ExprRef> Bytes;
  };
  std::vector<RegisteredRegion> Regions;
};

struct ExecutionResult {
  ExecutionStatus Status = ExecutionStatus::InternalError;
  std::vector<PathResult> Paths;
  ExecutionStatistics Statistics;
  bool HasLoopPolicyError = false;
  bool HasUnwindingAssumption = false;
};

} // namespace formal

#endif // FORMAL_RESULT_H
