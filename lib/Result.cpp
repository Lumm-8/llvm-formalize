#include "formal/Result.h"

namespace formal {

const char *toReasonCode(IncompletenessReason Reason) noexcept {
  switch (Reason) {
  case IncompletenessReason::None:
    return "NONE";
  case IncompletenessReason::UnsupportedConstant:
    return "UNKNOWN-UNSUPPORTED-CONSTANT";
  case IncompletenessReason::UndefValue:
    return "UNKNOWN-UNDEF";
  case IncompletenessReason::PoisonValue:
    return "UNKNOWN-POISON";
  case IncompletenessReason::FreezeInstruction:
    return "UNKNOWN-FREEZE";
  case IncompletenessReason::NoWrapFlags:
    return "UNKNOWN-NSW-NUW";
  case IncompletenessReason::ExactFlag:
    return "UNKNOWN-EXACT";
  case IncompletenessReason::PoisonGeneratingAnnotation:
    return "UNKNOWN-POISON-ANNOTATION";
  case IncompletenessReason::DivideByZero:
    return "UNKNOWN-DIVIDE-BY-ZERO";
  case IncompletenessReason::SignedDivisionOverflow:
    return "UNKNOWN-SDIV-OVERFLOW";
  case IncompletenessReason::Overshift:
    return "UNKNOWN-OVERSHIFT";
  case IncompletenessReason::UnsupportedCast:
    return "UNKNOWN-UNSUPPORTED-CAST";
  case IncompletenessReason::UnsupportedMemoryInstruction:
    return "UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION";
  case IncompletenessReason::UnsupportedCall:
    return "UNKNOWN-UNSUPPORTED-CALL";
  case IncompletenessReason::UnsupportedExceptionInstruction:
    return "UNKNOWN-UNSUPPORTED-EXCEPTION-INSTRUCTION";
  case IncompletenessReason::UnsupportedAtomicInstruction:
    return "UNKNOWN-UNSUPPORTED-ATOMIC-INSTRUCTION";
  case IncompletenessReason::UnsupportedFloatingPoint:
    return "UNKNOWN-UNSUPPORTED-FLOATING-POINT";
  case IncompletenessReason::UnsupportedVector:
    return "UNKNOWN-UNSUPPORTED-VECTOR";
  case IncompletenessReason::UnsupportedAggregate:
    return "UNKNOWN-UNSUPPORTED-AGGREGATE";
  case IncompletenessReason::UnsupportedInstruction:
    return "UNKNOWN-UNSUPPORTED-INSTRUCTION";
  case IncompletenessReason::UnsupportedType:
    return "UNKNOWN-UNSUPPORTED-TYPE";
  case IncompletenessReason::LoopUnsupported:
    return "UNKNOWN-M3-LOOP";
  case IncompletenessReason::InstructionLimit:
    return "UNKNOWN-INSTRUCTION-LIMIT";
  case IncompletenessReason::StateLimit:
    return "UNKNOWN-STATE-LIMIT";
  case IncompletenessReason::SolverTimeout:
    return "UNKNOWN-SOLVER-TIMEOUT";
  case IncompletenessReason::SolverUnknown:
    return "UNKNOWN-SOLVER-UNKNOWN";
  case IncompletenessReason::SolverFailure:
    return "UNKNOWN-SOLVER-FAILURE";
  case IncompletenessReason::UninitializedRead:
    return "UNINITIALIZED_READ";
  case IncompletenessReason::PointerProvenanceLost:
    return "POINTER_PROVENANCE_LOST";
  case IncompletenessReason::PointerResolutionIncomplete:
    return "POINTER_RESOLUTION_INCOMPLETE";
  case IncompletenessReason::PointerComparisonIncomplete:
    return "UNKNOWN-POINTER-COMPARISON";
  case IncompletenessReason::UnsupportedAddressSpace:
    return "UNSUPPORTED_ADDRESS_SPACE";
  case IncompletenessReason::SymbolicAllocationSize:
    return "SYMBOLIC_ALLOCATION_SIZE";
  case IncompletenessReason::ScalableType:
    return "UNKNOWN-SCALABLE-TYPE";
  case IncompletenessReason::UnsupportedGlobal:
    return "UNKNOWN-UNSUPPORTED-GLOBAL";
  case IncompletenessReason::InboundsGEP:
    return "UNKNOWN-INBOUNDS-GEP";
  case IncompletenessReason::AllocationLimit:
    return "UNKNOWN-ALLOCATION-LIMIT";
  case IncompletenessReason::AlignmentUndetermined:
    return "UNKNOWN-ALIGNMENT-UNDETERMINED";
  }
  return "UNKNOWN-INTERNAL-REASON";
}

const char *toString(PathStatus Status) noexcept {
  switch (Status) {
  case PathStatus::Completed:
    return "COMPLETED";
  case PathStatus::Incomplete:
    return "INCOMPLETE";
  case PathStatus::Error:
    return "ERROR";
  case PathStatus::InternalError:
    return "INTERNAL_ERROR";
  }
  return "INTERNAL_ERROR";
}

const char *toString(ExecutionStatus Status) noexcept {
  switch (Status) {
  case ExecutionStatus::Complete:
    return "M3_EXECUTOR_COMPLETE";
  case ExecutionStatus::Unknown:
    return "UNKNOWN";
  case ExecutionStatus::Error:
    return "ERROR";
  case ExecutionStatus::InternalError:
    return "INTERNAL_ERROR";
  }
  return "INTERNAL_ERROR";
}

const char *toReasonCode(PathErrorReason Reason) noexcept {
  switch (Reason) {
  case PathErrorReason::None:
    return "NONE";
  case PathErrorReason::ReachedUnreachable:
    return "REACHED-UNREACHABLE";
  case PathErrorReason::NullDereference:
    return "NULL_DEREFERENCE";
  case PathErrorReason::OutOfBounds:
    return "OUT_OF_BOUNDS";
  case PathErrorReason::ReadOnlyWrite:
    return "READ_ONLY_WRITE";
  case PathErrorReason::DeadObjectAccess:
    return "DEAD_OBJECT_ACCESS";
  case PathErrorReason::MisalignedAccess:
    return "MISALIGNED_ACCESS";
  }
  return "INTERNAL-ERROR-REASON";
}

const char *toReasonCode(InternalErrorReason Reason) noexcept {
  switch (Reason) {
  case InternalErrorReason::None:
    return "NONE";
  case InternalErrorReason::MissingSSABinding:
    return "INTERNAL-MISSING-SSA-BINDING";
  case InternalErrorReason::InvalidPHIPredecessor:
    return "INTERNAL-INVALID-PHI-PREDECESSOR";
  case InternalErrorReason::InvalidSymbolicBinding:
    return "INTERNAL-INVALID-SYMBOLIC-BINDING";
  case InternalErrorReason::NoFeasibleSuccessor:
    return "INTERNAL-NO-FEASIBLE-SUCCESSOR";
  case InternalErrorReason::InvalidExecutorState:
    return "INTERNAL-INVALID-EXECUTOR-STATE";
  case InternalErrorReason::SolverInternalError:
    return "INTERNAL-SOLVER-ERROR";
  case InternalErrorReason::ExpressionConstructionFailure:
    return "INTERNAL-EXPR-CONSTRUCTION";
  case InternalErrorReason::MemoryModelError:
    return "INTERNAL_MEMORY_MODEL_ERROR";
  }
  return "INTERNAL-UNKNOWN-REASON";
}

} // namespace formal
