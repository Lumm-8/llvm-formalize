#include "formal/Executor.h"
#include "formal/ExternalFunctionModel.h"

#include "llvm/ADT/APInt.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/ModRef.h"

#include <algorithm>
#include <chrono>
#include <functional>
#include <limits>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <sys/resource.h>
#include <utility>
#include <vector>

namespace formal {

const char *toString(LoopLimitPolicy Policy) noexcept {
  switch (Policy) {
  case LoopLimitPolicy::Unknown:
    return "unknown";
  case LoopLimitPolicy::Error:
    return "error";
  case LoopLimitPolicy::AssumeExit:
    return "assume-exit";
  }
  return "unknown";
}

void DFSWorklist::schedule(ExecutionState State) {
  States.push_back(std::move(State));
}

ExecutionState DFSWorklist::takeNext() {
  if (States.empty())
    throw std::logic_error("cannot take a state from an empty DFS worklist");
  ExecutionState Result = std::move(States.back());
  States.pop_back();
  return Result;
}

namespace {

enum class QueryDisposition { Feasible, Infeasible, Incomplete, InternalError };

struct QueryOutcome {
  QueryDisposition Disposition = QueryDisposition::InternalError;
  IncompletenessReason Reason = IncompletenessReason::None;
  std::string Detail;
};

struct GuardedDestination {
  ExprRef Guard;
  llvm::BasicBlock *Destination = nullptr;
};

std::string blockName(const llvm::BasicBlock *Block) {
  if (!Block)
    return {};
  if (Block->hasName())
    return Block->getName().str();
  unsigned Index = 0;
  for (const llvm::BasicBlock &Candidate : *Block->getParent()) {
    if (&Candidate == Block)
      return "bb" + std::to_string(Index);
    ++Index;
  }
  return "bb?";
}

IRLocation locationOf(const llvm::Instruction *Instruction) {
  IRLocation Result;
  if (!Instruction)
    return Result;
  const llvm::Function *Function = Instruction->getFunction();
  Result.Function = Function ? Function->getName().str() : std::string();
  Result.BasicBlock = blockName(Instruction->getParent());
  Result.Opcode = Instruction->getOpcodeName();
  if (Function) {
    std::uint64_t Ordinal = 0;
    bool Found = false;
    for (const llvm::BasicBlock &Block : *Function) {
      for (const llvm::Instruction &Candidate : Block) {
        if (&Candidate == Instruction) {
          Found = true;
          break;
        }
        ++Ordinal;
      }
      if (Found)
        break;
    }
    Result.InstructionOrdinal = Ordinal;
  }
  if (const llvm::DebugLoc &Debug = Instruction->getDebugLoc()) {
    Result.SourceLine = Debug.getLine();
    Result.SourceColumn = Debug.getCol();
    Result.SourceFile = Debug->getFilename().str();
  }
  return Result;
}

bool hasFloatingType(const llvm::Instruction &Instruction) {
  if (Instruction.getType()->isFloatingPointTy())
    return true;
  for (const llvm::Use &Operand : Instruction.operands())
    if (Operand->getType()->isFloatingPointTy())
      return true;
  return false;
}

bool hasVectorType(const llvm::Instruction &Instruction) {
  if (Instruction.getType()->isVectorTy())
    return true;
  for (const llvm::Use &Operand : Instruction.operands())
    if (Operand->getType()->isVectorTy())
      return true;
  return false;
}

bool hasAggregateType(const llvm::Instruction &Instruction) {
  if (Instruction.getType()->isAggregateType())
    return true;
  for (const llvm::Use &Operand : Instruction.operands())
    if (Operand->getType()->isAggregateType())
      return true;
  return false;
}

std::optional<std::uint64_t> fixedSize(llvm::TypeSize Size) {
  if (Size.isScalable())
    return std::nullopt;
  return Size.getFixedValue();
}

bool checkedAdd(std::uint64_t Left, std::uint64_t Right,
                std::uint64_t &Result) {
  if (Right > std::numeric_limits<std::uint64_t>::max() - Left)
    return false;
  Result = Left + Right;
  return true;
}

bool checkedMul(std::uint64_t Left, std::uint64_t Right,
                std::uint64_t &Result) {
  if (Right && Left > std::numeric_limits<std::uint64_t>::max() / Right)
    return false;
  Result = Left * Right;
  return true;
}

template <typename GEPType>
bool supportedGEPNoWrap(const GEPType &GEP) {
  const unsigned Flags = GEP.getNoWrapFlags().getRaw();
  if (Flags == 0 || Flags == llvm::GEPNoWrapFlags::inBounds().getRaw())
    return true;
  // A zero displacement cannot wrap at either the pointer or index width.
  for (const llvm::Use &Index : GEP.indices()) {
    const auto *Constant = llvm::dyn_cast<llvm::ConstantInt>(Index.get());
    if (!Constant || !Constant->isZero())
      return false;
  }
  return true;
}

const llvm::Function *resolveExactFunctionAlias(const llvm::Value *Target,
                                                std::string &Failure) {
  std::set<const llvm::GlobalAlias *> Chain;
  while (Target) {
    Target = Target->stripPointerCasts();
    const auto *Alias = llvm::dyn_cast<llvm::GlobalAlias>(Target);
    if (!Alias)
      break;
    if (!Chain.insert(Alias).second) {
      Failure = "cyclic function alias chain";
      return nullptr;
    }
    if (!Alias->hasExactDefinition() || Alias->isInterposable()) {
      Failure = "function alias lacks an exact, non-interposable definition: " +
                Alias->getName().str();
      return nullptr;
    }
    Target = Alias->getAliasee();
  }
  if (const auto *Function = llvm::dyn_cast_or_null<llvm::Function>(Target))
    return Function;
  Failure = "alias or call target does not resolve to a direct function";
  return nullptr;
}

std::optional<std::uint64_t> concreteUnsigned(const ExprRef &Expression) {
  if (!Expression || !Expression->isConstant() ||
      Expression->constantValue().ugt(
          std::numeric_limits<std::uint64_t>::max()))
    return std::nullopt;
  return Expression->constantValue().getZExtValue();
}

bool supportedMemcpyParamAttribute(unsigned Arg, llvm::Attribute Attr) {
  if (!Attr.hasKindAsEnum())
    return false;
  switch (Attr.getKindAsEnum()) {
  case llvm::Attribute::Alignment:
  case llvm::Attribute::NoAlias:
  case llvm::Attribute::NoCapture:
    return Arg < 2;
  case llvm::Attribute::WriteOnly:
    return Arg == 0;
  case llvm::Attribute::ReadOnly:
    return Arg == 1;
  case llvm::Attribute::ImmArg:
    return Arg == 3;
  case llvm::Attribute::NoUndef:
    return true;
  default:
    return false;
  }
}

bool hasUnsupportedMemcpyParamAttributes(const llvm::MemTransferInst &Copy) {
  const llvm::Function *Callee = Copy.getCalledFunction();
  if (!Callee)
    return true;
  for (unsigned Arg = 0; Arg < Copy.arg_size(); ++Arg) {
    for (llvm::Attribute Attr : Copy.getAttributes().getParamAttrs(Arg))
      if (!supportedMemcpyParamAttribute(Arg, Attr))
        return true;
    for (llvm::Attribute Attr : Callee->getAttributes().getParamAttrs(Arg))
      if (!supportedMemcpyParamAttribute(Arg, Attr))
        return true;
  }
  return false;
}

} // namespace

struct ScalarExecutor::Impl {
  struct LoopAnalysis {
    struct BoundDefinition {
      std::uint64_t Bound = 0;
      std::string Source = "global-default";
      IncompletenessReason Error = IncompletenessReason::None;
      std::string Detail;
      IRLocation MetadataLocation;
    };
    llvm::DominatorTree Dominators;
    llvm::LoopInfo Loops;
    std::set<const llvm::BasicBlock *> IrreducibleBlocks;
    std::map<const llvm::Loop *, BoundDefinition> Bounds;
    explicit LoopAnalysis(llvm::Function &Function) {
      Dominators.recalculate(Function);
      Loops.analyze(Dominators);
      // A reducible cycle has a LoopInfo latch-to-header edge. Remove every
      // such edge and find SCCs again: any remaining cycle is not represented
      // by the natural-loop forest, even when a larger SCC has one entry.
      auto IsNaturalBackedge = [&](llvm::BasicBlock *From,
                                   llvm::BasicBlock *To) {
        llvm::Loop *Loop = Loops.getLoopFor(To);
        return Loop && Loop->getHeader() == To && Loop->contains(From) &&
               Loop->isLoopLatch(From);
      };
      std::map<llvm::BasicBlock *, unsigned> Discovery, Lowlink;
      std::vector<llvm::BasicBlock *> Stack;
      std::set<llvm::BasicBlock *> OnStack;
      unsigned NextIndex = 0;
      std::function<void(llvm::BasicBlock *)> Visit =
          [&](llvm::BasicBlock *Block) {
            Discovery[Block] = Lowlink[Block] = NextIndex++;
            Stack.push_back(Block);
            OnStack.insert(Block);
            for (llvm::BasicBlock *Successor : llvm::successors(Block)) {
              if (IsNaturalBackedge(Block, Successor))
                continue;
              if (!Discovery.count(Successor)) {
                Visit(Successor);
                Lowlink[Block] = std::min(Lowlink[Block], Lowlink[Successor]);
              } else if (OnStack.count(Successor)) {
                Lowlink[Block] =
                    std::min(Lowlink[Block], Discovery[Successor]);
              }
            }
            if (Lowlink[Block] != Discovery[Block])
              return;
            std::vector<llvm::BasicBlock *> Component;
            llvm::BasicBlock *Member;
            do {
              Member = Stack.back();
              Stack.pop_back();
              OnStack.erase(Member);
              Component.push_back(Member);
            } while (Member != Block);
            bool Cyclic = Component.size() > 1;
            if (!Cyclic)
              for (llvm::BasicBlock *Successor : llvm::successors(Block))
                Cyclic |= Successor == Block &&
                          !IsNaturalBackedge(Block, Successor);
            if (Cyclic)
              IrreducibleBlocks.insert(Component.begin(), Component.end());
          };
      for (llvm::BasicBlock &Block : Function)
        if (!Discovery.count(&Block))
          Visit(&Block);
    }
  };

  Impl(ExprContext &Expressions, Solver &Solver, ExecutorOptions Options)
      : Expressions(Expressions), SolverBackend(Solver), Options(Options) {
    if (Options.MaxInstructionsPerState == 0 || Options.MaxStates == 0 ||
        Options.MaxCallDepth == 0)
      throw std::invalid_argument("executor state, instruction, and call limits must be positive");
  }

  void reset() {
    RunStart = std::chrono::steady_clock::now();
    Worklist.clear();
    Paths.clear();
    Statistics = {};
    NextStateId = 1;
    NextFrameId = 1;
    NextActivationId = 1;
    NextObjectId = 1;
    NextLogicalBase = 0x10000;
    GlobalObjects.clear();
    LoopAnalyses.clear();
  }

  void append(PathResult Result) {
    switch (Result.Status) {
    case PathStatus::Completed:
      ++Statistics.CompletedPaths;
      break;
    case PathStatus::Incomplete:
      ++Statistics.IncompletePaths;
      break;
    case PathStatus::Error:
      ++Statistics.ErrorPaths;
      break;
    case PathStatus::InternalError:
      ++Statistics.InternalErrorPaths;
      break;
    }
    Paths.push_back(std::move(Result));
  }

  LoopDiagnostic loopDiagnostic(const ExecutionState &State,
                                const llvm::BasicBlock *Header,
                                const StackFrame::LoopActivation &Activation) const {
    LoopDiagnostic Result;
    Result.FrameId = State.frame().id();
    Result.ActivationId = Activation.Id;
    Result.Function = Header->getParent()->getName().str();
    Result.Header = blockName(Header);
    Result.StaticLoopId = Result.Function + ":" + Result.Header;
    Result.BoundSource = Activation.BoundSource;
    Result.Bound = Activation.Bound;
    Result.TraversedBackedges = Activation.Traversals;
    auto Analysis = LoopAnalyses.find(Header->getParent());
    if (Analysis != LoopAnalyses.end()) {
      llvm::Loop *Loop = Analysis->second->Loops.getLoopFor(Header);
      auto Bound = Analysis->second->Bounds.find(Loop);
      if (Bound != Analysis->second->Bounds.end())
        Result.BoundLocation = Bound->second.MetadataLocation;
    }
    return Result;
  }

  PathResult basePath(const ExecutionState &State, PathStatus Status,
                      IRLocation Location, std::string Detail) const {
    PathResult Result;
    Result.StateId = State.id();
    Result.Depth = State.depth();
    Result.ExecutedInstructions = State.executedInstructions();
    Result.Status = Status;
    Result.Constraints = State.constraints().constraints();
    if (Options.EnableRegistration)
      Result.Regions = State.regions();
    Result.Location = std::move(Location);
    Result.Detail = std::move(Detail);
    Result.LoopPolicyError = State.loopPolicyError();
    Result.UnwindingAssumption = State.unwindingAssumption();
    Result.LoopBounds = State.loopHistory();
    Result.Loop = State.loopDiagnostic();
    if (!Result.Loop && State.pc().block()) {
      auto Analysis = LoopAnalyses.find(State.pc().function());
      if (Analysis != LoopAnalyses.end())
        for (llvm::Loop *Loop = Analysis->second->Loops.getLoopFor(State.pc().block());
             Loop; Loop = Loop->getParentLoop()) {
          auto Activation =
              State.frame().loopActivations().find(Loop->getHeader());
          if (Activation != State.frame().loopActivations().end()) {
            Result.Loop = loopDiagnostic(State, Loop->getHeader(),
                                         Activation->second);
            break;
          }
        }
    }
    Result.Events = State.events();
    for (const StackFrame &Frame : State.frames()) {
      CallFrameInfo Info;
      Info.FrameId = Frame.id();
      if (Frame.function())
        Info.Function = Frame.function()->getName().str();
      if (Frame.callSite())
        Info.CallSite = locationOf(Frame.callSite());
      if (Frame.continuation().instruction())
        Info.Continuation = locationOf(Frame.continuation().instruction());
      Result.CallStack.push_back(std::move(Info));
    }
    return Result;
  }

  void recordEvent(ExecutionState &State, std::string Kind,
                   std::string Model = {}, std::uint64_t ObjectId = 0,
                   std::uint64_t PreviousObjectId = 0,
                   std::uint64_t Size = 0, std::string Detail = {}) {
    ExecutionEvent Event;
    Event.Kind = std::move(Kind);
    Event.Model = std::move(Model);
    Event.FrameId = State.frame().id();
    Event.ObjectId = ObjectId;
    Event.PreviousObjectId = PreviousObjectId;
    Event.Size = Size;
    Event.Location = locationOf(State.pc().instruction());
    Event.Detail = std::move(Detail);
    State.events().push_back(std::move(Event));
  }

  void incomplete(ExecutionState &State, IncompletenessReason Reason,
                  std::string Detail,
                  IRLocation Location = IRLocation()) {
    if (Location.Function.empty())
      Location = locationOf(State.pc().instruction());
    State.setIncomplete(Reason, Location, Detail);
    PathResult Result =
        basePath(State, PathStatus::Incomplete, std::move(Location),
                 std::move(Detail));
    Result.IncompleteReason = Reason;
    auto SetLimit = [&](const char *Name, std::uint64_t Limit,
                        std::uint64_t Observed) {
      Result.LimitName = Name;
      Result.LimitValue = Limit;
      Result.ObservedValue = Observed;
    };
    switch (Reason) {
    case IncompletenessReason::LoopBound:
      if (Result.Loop)
        SetLimit("backedge_traversals", Result.Loop->Bound,
                 Result.Loop->AttemptedBackedge);
      break;
    case IncompletenessReason::InstructionLimit:
      SetLimit("instructions_per_state", Options.MaxInstructionsPerState,
               State.executedInstructions());
      break;
    case IncompletenessReason::CallDepthLimit:
      SetLimit("call_depth", Options.MaxCallDepth, State.frames().size());
      break;
    case IncompletenessReason::StateLimit:
      SetLimit("states", Options.MaxStates, Statistics.StatesCreated);
      break;
    case IncompletenessReason::WallTimeLimit:
      SetLimit("wall_time_ms", Options.MaxWallTimeMs,
               static_cast<std::uint64_t>(
                   std::chrono::duration_cast<std::chrono::milliseconds>(
                       std::chrono::steady_clock::now() - RunStart)
                       .count()));
      break;
    case IncompletenessReason::SolverQueryLimit:
      SetLimit("solver_queries", Options.MaxSolverQueries,
               Statistics.SolverQueries);
      break;
    case IncompletenessReason::SolverTimeout:
      SetLimit("solver_timeout_ms", Options.SolverTimeoutMs,
               Options.SolverTimeoutMs);
      break;
    case IncompletenessReason::AllocationLimit:
      Result.LimitName = "object_bytes";
      Result.LimitValue = Options.MaxObjectBytes;
      break;
    case IncompletenessReason::PointerResolutionIncomplete:
      Result.LimitName = "pointer_candidates";
      Result.LimitValue = Options.MaxPointerCandidates;
      Result.ObservedValue = Statistics.PointerCandidates;
      break;
    default:
      break;
    }
    append(std::move(Result));
  }

  void assumeAllocationSuccess(ExecutionState &State,
                               llvm::StringRef Model) {
    if (Options.EnableRegistration) {
      ExecutionState Failure = State;
      Failure.setId(allocateStateId());
      ++Statistics.Forks;
      recordEvent(Failure, "allocation_failure_unmodeled", Model.str());
      incomplete(Failure, IncompletenessReason::AllocationFailureAssumed,
                 "allocation failure was not explored for " + Model.str());
    }
    recordEvent(State, "allocation_success_assumption", Model.str());
  }

  void internalError(ExecutionState &State, InternalErrorReason Reason,
                     std::string Detail,
                     IRLocation Location = IRLocation()) {
    if (Location.Function.empty())
      Location = locationOf(State.pc().instruction());
    State.setStatus(ExecutionStateStatus::Error);
    State.setTerminationLocation(Location, Detail);
    PathResult Result =
        basePath(State, PathStatus::InternalError, std::move(Location),
                 std::move(Detail));
    Result.InternalReason = Reason;
    append(std::move(Result));
  }

  void pathError(ExecutionState &State, PathErrorReason Reason,
                 std::string Detail, std::uint64_t ObjectId = 0) {
    IRLocation Location = locationOf(State.pc().instruction());
    std::optional<Model> ErrorModel;
    if (Reason != PathErrorReason::ReachedUnreachable ||
        Options.EnableRegistration) {
      if (Statistics.SolverQueries >= Options.MaxSolverQueries) {
        incomplete(State, IncompletenessReason::SolverQueryLimit,
                   "solver query limit reached before error witness", Location);
        return;
      }
      ++Statistics.SolverQueries;
      SolverResult Witness =
          SolverBackend.check(State.constraints().constraints());
      if (Witness.Status == SolverStatus::Timeout) {
        incomplete(State, IncompletenessReason::SolverTimeout,
                   "memory error witness query timed out", Location);
        return;
      }
      if (Witness.Status == SolverStatus::Unknown) {
        incomplete(State,
                   Witness.Metadata.Reason == SolverReason::WorkerFailure
                       ? IncompletenessReason::SolverFailure
                       : IncompletenessReason::SolverUnknown,
                   "memory error witness query returned UNKNOWN", Location);
        return;
      }
      if (Witness.Status != SolverStatus::Sat ||
          !Witness.SatisfyingModel.has_value()) {
        internalError(State, InternalErrorReason::SolverInternalError,
                      "feasible memory error has no SAT witness model",
                      Location);
        return;
      }
      std::string ValidationError;
      if (!validateModel(State.constraints().constraints(),
                         *Witness.SatisfyingModel, ValidationError)) {
        internalError(State, InternalErrorReason::SolverInternalError,
                      "memory error witness model is invalid: " +
                          ValidationError,
                      Location);
        return;
      }
      ErrorModel = std::move(Witness.SatisfyingModel);
    }
    State.setStatus(ExecutionStateStatus::Error);
    State.setTerminationLocation(Location, Detail);
    if (Reason == PathErrorReason::UseAfterFree ||
        Reason == PathErrorReason::InvalidFree ||
        Reason == PathErrorReason::DoubleFree)
      recordEvent(State, "heap_error", {}, ObjectId, 0, 0,
                  toReasonCode(Reason));
    PathResult Result = basePath(State, PathStatus::Error, std::move(Location),
                                 std::move(Detail));
    Result.ErrorReason = Reason;
    Result.ErrorModel = std::move(ErrorModel);
    append(std::move(Result));
  }

  void completed(ExecutionState &State,
                 std::optional<SymbolicValue> ReturnValue) {
    IRLocation Location = locationOf(State.pc().instruction());
    State.setStatus(ExecutionStateStatus::Completed);
    State.setTerminationLocation(Location, "normal scalar return");
    PathResult Result = basePath(State, PathStatus::Completed,
                                 std::move(Location), "normal scalar return");
    Result.ReturnValue = std::move(ReturnValue);
    append(std::move(Result));
  }

  std::uint64_t allocateStateId() {
    ++Statistics.StatesCreated;
    return NextStateId++;
  }

  QueryOutcome query(const ExecutionState &State, const ExprRef &Guard) {
    if (Statistics.SolverQueries >= Options.MaxSolverQueries)
      return {QueryDisposition::Incomplete,
              IncompletenessReason::SolverQueryLimit,
              "solver query limit reached"};
    ++Statistics.SolverQueries;
    SolverResult Result =
        SolverBackend.mayBeTrue(State.constraints().constraints(), Guard);
    switch (Result.Status) {
    case SolverStatus::Sat:
      if (!Result.Truth || !*Result.Truth)
        return {QueryDisposition::InternalError,
                IncompletenessReason::None,
                "SAT mayBeTrue result omitted a true Truth payload"};
      return {QueryDisposition::Feasible, IncompletenessReason::None, {}};
    case SolverStatus::Unsat:
      if (!Result.Truth || *Result.Truth)
        return {QueryDisposition::InternalError,
                IncompletenessReason::None,
                "UNSAT mayBeTrue result omitted a false Truth payload"};
      return {QueryDisposition::Infeasible, IncompletenessReason::None, {}};
    case SolverStatus::Timeout:
      return {QueryDisposition::Incomplete,
              IncompletenessReason::SolverTimeout,
              Result.Metadata.Detail.empty() ? "solver query timed out"
                                             : Result.Metadata.Detail};
    case SolverStatus::Unknown:
      return {QueryDisposition::Incomplete,
              Result.Metadata.Reason == SolverReason::WorkerFailure
                  ? IncompletenessReason::SolverFailure
                  : IncompletenessReason::SolverUnknown,
              Result.Metadata.Detail.empty() ? "solver returned UNKNOWN"
                                             : Result.Metadata.Detail};
    case SolverStatus::InternalError:
      return {QueryDisposition::InternalError,
              IncompletenessReason::None,
              Result.Metadata.Detail.empty() ? "solver internal error"
                                             : Result.Metadata.Detail};
    }
    return {QueryDisposition::InternalError, IncompletenessReason::None,
            "invalid solver status"};
  }

  const llvm::DataLayout &layout(const ExecutionState &State) const {
    return State.pc().function()->getParent()->getDataLayout();
  }

  bool pointerObjectsBelongToState(const PointerValue &Pointer,
                                   const ExecutionState &State) const {
    for (const PointerCandidate &Candidate : Pointer.Candidates) {
      if (!Candidate.Object)
        continue;
      const AddressSpace::Entry *Entry =
          State.addressSpace().find(Candidate.Object->Id);
      if (!Entry || Entry->Object.get() != Candidate.Object.get())
        return false;
    }
    return true;
  }

  std::shared_ptr<const MemoryObject>
  allocateObject(ExecutionState &State, AllocationKind Kind,
                 std::uint64_t Size, std::uint64_t Alignment,
                 std::string Site, bool ReadOnly,
                 bool MergeableGlobalAddress = false,
                 bool ExternallyInitialized = false,
                 std::uint64_t OwnerFrameId = 0) {
    if (!Alignment || (Alignment & (Alignment - 1))) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "object alignment is not a positive power of two");
      return nullptr;
    }
    if (Size > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "object exceeds M4 byte limit");
      return nullptr;
    }
    unsigned Width = layout(State).getPointerSizeInBits(0);
    if (!Width || Width > 64) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "M4 supports default pointers up to 64 bits");
      return nullptr;
    }
    std::uint64_t Aligned = 0;
    if (!checkedAdd(NextLogicalBase, Alignment - 1, Aligned)) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "logical address allocator overflow");
      return nullptr;
    }
    Aligned &= ~(Alignment - 1);
    std::uint64_t End = 0;
    if (!checkedAdd(Aligned, Size, End) ||
        !checkedAdd(End, 1, NextLogicalBase) ||
        (Width < 64 && NextLogicalBase >= (std::uint64_t{1} << (Width - 1))) ||
        (Width == 64 && NextLogicalBase >= (std::uint64_t{1} << 63))) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "logical address space exhausted");
      return nullptr;
    }
    auto Object = std::make_shared<MemoryObject>();
    Object->Id = NextObjectId++;
    Object->OwnerFrameId = Kind == AllocationKind::Stack
                               ? (OwnerFrameId ? OwnerFrameId : State.frame().id())
                               : 0;
    Object->Kind = Kind;
    Object->Size = Size;
    Object->Alignment = Alignment;
    Object->LogicalBase = Aligned;
    Object->AllocationSite = std::move(Site);
    Object->ReadOnly = ReadOnly;
    Object->MergeableGlobalAddress = MergeableGlobalAddress;
    Object->ExternallyInitialized = ExternallyInitialized;
    auto ObjectStateValue =
        std::make_shared<ObjectState>(Object, Expressions, Width);
    if (!State.addressSpace().bind(Object, std::move(ObjectStateValue))) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "duplicate memory object identity");
      return nullptr;
    }
    ++Statistics.MemoryObjects;
    Statistics.AllocatedObjectIds.push_back(Object->Id);
    ++Statistics.ObjectStates;
    recordEvent(State, Kind == AllocationKind::Stack ? "stack_allocation"
                     : Kind == AllocationKind::Global ? "global_allocation"
                     : "heap_allocation",
                {}, Object->Id, 0, Size);
    if (Kind == AllocationKind::Stack)
      State.events().back().FrameId = Object->OwnerFrameId;
    return Object;
  }

  ExprRef byteOffset(ExprRef Offset, std::uint64_t Delta) {
    if (Delta == 0)
      return Offset;
    return Expressions.add(Offset,
                           Expressions.constant(Offset->width(), Delta));
  }

  void writeIntegerBytes(ObjectState &Object, ExprRef Offset, ExprRef Value,
                         std::uint64_t Size, bool LittleEndian,
                         bool Initializer = false,
                         ExprRef AddressSensitive = nullptr) {
    if (!Value || !Value->sort().isBitVector() ||
        Size > std::numeric_limits<unsigned>::max() / 8 ||
        Value->width() > Size * 8)
      throw std::invalid_argument("invalid integer memory store size");
    unsigned StorageWidth = static_cast<unsigned>(Size * 8);
    ExprRef Stored = Value->width() == StorageWidth
                         ? Value
                         : Expressions.zext(Value, StorageWidth);
    for (std::uint64_t Byte = 0; Byte < Size; ++Byte) {
      const std::uint64_t LogicalByte =
          LittleEndian ? Byte : Size - 1 - Byte;
      ExprRef Part = Expressions.extract(
          Stored, static_cast<unsigned>(LogicalByte * 8), 8);
      Object.writeByte(byteOffset(Offset, Byte), Part, Expressions,
                       Initializer, AddressSensitive);
      const std::uint64_t Remaining =
          Value->width() > LogicalByte * 8
              ? Value->width() - LogicalByte * 8
              : 0;
      if (Remaining < 8) {
        const std::uint8_t Mask =
            Remaining == 0
                ? 0
                : static_cast<std::uint8_t>((1U << Remaining) - 1);
        Object.setKnownMask(byteOffset(Offset, Byte), Mask, Expressions);
      }
      ++Statistics.ByteWriteExprs;
    }
    Statistics.MaxUpdateListLength =
        std::max<std::uint64_t>(Statistics.MaxUpdateListLength,
                                Object.bytes().size());
  }

  ExprRef readIntegerBytes(const ObjectState &Object, ExprRef Offset,
                           std::uint64_t Size, unsigned Width,
                           bool LittleEndian) {
    if (!Size || Size > std::numeric_limits<unsigned>::max() / 8 ||
        Width > Size * 8)
      throw std::invalid_argument("invalid integer memory load size");
    ExprRef Packed;
    for (std::uint64_t Byte = 0; Byte < Size; ++Byte) {
      ExprRef Part = Object.readByte(byteOffset(Offset, Byte), Expressions);
      ++Statistics.ByteReadExprs;
      if (!Packed)
        Packed = Part;
      else
        Packed = LittleEndian ? Expressions.concat(Part, Packed)
                              : Expressions.concat(Packed, Part);
    }
    return Width == Size * 8 ? Packed : Expressions.trunc(Packed, Width);
  }

  bool initializeGlobals(ExecutionState &State) {
    llvm::Module &Module = *State.pc().function()->getParent();
    const llvm::DataLayout &DL = Module.getDataLayout();
    std::set<const llvm::GlobalVariable *> Needed;
    std::set<const llvm::Value *> Seen;
    std::vector<const llvm::Value *> Pending;
    for (const llvm::BasicBlock &Block : *State.pc().function())
      for (const llvm::Instruction &Instruction : Block)
        for (const llvm::Use &Operand : Instruction.operands())
          Pending.push_back(Operand.get());
    while (!Pending.empty()) {
      const llvm::Value *Value = Pending.back();
      Pending.pop_back();
      if (!Value || !Seen.insert(Value).second)
        continue;
      if (Seen.size() > (1U << 20)) {
        incomplete(State, IncompletenessReason::AllocationLimit,
                   "global dependency traversal limit reached");
        return false;
      }
      if (const auto *Alias = llvm::dyn_cast<llvm::GlobalAlias>(Value)) {
        std::string Failure;
        const llvm::Function *Function =
            resolveExactFunctionAlias(Alias, Failure);
        if (!Function) {
          incomplete(State, IncompletenessReason::UnsupportedGlobal,
                     std::move(Failure));
          return false;
        }
        Pending.push_back(Function);
        continue;
      }
      if (const auto *Global = llvm::dyn_cast<llvm::GlobalVariable>(Value)) {
        Needed.insert(Global);
        if (Global->hasInitializer() && !Global->isExternallyInitialized())
          Pending.push_back(Global->getInitializer());
      } else if (const auto *Function = llvm::dyn_cast<llvm::Function>(Value)) {
        if (!Function->isDeclaration())
          for (const llvm::BasicBlock &Block : *Function)
            for (const llvm::Instruction &Instruction : Block)
              for (const llvm::Use &Operand : Instruction.operands())
                Pending.push_back(Operand.get());
      } else if (const auto *Constant = llvm::dyn_cast<llvm::Constant>(Value)) {
        if (const auto *Expression =
                llvm::dyn_cast<llvm::ConstantExpr>(Constant);
            Expression &&
            Expression->getOpcode() != llvm::Instruction::GetElementPtr)
          continue;
        for (const llvm::Use &Operand : Constant->operands())
          Pending.push_back(Operand.get());
      }
    }
    for (llvm::GlobalVariable &Global : Module.globals()) {
      if (!Needed.count(&Global))
        continue;
      if (Global.getAddressSpace() != 0) {
        incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                   "global variable uses a non-default address space");
        return false;
      }
      if (Global.isDeclaration() || !Global.hasInitializer()) {
        incomplete(State, IncompletenessReason::UnsupportedGlobal,
                   "external or uninitialized global is unsupported");
        return false;
      }
      auto Size = fixedSize(DL.getTypeAllocSize(Global.getValueType()));
      if (!Size) {
        incomplete(State, IncompletenessReason::ScalableType,
                   "global has scalable allocation size");
        return false;
      }
      std::uint64_t Alignment =
          Global.getAlign().value_or(DL.getABITypeAlign(Global.getValueType()))
              .value();
      auto Object = allocateObject(State, AllocationKind::Global, *Size,
                                   Alignment, Global.getName().str(),
                                   Global.isConstant(),
                                   Global.isConstant() &&
                                       Global.hasAtLeastLocalUnnamedAddr(),
                                   Global.isExternallyInitialized());
      if (!Object)
        return false;
      GlobalObjects.emplace(&Global, std::move(Object));
    }
    for (llvm::GlobalVariable &Global : Module.globals()) {
      if (!Needed.count(&Global))
        continue;
      // The initializer is not an entry-state value for this global.  Leave
      // its bytes unknown until execution writes them explicitly.
      if (Global.isExternallyInitialized())
        continue;
      const auto &Object = GlobalObjects.at(&Global);
      bool Copied = false;
      auto Memory =
          State.addressSpace().getWriteable(Object->Id, Copied);
      if (!Memory) {
        internalError(State, InternalErrorReason::MemoryModelError,
                      "global object missing immediately after binding");
        return false;
      }
      llvm::Constant *Initializer = Global.getInitializer();
      const unsigned Width = DL.getPointerSizeInBits(0);
      std::function<bool(llvm::Constant *, llvm::Type *, std::uint64_t)>
          Initialize = [&](llvm::Constant *Value, llvm::Type *Type,
                           std::uint64_t At) -> bool {
        auto AllocSize = fixedSize(DL.getTypeAllocSize(Type));
        if (!AllocSize) {
          incomplete(State, IncompletenessReason::ScalableType,
                     "global initializer has scalable allocation size");
          return false;
        }
        if (At > Object->Size || *AllocSize > Object->Size - At) {
          internalError(State, InternalErrorReason::MemoryModelError,
                        "global initializer exceeds allocated object");
          return false;
        }
        // LLVM undef contributes no initialized bits. Later field stores or
        // byte copies retain this mask and cannot turn an undef read into SAFE.
        if (llvm::isa<llvm::UndefValue>(Value))
          return true;
        if (llvm::isa<llvm::ConstantAggregateZero>(Value)) {
          for (std::uint64_t Byte = 0; Byte < *AllocSize; ++Byte)
            Memory->writeByte(Expressions.constant(Width, At + Byte),
                              Expressions.constant(8, 0), Expressions, true);
          if (auto *Array = llvm::dyn_cast<llvm::ArrayType>(Type)) {
            auto Stride = fixedSize(DL.getTypeAllocSize(Array->getElementType()));
            if (!Stride) {
              incomplete(State, IncompletenessReason::ScalableType,
                         "zero global array has scalable element stride");
              return false;
            }
            if (Array->getNumElements() > Options.MaxObjectBytes) {
              incomplete(State, IncompletenessReason::AllocationLimit,
                         "zero global array has too many elements");
              return false;
            }
            for (std::uint64_t Index = 0; Index < Array->getNumElements(); ++Index)
              if (!Initialize(llvm::Constant::getNullValue(Array->getElementType()),
                              Array->getElementType(), At + Index * *Stride))
                return false;
          } else if (auto *Structure = llvm::dyn_cast<llvm::StructType>(Type)) {
            const llvm::StructLayout *Layout = DL.getStructLayout(Structure);
            for (unsigned Index = 0; Index < Structure->getNumElements(); ++Index)
              if (!Initialize(llvm::Constant::getNullValue(Structure->getElementType(Index)),
                              Structure->getElementType(Index),
                              At + Layout->getElementOffset(Index)))
                return false;
          }
          return true;
        }
        if (auto *Array = llvm::dyn_cast<llvm::ArrayType>(Type)) {
          if (!llvm::isa<llvm::ConstantArray, llvm::ConstantDataArray>(Value)) {
            incomplete(State, IncompletenessReason::UnsupportedGlobal,
                       "unsupported array global initializer");
            return false;
          }
          auto Stride = fixedSize(DL.getTypeAllocSize(Array->getElementType()));
          if (!Stride) {
            incomplete(State, IncompletenessReason::ScalableType,
                       "global array has scalable element stride");
            return false;
          }
          if (Array->getNumElements() > Options.MaxObjectBytes) {
            incomplete(State, IncompletenessReason::AllocationLimit,
                       "global array initializer has too many elements");
            return false;
          }
          for (std::uint64_t Index = 0; Index < Array->getNumElements(); ++Index) {
            llvm::Constant *Element = Value->getAggregateElement(Index);
            if (!Element || Index > (Object->Size - At) / std::max<std::uint64_t>(1, *Stride) ||
                !Initialize(Element, Array->getElementType(), At + Index * *Stride))
              return false;
          }
          return true;
        }
        if (auto *Structure = llvm::dyn_cast<llvm::StructType>(Type)) {
          if (!llvm::isa<llvm::ConstantStruct>(Value)) {
            incomplete(State, IncompletenessReason::UnsupportedGlobal,
                       "unsupported struct global initializer");
            return false;
          }
          const llvm::StructLayout *Layout = DL.getStructLayout(Structure);
          for (unsigned Index = 0; Index < Structure->getNumElements(); ++Index) {
            llvm::Constant *Element = Value->getAggregateElement(Index);
            if (!Element || !Initialize(Element, Structure->getElementType(Index),
                                        At + Layout->getElementOffset(Index)))
              return false;
          }
          return true;
        }
        if (!Type->isIntegerTy() && !Type->isPointerTy()) {
          incomplete(State, IncompletenessReason::UnsupportedGlobal,
                     "unsupported scalar in aggregate global initializer");
          return false;
        }
        auto StoreSize = fixedSize(DL.getTypeStoreSize(Type));
        if (!StoreSize) {
          incomplete(State, IncompletenessReason::ScalableType,
                     "global scalar has scalable store size");
          return false;
        }
        ExprRef Offset = Expressions.constant(Width, At);
        if (auto *Integer = llvm::dyn_cast<llvm::ConstantInt>(Value)) {
          writeIntegerBytes(*Memory, Offset,
                            Expressions.constant(Integer->getValue()),
                            *StoreSize, DL.isLittleEndian(), true);
          return true;
        }
        if (Type->isPointerTy()) {
          auto Pointer = resolve(Value, State);
          if (!Pointer)
            return false;
          if (!Pointer->isPointer() || !Pointer->pointer().Numeric) {
            incomplete(State, IncompletenessReason::UnsupportedGlobal,
                       "global pointer initializer has no provenance");
            return false;
          }
          writeIntegerBytes(*Memory, Offset, Pointer->pointer().Numeric,
                            *StoreSize, DL.isLittleEndian(), true,
                            Expressions.boolean(std::any_of(
                                Pointer->pointer().Candidates.begin(),
                                Pointer->pointer().Candidates.end(),
                                [](const PointerCandidate &Candidate) {
                                  return static_cast<bool>(Candidate.Object);
                                })));
          Memory->installShadow({At, *StoreSize, Pointer->width(),
                                 Pointer->pointer().AddressSpace,
                                 Pointer->pointer(), Pointer->pointer().Numeric},
                                true);
          return true;
        }
        incomplete(State, IncompletenessReason::UnsupportedGlobal,
                   "unsupported global integer initializer");
        return false;
      };
      if (!Initialize(Initializer, Global.getValueType(), 0))
        return false;
      Statistics.MaxUpdateListLength =
          std::max<std::uint64_t>(Statistics.MaxUpdateListLength,
                                  Memory->bytes().size());
    }
    return true;
  }

  std::optional<PointerValue> computeGEP(const llvm::GEPOperator &GEP,
                                          PointerValue Base,
                                          ExecutionState &State) {
    if (GEP.getInRange()) {
      incomplete(State, IncompletenessReason::InboundsGEP,
                 "M4 does not model inrange GEP derived-access and comparison restrictions");
      return std::nullopt;
    }
    if (!supportedGEPNoWrap(GEP)) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "GEP no-wrap flags need a proven zero displacement");
      return std::nullopt;
    }
    if (Base.AddressSpace != 0 ||
        llvm::cast<llvm::PointerType>(GEP.getType())->getAddressSpace() != 0) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "M4 GEP requires address space zero");
      return std::nullopt;
    }
    const llvm::DataLayout &DL = layout(State);
    const unsigned Width = Base.Width;
    if (DL.getIndexSizeInBits(0) != Width || Width > 64) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "M4 requires matching pointer/index widths up to 64 bits");
      return std::nullopt;
    }
    if (GEP.getSourceElementType()->isVectorTy()) {
      incomplete(State, IncompletenessReason::UnsupportedVector,
                 "vector GEP source is unsupported");
      return std::nullopt;
    }
    llvm::Type *Current = GEP.getSourceElementType();
    bool AllZeroIndices = true;
    for (llvm::Value *Index : GEP.indices()) {
      const auto *Constant = llvm::dyn_cast<llvm::ConstantInt>(Index);
      if (!Constant || !Constant->isZero()) {
        AllZeroIndices = false;
        break;
      }
    }
    auto CheckInbounds = [&]() -> bool {
      if (!GEP.isInBounds() || AllZeroIndices)
        return true;
      for (const PointerCandidate &Candidate : Base.Candidates) {
        if (!Candidate.Object) {
          incomplete(State, IncompletenessReason::InboundsGEP,
                     "inbounds GEP from null has unsupported preconditions");
          return false;
        }
        ExprRef Nonnegative = Expressions.logicalNot(Expressions.slt(
            Candidate.Offset, Expressions.constant(Width, 0)));
        ExprRef AtMostEnd = Expressions.ule(
            Candidate.Offset,
            Expressions.constant(Width, Candidate.Object->Size));
        ExprRef InRange = Expressions.select(
            Nonnegative, AtMostEnd, Expressions.boolean(false));
        if (InRange->isConstant()) {
          if (InRange->constantValue().isZero()) {
            incomplete(State, IncompletenessReason::InboundsGEP,
                       "inbounds GEP intermediate address leaves object");
            return false;
          }
        } else if (!enforceDefined(
                       State, InRange, IncompletenessReason::InboundsGEP,
                       "inbounds GEP intermediate address leaves object"))
          return false;
      }
      return true;
    };
    bool First = true;
    for (llvm::Value *IndexValue : GEP.indices()) {
      std::uint64_t Stride = 0;
      if (First) {
        auto Size = fixedSize(DL.getTypeAllocSize(Current));
        if (!Size) {
          incomplete(State, IncompletenessReason::ScalableType,
                     "scalable GEP source size");
          return std::nullopt;
        }
        Stride = *Size;
        First = false;
      } else if (auto *Structure = llvm::dyn_cast<llvm::StructType>(Current)) {
        auto *Field = llvm::dyn_cast<llvm::ConstantInt>(IndexValue);
        if (!Field || Field->getValue().isNegative() ||
            Field->getValue().uge(Structure->getNumElements())) {
          incomplete(State, IncompletenessReason::UnsupportedAggregate,
                     "GEP struct field must be a valid constant index");
          return std::nullopt;
        }
        unsigned FieldIndex = static_cast<unsigned>(Field->getZExtValue());
        Stride = DL.getStructLayout(Structure)->getElementOffset(FieldIndex);
        if (Stride > Options.MaxObjectBytes) {
          incomplete(State, IncompletenessReason::AllocationLimit,
                     "GEP struct field offset exceeds M4 limit");
          return std::nullopt;
        }
        Current = Structure->getElementType(FieldIndex);
        for (PointerCandidate &Candidate : Base.Candidates) {
          const unsigned WideWidth = Width * 2;
          ExprRef WideResult = Expressions.add(
              Expressions.sext(Candidate.Offset, WideWidth),
              Expressions.constant(WideWidth, Stride));
          ExprRef Narrow = Expressions.trunc(WideResult, Width);
          ExprRef NoOverflow = Expressions.eq(
              WideResult, Expressions.sext(Narrow, WideWidth));
          if (NoOverflow->isConstant()) {
            if (NoOverflow->constantValue().isZero()) {
              incomplete(State, IncompletenessReason::InboundsGEP,
                         "GEP struct offset exceeds pointer width");
              return std::nullopt;
            }
          } else if (!enforceDefined(
                         State, NoOverflow,
                         IncompletenessReason::InboundsGEP,
                         "GEP struct offset exceeds pointer width"))
            return std::nullopt;
          Candidate.Offset = Narrow;
        }
        if (!CheckInbounds())
          return std::nullopt;
        continue;
      } else if (auto *Array = llvm::dyn_cast<llvm::ArrayType>(Current)) {
        Current = Array->getElementType();
        auto Size = fixedSize(DL.getTypeAllocSize(Current));
        if (!Size) {
          incomplete(State, IncompletenessReason::ScalableType,
                     "scalable GEP array stride");
          return std::nullopt;
        }
        Stride = *Size;
      } else {
        incomplete(State, IncompletenessReason::UnsupportedAggregate,
                   "unsupported GEP indexed type");
        return std::nullopt;
      }
      if (Stride > Options.MaxObjectBytes) {
        incomplete(State, IncompletenessReason::AllocationLimit,
                   "GEP stride exceeds M4 limit");
        return std::nullopt;
      }

      std::optional<SymbolicValue> Index = resolve(IndexValue, State);
      if (!Index)
        return std::nullopt;
      if (Index->isPointer()) {
        internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                      "GEP integer index resolved to a pointer");
        return std::nullopt;
      }
      ExprRef IndexExpr = Index->bitVector();
      if (IndexExpr->width() < Width)
        IndexExpr = Expressions.sext(IndexExpr, Width);
      else if (IndexExpr->width() > Width)
        IndexExpr = Expressions.trunc(IndexExpr, Width);
      const unsigned WideWidth = Width * 2;
      ExprRef WideIndex = Expressions.sext(IndexExpr, WideWidth);
      ExprRef WideDelta = Expressions.mul(
          WideIndex, Expressions.constant(WideWidth, Stride));
      for (PointerCandidate &Candidate : Base.Candidates) {
        ExprRef WideResult = Expressions.add(
            Expressions.sext(Candidate.Offset, WideWidth), WideDelta);
        ExprRef Narrow = Expressions.trunc(WideResult, Width);
        ExprRef NoOverflow = Expressions.eq(
            WideResult, Expressions.sext(Narrow, WideWidth));
        if (NoOverflow->isConstant()) {
          if (NoOverflow->constantValue().isZero()) {
            incomplete(State, IncompletenessReason::InboundsGEP,
                       "GEP offset arithmetic exceeds pointer width");
            return std::nullopt;
          }
        } else if (!enforceDefined(
                       State, NoOverflow, IncompletenessReason::InboundsGEP,
                       "GEP offset arithmetic exceeds pointer width"))
          return std::nullopt;
        Candidate.Offset = Narrow;
      }
      if (!CheckInbounds())
        return std::nullopt;
    }
    // Numeric bytes are a representation for pointer stores, not the source
    // of target identity.  Multi-candidate values retain their guarded
    // provenance and get a conditional numeric representation.
    ExprRef Numeric;
    for (auto It = Base.Candidates.rbegin(); It != Base.Candidates.rend();
         ++It) {
      ExprRef CandidateNumber = It->Object
                                    ? Expressions.add(
                                          Expressions.constant(
                                              Width, It->Object->LogicalBase),
                                          It->Offset)
                                    : It->Offset;
      Numeric = Numeric && It->Guard
                    ? Expressions.select(It->Guard, CandidateNumber, Numeric)
                    : CandidateNumber;
    }
    Base.Numeric = Numeric;
    return Base;
  }

  bool addConstraint(ExecutionState &State, const ExprRef &Guard) {
    try {
      State.constraints().add(Guard);
      return true;
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
      return false;
    }
  }

  const LoopAnalysis::BoundDefinition &boundFor(LoopAnalysis &Analysis,
                                                const llvm::Loop *Loop) {
    auto [Position, Inserted] = Analysis.Bounds.try_emplace(Loop);
    LoopAnalysis::BoundDefinition &Result = Position->second;
    if (!Inserted)
      return Result;
    Result.Bound = Options.DefaultLoopBound;
    llvm::MDNode *LoopID = nullptr;
    bool HaveLatch = false;
    for (llvm::BasicBlock *Block : Loop->blocks()) {
      if (!Loop->isLoopLatch(Block))
        continue;
      llvm::MDNode *Candidate = Block->getTerminator()->getMetadata("llvm.loop");
      if (HaveLatch && Candidate != LoopID) {
        Result.Error = IncompletenessReason::LoopUnsupported;
        Result.Detail = "loop latches carry inconsistent loop metadata";
        return Result;
      }
      LoopID = Candidate;
      Result.MetadataLocation = locationOf(Block->getTerminator());
      HaveLatch = true;
    }
    if (!LoopID) {
      if (Options.RequireLocalLoopBound) {
        Result.Error = IncompletenessReason::LoopMetadataLost;
        Result.Detail = "required local loop bound metadata is absent";
      }
      return Result;
    }
    std::optional<std::uint64_t> FormalBound, UnrollBound;
    bool UnrollDisabled = false;
    for (unsigned Index = 1; Index < LoopID->getNumOperands(); ++Index) {
      auto *Property = llvm::dyn_cast_or_null<llvm::MDNode>(
          LoopID->getOperand(Index).get());
      if (!Property || Property->getNumOperands() < 1)
        continue;
      auto *Name = llvm::dyn_cast<llvm::MDString>(
          Property->getOperand(0).get());
      if (!Name)
        continue;
      const llvm::StringRef Key = Name->getString();
      if (Key == "llvm.loop.unroll.disable") {
        UnrollDisabled = true;
        continue;
      }
      std::optional<std::uint64_t> *Selected = nullptr;
      if (Key == "formal.loop.bound")
        Selected = &FormalBound;
      else if (Key == "llvm.loop.unroll.count")
        Selected = &UnrollBound;
      else
        continue;
      auto *Number = Property->getNumOperands() == 2
                         ? llvm::dyn_cast<llvm::ConstantAsMetadata>(
                               Property->getOperand(1).get())
                         : nullptr;
      auto *Integer = Number
                          ? llvm::dyn_cast<llvm::ConstantInt>(Number->getValue())
                          : nullptr;
      if (*Selected || !Integer || Integer->getValue().isNegative() ||
          Integer->getValue().ugt(std::numeric_limits<std::uint64_t>::max())) {
        Result.Error = IncompletenessReason::LoopUnsupported;
        Result.Detail = "duplicate or invalid " + Key.str() + " metadata";
        return Result;
      }
      *Selected = Integer->getZExtValue();
    }
    if (FormalBound) {
      Result.Bound = *FormalBound;
      Result.Source = "formal.loop.bound";
    } else if (UnrollBound) {
      Result.Bound = *UnrollBound;
      Result.Source = "llvm.loop.unroll.count";
    } else if (UnrollDisabled) {
      Result.Error = IncompletenessReason::LoopMetadataLost;
      Result.Detail = "loop unroll hint was consumed; no execution bound remains";
    } else if (Options.RequireLocalLoopBound) {
      Result.Error = IncompletenessReason::LoopMetadataLost;
      Result.Detail = "loop metadata contains no required local bound";
    }
    return Result;
  }

  bool transfer(ExecutionState &State, llvm::BasicBlock *Destination) {
    if (!Destination) {
      internalError(State, InternalErrorReason::InvalidExecutorState,
                    "control-flow destination is null");
      return false;
    }
    llvm::BasicBlock *Predecessor = State.pc().block();
    llvm::Function *Function = Destination->getParent();
    auto &Analysis = LoopAnalyses[Function];
    if (!Analysis)
      Analysis = std::make_unique<LoopAnalysis>(*Function);
    if (Analysis->IrreducibleBlocks.count(Destination)) {
      LoopDiagnostic Diagnostic;
      Diagnostic.FrameId = State.frame().id();
      Diagnostic.Function = Function->getName().str();
      Diagnostic.Header = blockName(Destination);
      Diagnostic.StaticLoopId =
          "irreducible:" + Diagnostic.Function + ":" + Diagnostic.Header;
      Diagnostic.SuppressedFrom = blockName(Predecessor);
      Diagnostic.SuppressedTo = blockName(Destination);
      State.setLoopDiagnostic(std::move(Diagnostic));
      incomplete(State, IncompletenessReason::IrreducibleCycle,
                 "successor enters a CFG cycle outside natural-loop backedges");
      return false;
    }
    llvm::Loop *SourceLoop = Analysis->Loops.getLoopFor(Predecessor);
    llvm::Loop *TargetLoop = Analysis->Loops.getLoopFor(Destination);
    auto &Counts = State.frame().loopActivations();
    // Every loop entered from outside receives a fresh dynamic activation.
    for (llvm::Loop *Loop = SourceLoop; Loop; Loop = Loop->getParentLoop())
      if (!Loop->contains(Destination))
        Counts.erase(Loop->getHeader());
    for (llvm::Loop *Loop = TargetLoop; Loop; Loop = Loop->getParentLoop())
      if (!Loop->contains(Predecessor)) {
        if (Destination != Loop->getHeader()) {
          LoopDiagnostic Diagnostic;
          Diagnostic.FrameId = State.frame().id();
          Diagnostic.Function = Function->getName().str();
          Diagnostic.Header = blockName(Loop->getHeader());
          Diagnostic.StaticLoopId =
              Diagnostic.Function + ":" + Diagnostic.Header;
          Diagnostic.SuppressedFrom = blockName(Predecessor);
          Diagnostic.SuppressedTo = blockName(Destination);
          State.setLoopDiagnostic(std::move(Diagnostic));
          incomplete(State, IncompletenessReason::LoopUnsupported,
                     "CFG enters a natural loop outside its header");
          return false;
        }
        const auto &Bound = boundFor(*Analysis, Loop);
        if (Bound.Error != IncompletenessReason::None) {
          LoopDiagnostic Diagnostic;
          Diagnostic.FrameId = State.frame().id();
          Diagnostic.Function = Function->getName().str();
          Diagnostic.Header = blockName(Loop->getHeader());
          Diagnostic.StaticLoopId =
              Diagnostic.Function + ":" + Diagnostic.Header;
          Diagnostic.BoundSource = Bound.Source;
          Diagnostic.BoundLocation = Bound.MetadataLocation;
          Diagnostic.SuppressedFrom = blockName(Predecessor);
          Diagnostic.SuppressedTo = blockName(Destination);
          State.setLoopDiagnostic(std::move(Diagnostic));
          incomplete(State, Bound.Error, Bound.Detail);
          return false;
        }
        Counts[Loop->getHeader()] =
            {NextActivationId++, 0, Bound.Bound, Bound.Source};
        State.loopHistory().push_back(loopDiagnostic(
            State, Loop->getHeader(), Counts.at(Loop->getHeader())));
      }

    if (TargetLoop && Destination == TargetLoop->getHeader() &&
        TargetLoop->contains(Predecessor)) {
      if (!TargetLoop->isLoopLatch(Predecessor)) {
        incomplete(State, IncompletenessReason::LoopUnsupported,
                   "edge into loop header is not a natural backedge");
        return false;
      }
      auto Position = Counts.find(Destination);
      if (Position == Counts.end() && Destination == &Function->getEntryBlock()) {
        const auto &Bound = boundFor(*Analysis, TargetLoop);
        if (Bound.Error != IncompletenessReason::None) {
          incomplete(State, Bound.Error, Bound.Detail);
          return false;
        }
        Position = Counts.emplace(
            Destination, StackFrame::LoopActivation{
                             NextActivationId++, 0, Bound.Bound, Bound.Source})
                       .first;
        State.loopHistory().push_back(
            loopDiagnostic(State, Destination, Position->second));
      }
      if (Position == Counts.end()) {
        incomplete(State, IncompletenessReason::LoopUnsupported,
                   "natural backedge has no active loop counter");
        return false;
      }
      if (Position->second.Traversals >= Position->second.Bound) {
        LoopDiagnostic Diagnostic =
            loopDiagnostic(State, Destination, Position->second);
        Diagnostic.AttemptedBackedge = Position->second.Traversals + 1;
        Diagnostic.SuppressedFrom = blockName(Predecessor);
        Diagnostic.SuppressedTo = blockName(Destination);
        State.setLoopDiagnostic(std::move(Diagnostic));
        if (Options.LoopPolicy == LoopLimitPolicy::Error)
          State.setLoopPolicyError();
        else if (Options.LoopPolicy == LoopLimitPolicy::AssumeExit)
          State.setUnwindingAssumption();
        incomplete(State, IncompletenessReason::LoopBound,
                   "backedge bound " + std::to_string(Position->second.Bound) +
                       " exhausted at latch " + blockName(Predecessor) +
                       " -> header " + blockName(Destination));
        return false;
      }
      ++Position->second.Traversals;
      for (LoopDiagnostic &History : State.loopHistory())
        if (History.ActivationId == Position->second.Id &&
            History.FrameId == State.frame().id()) {
          History.TraversedBackedges = Position->second.Traversals;
          break;
        }
    } else if (State.hasVisited(Destination) && !TargetLoop) {
      incomplete(State, IncompletenessReason::LoopUnsupported,
                 "revisited CFG block outside a natural loop");
      return false;
    }
    State.setPredecessor(Predecessor);
    State.markVisited(Destination);
    State.pc().set(Destination->getParent(), Destination,
                   Destination->empty() ? nullptr : &Destination->front());
    return true;
  }

  void forkToDestinations(ExecutionState &State,
                          const std::vector<GuardedDestination> &Destinations) {
    const ExecutionState Base = State;
    std::vector<QueryOutcome> Outcomes;
    Outcomes.reserve(Destinations.size());
    std::size_t CandidateCount = 0;
    for (const GuardedDestination &Destination : Destinations) {
      QueryOutcome Outcome = query(Base, Destination.Guard);
      if (Outcome.Disposition == QueryDisposition::Infeasible)
        ++Statistics.PrunedPaths;
      else
        ++CandidateCount;
      Outcomes.push_back(std::move(Outcome));
    }

    if (CandidateCount == 0) {
      internalError(State, InternalErrorReason::NoFeasibleSuccessor,
                    "all control-flow successors were UNSAT");
      return;
    }
    if (CandidateCount > 1)
      Statistics.Forks += CandidateCount - 1;

    std::vector<ExecutionState> Scheduled;
    bool UsedParentId = false;
    for (std::size_t Index = 0; Index < Destinations.size(); ++Index) {
      QueryOutcome &Outcome = Outcomes[Index];
      if (Outcome.Disposition == QueryDisposition::Infeasible)
        continue;
      ExecutionState Child = Base;
      if (UsedParentId)
        Child.setId(allocateStateId());
      else
        UsedParentId = true;
      Child.setDepth(Base.depth() + 1);
      if (!addConstraint(Child, Destinations[Index].Guard))
        continue;

      if (Outcome.Disposition == QueryDisposition::Incomplete) {
        incomplete(Child, Outcome.Reason, std::move(Outcome.Detail));
        continue;
      }
      if (Outcome.Disposition == QueryDisposition::InternalError) {
        internalError(Child, InternalErrorReason::SolverInternalError,
                      std::move(Outcome.Detail));
        continue;
      }
      if (Statistics.StatesCreated > Options.MaxStates) {
        incomplete(Child, IncompletenessReason::StateLimit,
                   "M3 state limit reached before scheduling successor");
        continue;
      }
      if (transfer(Child, Destinations[Index].Destination))
        Scheduled.push_back(std::move(Child));
    }

    // Push in reverse logical order so the vector-backed worklist executes
    // the first LLVM successor/case first while retaining DFS behavior.
    for (auto Position = Scheduled.rbegin(); Position != Scheduled.rend();
         ++Position)
      Worklist.schedule(std::move(*Position));
    State.setStatus(ExecutionStateStatus::Scheduled);
  }

  bool enforceDefined(ExecutionState &State, const ExprRef &Good,
                      IncompletenessReason BadReason,
                      const std::string &BadDetail) {
    ExprRef Bad;
    try {
      Bad = Expressions.logicalNot(Good);
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
      return false;
    }

    const ExecutionState Base = State;
    QueryOutcome GoodOutcome = query(Base, Good);
    QueryOutcome BadOutcome = query(Base, Bad);
    if (GoodOutcome.Disposition == QueryDisposition::Infeasible)
      ++Statistics.PrunedPaths;
    if (BadOutcome.Disposition == QueryDisposition::Infeasible)
      ++Statistics.PrunedPaths;
    const std::size_t Candidates =
        (GoodOutcome.Disposition != QueryDisposition::Infeasible) +
        (BadOutcome.Disposition != QueryDisposition::Infeasible);
    if (Candidates == 0) {
      internalError(State, InternalErrorReason::NoFeasibleSuccessor,
                    "defined and undefined guards were both UNSAT");
      return false;
    }
    if (Candidates > 1)
      Statistics.Forks += Candidates - 1;

    bool UsedParentId = false;
    bool ContinueGood = false;
    if (GoodOutcome.Disposition != QueryDisposition::Infeasible) {
      ExecutionState GoodState = Base;
      UsedParentId = true;
      GoodState.setDepth(Base.depth() + 1);
      if (addConstraint(GoodState, Good)) {
        if (GoodOutcome.Disposition == QueryDisposition::Feasible) {
          State = std::move(GoodState);
          ContinueGood = true;
        } else if (GoodOutcome.Disposition == QueryDisposition::Incomplete) {
          incomplete(GoodState, GoodOutcome.Reason,
                     std::move(GoodOutcome.Detail));
        } else {
          internalError(GoodState, InternalErrorReason::SolverInternalError,
                        std::move(GoodOutcome.Detail));
        }
      }
    }

    if (BadOutcome.Disposition != QueryDisposition::Infeasible) {
      ExecutionState BadState = Base;
      if (UsedParentId)
        BadState.setId(allocateStateId());
      BadState.setDepth(Base.depth() + 1);
      if (Statistics.StatesCreated > Options.MaxStates) {
        incomplete(BadState, IncompletenessReason::StateLimit,
                   "state limit reached during definedness split");
      } else if (addConstraint(BadState, Bad)) {
        if (BadOutcome.Disposition == QueryDisposition::Feasible)
          incomplete(BadState, BadReason, BadDetail);
        else if (BadOutcome.Disposition == QueryDisposition::Incomplete)
          incomplete(BadState, BadOutcome.Reason, std::move(BadOutcome.Detail));
        else
          internalError(BadState, InternalErrorReason::SolverInternalError,
                        std::move(BadOutcome.Detail));
      }
    }
    if (!ContinueGood &&
        State.status() == ExecutionStateStatus::Running)
      State.setStatus(ExecutionStateStatus::Scheduled);
    return ContinueGood;
  }

  std::optional<AggregateValue> emptyAggregate(llvm::Type *Type,
                                                ExecutionState &State) {
    auto Size = fixedSize(layout(State).getTypeAllocSize(Type));
    if (!Size || *Size > Options.MaxObjectBytes) {
      incomplete(State, !Size ? IncompletenessReason::ScalableType
                              : IncompletenessReason::AllocationLimit,
                 "aggregate size is not bounded and fixed");
      return std::nullopt;
    }
    AggregateValue Result;
    Result.Type = Type;
    Result.Bytes.assign(static_cast<std::size_t>(*Size),
                        Expressions.constant(8, 0));
    Result.KnownMasks.assign(static_cast<std::size_t>(*Size),
                             Expressions.constant(8, 0));
    Result.AddressSensitiveBytes.assign(static_cast<std::size_t>(*Size),
                                        Expressions.boolean(false));
    return Result;
  }

  struct AggregateLayout {
    struct PointerField {
      std::uint64_t Size = 0;
      unsigned Width = 0;
      unsigned AddressSpace = 0;
    };
    std::vector<std::uint8_t> SemanticMasks;
    std::map<std::uint64_t, PointerField> PointerFields;
  };

  std::optional<AggregateLayout> aggregateLayout(llvm::Type *Type,
                                                  ExecutionState &State) {
    if (!Type->isAggregateType() || !Type->isSized()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "aggregate has no supported fixed layout");
      return std::nullopt;
    }
    const llvm::DataLayout &DL = layout(State);
    auto Size = fixedSize(DL.getTypeStoreSize(Type));
    if (!Size || *Size > Options.MaxObjectBytes) {
      incomplete(State, !Size ? IncompletenessReason::ScalableType
                              : IncompletenessReason::AllocationLimit,
                 "aggregate store size is not bounded and fixed");
      return std::nullopt;
    }
    AggregateLayout Result;
    Result.SemanticMasks.assign(static_cast<std::size_t>(*Size), 0);
    std::function<bool(llvm::Type *, std::uint64_t, unsigned)> Mark =
        [&](llvm::Type *Field, std::uint64_t At, unsigned Depth) -> bool {
      if (Depth > 64 || !Field->isSized())
        return false;
      if (auto *Structure = llvm::dyn_cast<llvm::StructType>(Field)) {
        if (Structure->isOpaque() ||
            Structure->getNumElements() > Options.MaxObjectBytes)
          return false;
        const llvm::StructLayout *Fields = DL.getStructLayout(Structure);
        for (unsigned Index = 0; Index < Structure->getNumElements(); ++Index) {
          std::uint64_t Offset = 0;
          if (!checkedAdd(At, Fields->getElementOffset(Index), Offset) ||
              !Mark(Structure->getElementType(Index), Offset, Depth + 1))
            return false;
        }
        return true;
      }
      if (auto *Array = llvm::dyn_cast<llvm::ArrayType>(Field)) {
        if (Array->getNumElements() > Options.MaxObjectBytes)
          return false;
        auto Stride = fixedSize(DL.getTypeAllocSize(Array->getElementType()));
        if (!Stride)
          return false;
        for (std::uint64_t Index = 0; Index < Array->getNumElements(); ++Index) {
          std::uint64_t Delta = 0;
          std::uint64_t Offset = 0;
          if (!checkedMul(Index, *Stride, Delta) ||
              !checkedAdd(At, Delta, Offset) ||
              !Mark(Array->getElementType(), Offset, Depth + 1))
            return false;
        }
        return true;
      }
      auto FieldSize = fixedSize(DL.getTypeStoreSize(Field));
      if (!FieldSize || At > Result.SemanticMasks.size() ||
          *FieldSize > Result.SemanticMasks.size() - At)
        return false;
      std::uint64_t Bits = 0;
      if (auto *Integer = llvm::dyn_cast<llvm::IntegerType>(Field))
        Bits = Integer->getBitWidth();
      else if (auto *Pointer = llvm::dyn_cast<llvm::PointerType>(Field)) {
        if (Pointer->getAddressSpace() != 0 ||
            *FieldSize > std::numeric_limits<unsigned>::max() / 8)
          return false;
        Bits = DL.getPointerSizeInBits(0);
        if (Bits != *FieldSize * 8)
          return false;
        Result.PointerFields.emplace(
            At, AggregateLayout::PointerField{*FieldSize,
                                               static_cast<unsigned>(Bits), 0});
      } else
        return false;
      for (std::uint64_t Byte = 0; Byte < *FieldSize; ++Byte) {
        const std::uint64_t LogicalByte =
            DL.isLittleEndian() ? Byte : *FieldSize - 1 - Byte;
        const std::uint64_t Remaining =
            Bits > LogicalByte * 8 ? Bits - LogicalByte * 8 : 0;
        const std::uint8_t Mask =
            Remaining >= 8 ? 0xff
                           : Remaining == 0
                                 ? 0
                                 : static_cast<std::uint8_t>((1U << Remaining) - 1);
        Result.SemanticMasks[At + Byte] |= Mask;
      }
      return true;
    };
    if (!Mark(Type, 0, 0)) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "aggregate contains an unsupported field layout");
      return std::nullopt;
    }
    return Result;
  }

  bool requireNoundefAggregate(const AggregateValue &Value, llvm::Type *Type,
                               ExecutionState &State,
                               llvm::StringRef Boundary) {
    if (Value.Type != Type) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "noundef aggregate type does not match its ABI boundary");
      return false;
    }
    auto Layout = aggregateLayout(Type, State);
    if (!Layout)
      return false;
    if (Value.KnownMasks.size() < Layout->SemanticMasks.size()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "noundef aggregate has an incomplete storage extent");
      return false;
    }
    for (std::size_t Byte = 0; Byte < Layout->SemanticMasks.size(); ++Byte) {
      const std::uint8_t Required = Layout->SemanticMasks[Byte];
      if (!Required)
        continue;
      const ExprRef &Known = Value.KnownMasks[Byte];
      if (!Known || !Known->isConstant() ||
          (Known->constantValue().getZExtValue() & Required) != Required) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "noundef aggregate " + Boundary.str() +
                       " contains unproven semantic bits");
        return false;
      }
    }
    return true;
  }

  void discardAggregatePadding(AggregateValue &Value,
                               const AggregateLayout &Layout) {
    for (std::size_t Byte = 0; Byte < Layout.SemanticMasks.size(); ++Byte) {
      const std::uint8_t Mask = Layout.SemanticMasks[Byte];
      if (Mask == 0xff)
        continue;
      ExprRef MaskValue = Expressions.constant(8, Mask);
      Value.Bytes[Byte] = Expressions.bitAnd(Value.Bytes[Byte], MaskValue);
      Value.KnownMasks[Byte] =
          Expressions.bitAnd(Value.KnownMasks[Byte], MaskValue);
      if (Mask == 0)
        Value.AddressSensitiveBytes[Byte] = Expressions.boolean(false);
    }
    for (auto It = Value.Shadows.begin(); It != Value.Shadows.end();) {
      auto Field = Layout.PointerFields.find(It->first);
      if (Field == Layout.PointerFields.end() ||
          It->second.Size != Field->second.Size ||
          It->second.PointerWidth != Field->second.Width ||
          It->second.AddressSpace != Field->second.AddressSpace)
        It = Value.Shadows.erase(It);
      else
        ++It;
    }
  }

  bool placeAggregateElement(AggregateValue &Target, llvm::Type *Type,
                             std::uint64_t At, const SymbolicValue &Value,
                             ExecutionState &State) {
    auto Size = fixedSize(layout(State).getTypeStoreSize(Type));
    if (!Size || At > Target.Bytes.size() || *Size > Target.Bytes.size() - At) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "aggregate element has an invalid store extent");
      return false;
    }
    const std::uint64_t End = At + *Size;
    for (auto It = Target.Shadows.begin(); It != Target.Shadows.end();) {
      if (At < It->second.Start + It->second.Size && It->second.Start < End)
        It = Target.Shadows.erase(It);
      else
        ++It;
    }
    if (Value.isAggregate()) {
      AggregateValue Source = Value.aggregate();
      if (Source.Type != Type || Source.Bytes.size() < *Size) {
        incomplete(State, IncompletenessReason::UnsupportedAggregate,
                   "aggregate element type or extent mismatch");
        return false;
      }
      auto FieldLayout = aggregateLayout(Type, State);
      if (!FieldLayout)
        return false;
      discardAggregatePadding(Source, *FieldLayout);
      for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
        Target.Bytes[At + Byte] = Source.Bytes[Byte];
        Target.KnownMasks[At + Byte] = Source.KnownMasks[Byte];
        Target.AddressSensitiveBytes[At + Byte] = Source.AddressSensitiveBytes[Byte];
      }
      for (const auto &[Start, Shadow] : Source.Shadows) {
        if (Start > *Size || Shadow.Size > *Size - Start)
          continue;
        PointerShadowEntry Copied = Shadow;
        Copied.Start += At;
        Target.Shadows.emplace(Copied.Start, std::move(Copied));
      }
      return true;
    }
    if (!Type->isIntegerTy() && !Type->isPointerTy()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "unsupported aggregate scalar element");
      return false;
    }
    ExprRef Bits = Value.isPointer() ? Value.pointer().Numeric : Value.bitVector();
    if (!Bits || *Size > std::numeric_limits<unsigned>::max() / 8 ||
        Bits->width() > *Size * 8) {
      incomplete(State, IncompletenessReason::PointerProvenanceLost,
                 "aggregate scalar has no usable numeric bytes");
      return false;
    }
    const unsigned StorageWidth = static_cast<unsigned>(*Size * 8);
    ExprRef Stored = Bits->width() == StorageWidth
                         ? Bits : Expressions.zext(Bits, StorageWidth);
    bool Sensitive = Value.isPointer() && std::any_of(
        Value.pointer().Candidates.begin(), Value.pointer().Candidates.end(),
        [](const PointerCandidate &Candidate) { return bool(Candidate.Object); });
    for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
      const std::uint64_t LogicalByte =
          layout(State).isLittleEndian() ? Byte : *Size - 1 - Byte;
      Target.Bytes[At + Byte] = Expressions.extract(
          Stored, static_cast<unsigned>(LogicalByte * 8), 8);
      const std::uint64_t Remaining =
          Bits->width() > LogicalByte * 8 ? Bits->width() - LogicalByte * 8 : 0;
      const std::uint8_t Mask = Remaining >= 8 ? 0xff
                                 : Remaining == 0 ? 0
                                 : static_cast<std::uint8_t>((1U << Remaining) - 1);
      Target.KnownMasks[At + Byte] = Expressions.constant(8, Mask);
      Target.AddressSensitiveBytes[At + Byte] = Expressions.boolean(Sensitive);
    }
    if (Value.isPointer())
      Target.Shadows.emplace(At, PointerShadowEntry{
          At, *Size, Value.width(), Value.pointer().AddressSpace,
          Value.pointer(), Bits});
    return true;
  }

  std::optional<std::pair<llvm::Type *, std::uint64_t>>
  aggregateElement(llvm::Type *Type, llvm::ArrayRef<unsigned> Indices,
                   ExecutionState &State) {
    std::uint64_t At = 0;
    for (unsigned Index : Indices) {
      std::uint64_t Delta = 0;
      if (auto *Structure = llvm::dyn_cast<llvm::StructType>(Type)) {
        if (Structure->isOpaque() || Index >= Structure->getNumElements())
          return std::nullopt;
        Delta = layout(State).getStructLayout(Structure)->getElementOffset(Index);
        Type = Structure->getElementType(Index);
      } else if (auto *Array = llvm::dyn_cast<llvm::ArrayType>(Type)) {
        if (Index >= Array->getNumElements())
          return std::nullopt;
        auto Stride = fixedSize(layout(State).getTypeAllocSize(Array->getElementType()));
        if (!Stride || !checkedMul(Index, *Stride, Delta))
          return std::nullopt;
        Type = Array->getElementType();
      } else
        return std::nullopt;
      if (!checkedAdd(At, Delta, At))
        return std::nullopt;
    }
    return std::make_pair(Type, At);
  }

  std::optional<AggregateValue>
  materializeAggregate(const llvm::Constant *Constant, ExecutionState &State,
                       const StackFrame *FrameOverride) {
    llvm::Type *Type = Constant->getType();
    auto Result = emptyAggregate(Type, State);
    if (!Result)
      return std::nullopt;
    const bool Zero = llvm::isa<llvm::ConstantAggregateZero>(Constant);
    if (!Zero && !llvm::isa<llvm::ConstantStruct, llvm::ConstantArray,
                            llvm::ConstantDataArray>(Constant)) {
      incomplete(State, IncompletenessReason::UnsupportedConstant,
                 "unsupported aggregate constant form");
      return std::nullopt;
    }
    unsigned Count = 0;
    if (auto *Structure = llvm::dyn_cast<llvm::StructType>(Type)) {
      if (Structure->isOpaque()) {
        incomplete(State, IncompletenessReason::UnsupportedAggregate,
                   "opaque struct constant");
        return std::nullopt;
      }
      Count = Structure->getNumElements();
    } else if (auto *Array = llvm::dyn_cast<llvm::ArrayType>(Type)) {
      if (Array->getNumElements() > std::numeric_limits<unsigned>::max()) {
        incomplete(State, IncompletenessReason::AllocationLimit,
                   "aggregate constant has too many elements");
        return std::nullopt;
      }
      Count = static_cast<unsigned>(Array->getNumElements());
    } else {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "aggregate constant has unsupported type");
      return std::nullopt;
    }
    if (Count > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "aggregate constant has too many elements");
      return std::nullopt;
    }
    for (unsigned Index = 0; Index < Count; ++Index) {
      auto Element = aggregateElement(Type, {Index}, State);
      if (!Element) {
        incomplete(State, IncompletenessReason::UnsupportedAggregate,
                   "aggregate constant element layout is unsupported");
        return std::nullopt;
      }
      llvm::Constant *Field = Zero
          ? llvm::Constant::getNullValue(Element->first)
          : Constant->getAggregateElement(Index);
      if (!Field) {
        incomplete(State, IncompletenessReason::UnsupportedConstant,
                   "aggregate constant element is missing");
        return std::nullopt;
      }
      if (llvm::isa<llvm::UndefValue>(Field))
        continue;
      auto Value = resolve(Field, State, FrameOverride);
      if (!Value || !placeAggregateElement(*Result, Element->first,
                                           Element->second, *Value, State))
        return std::nullopt;
    }
    return Result;
  }

  std::optional<SymbolicValue>
  resolve(const llvm::Value *Value, ExecutionState &State,
          const StackFrame *FrameOverride = nullptr) {
    if (!Value) {
      internalError(State, InternalErrorReason::MissingSSABinding,
                    "attempted to resolve a null LLVM Value");
      return std::nullopt;
    }
    if (const auto *Constant = llvm::dyn_cast<llvm::ConstantInt>(Value))
      return SymbolicValue(Expressions.constant(Constant->getValue()));
    if (Value->getType()->isAggregateType()) {
      if (llvm::isa<llvm::UndefValue>(Value)) {
        auto Aggregate = emptyAggregate(Value->getType(), State);
        if (!Aggregate)
          return std::nullopt;
        return SymbolicValue(std::move(*Aggregate));
      }
      if (const auto *Constant = llvm::dyn_cast<llvm::Constant>(Value)) {
        auto Aggregate = materializeAggregate(Constant, State, FrameOverride);
        if (!Aggregate)
          return std::nullopt;
        return SymbolicValue(std::move(*Aggregate));
      }
    }
    if (const auto *Null = llvm::dyn_cast<llvm::ConstantPointerNull>(Value)) {
      unsigned AddressSpace = Null->getType()->getAddressSpace();
      if (AddressSpace != 0) {
        incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                   "non-default null pointer address space");
        return std::nullopt;
      }
      return SymbolicValue(PointerValue::null(
          layout(State).getPointerSizeInBits(AddressSpace), Expressions,
          AddressSpace));
    }
    if (const auto *Global = llvm::dyn_cast<llvm::GlobalVariable>(Value)) {
      auto It = GlobalObjects.find(Global);
      if (It == GlobalObjects.end()) {
        incomplete(State, IncompletenessReason::UnsupportedGlobal,
                   "unbound or external global variable");
        return std::nullopt;
      }
      const unsigned Width = layout(State).getPointerSizeInBits(0);
      return SymbolicValue(PointerValue::object(
          It->second, Expressions.constant(Width, 0), Expressions));
    }
    if (const auto *GEP = llvm::dyn_cast<llvm::GEPOperator>(Value);
        GEP && llvm::isa<llvm::Constant>(Value)) {
      std::optional<SymbolicValue> Base =
          resolve(GEP->getPointerOperand(), State, FrameOverride);
      if (!Base)
        return std::nullopt;
      if (!Base->isPointer()) {
        internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                      "constant GEP base lost pointer provenance");
        return std::nullopt;
      }
      std::optional<PointerValue> Pointer =
          computeGEP(*GEP, Base->pointer(), State);
      if (!Pointer)
        return std::nullopt;
      return SymbolicValue(std::move(*Pointer));
    }
    if (llvm::isa<llvm::PoisonValue>(Value)) {
      incomplete(State, IncompletenessReason::PoisonValue,
                 "M3 does not model LLVM poison values");
      return std::nullopt;
    }
    if (llvm::isa<llvm::UndefValue>(Value)) {
      incomplete(State, IncompletenessReason::UndefValue,
                 "M3 does not model LLVM undef values");
      return std::nullopt;
    }
    if (llvm::isa<llvm::Constant>(Value)) {
      incomplete(State, IncompletenessReason::UnsupportedConstant,
                 "M3 supports ConstantInt only; ConstantExpr and other "
                 "constants are unsupported");
      return std::nullopt;
    }
    const StackFrame &Frame = FrameOverride ? *FrameOverride : State.frame();
    const SymbolicValue *Binding = Frame.lookup(Value);
    if (!Binding) {
      internalError(State, InternalErrorReason::MissingSSABinding,
                    "SSA value has no local binding; fresh symbols are "
                    "forbidden");
      return std::nullopt;
    }
    bool Matches = false;
    if (Value->getType()->isAggregateType() && Binding->isAggregate())
      Matches = Value->getType() == Binding->aggregate().Type;
    else if (Value->getType()->isIntegerTy() &&
             !Binding->isPointer() && !Binding->isAggregate())
      Matches = Binding->width() ==
                llvm::cast<llvm::IntegerType>(Value->getType())->getBitWidth();
    else if (auto *Pointer =
                 llvm::dyn_cast<llvm::PointerType>(Value->getType());
             Pointer && Binding->isPointer())
      Matches = Pointer->getAddressSpace() ==
                    Binding->pointer().AddressSpace &&
                Binding->width() ==
                    layout(State).getPointerSizeInBits(Pointer->getAddressSpace());
    if (!Matches) {
      internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                    "SSA binding does not match the LLVM value type");
      return std::nullopt;
    }
    return *Binding;
  }

  ExprRef toBool(const SymbolicValue &Value) {
    return Expressions.toBool(Value.bitVector());
  }

  SymbolicValue boolToI1(const ExprRef &Boolean) {
    return SymbolicValue(Expressions.select(Boolean, Expressions.constant(1, 1),
                                            Expressions.constant(1, 0)));
  }

  bool bindResult(llvm::Instruction &Instruction, ExecutionState &State,
                  SymbolicValue Value) {
    bool Matches = false;
    if (Instruction.getType()->isAggregateType() && Value.isAggregate())
      Matches = Instruction.getType() == Value.aggregate().Type;
    else if (Instruction.getType()->isIntegerTy() &&
             !Value.isPointer() && !Value.isAggregate())
      Matches = llvm::cast<llvm::IntegerType>(Instruction.getType())
                    ->getBitWidth() == Value.width();
    else if (auto *Pointer =
                 llvm::dyn_cast<llvm::PointerType>(Instruction.getType());
             Pointer && Value.isPointer())
      Matches = Pointer->getAddressSpace() == Value.pointer().AddressSpace &&
                Value.width() == layout(State).getPointerSizeInBits(
                                     Pointer->getAddressSpace());
    if (!Matches) {
      internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                    "instruction result type does not match symbolic value");
      return false;
    }
    State.frame().bind(&Instruction, std::move(Value));
    State.pc().advance();
    return true;
  }

  bool executePHIs(ExecutionState &State) {
    llvm::Instruction *Current = State.pc().instruction();
    if (!llvm::isa_and_nonnull<llvm::PHINode>(Current))
      return false;
    std::vector<llvm::PHINode *> PHIs;
    for (llvm::Instruction *Instruction = Current;
         auto *Phi = llvm::dyn_cast_or_null<llvm::PHINode>(Instruction);
         Instruction = Instruction->getNextNode())
      PHIs.push_back(Phi);

    if (State.executedInstructions() + PHIs.size() >
        Options.MaxInstructionsPerState) {
      incomplete(State, IncompletenessReason::InstructionLimit,
                 "instruction limit reached while entering PHI block");
      return true;
    }
    State.addExecutedInstructions(PHIs.size());
    Statistics.ExecutedInstructions += PHIs.size();
    if (!State.predecessor()) {
      internalError(State, InternalErrorReason::InvalidPHIPredecessor,
                    "PHI block has no recorded predecessor");
      return true;
    }

    // All incoming operands are read from the same pre-entry snapshot.  Only
    // after every operand resolves do we publish any PHI result.
    const StackFrame Snapshot = State.frame();
    std::vector<SymbolicValue> Values;
    Values.reserve(PHIs.size());
    for (llvm::PHINode *Phi : PHIs) {
      State.pc().set(State.pc().function(), State.pc().block(), Phi);
      int Incoming = Phi->getBasicBlockIndex(State.predecessor());
      if (Incoming < 0) {
        internalError(State, InternalErrorReason::InvalidPHIPredecessor,
                      "PHI has no incoming value for the actual predecessor");
        return true;
      }
      std::optional<SymbolicValue> Value = resolve(
          Phi->getIncomingValue(static_cast<unsigned>(Incoming)), State,
          &Snapshot);
      if (!Value)
        return true;
      Values.push_back(std::move(*Value));
    }
    for (std::size_t Index = 0; Index < PHIs.size(); ++Index)
      State.frame().bind(PHIs[Index], std::move(Values[Index]));
    State.pc().set(State.pc().function(), State.pc().block(),
                   PHIs.back()->getNextNode());
    if (!State.pc().instruction())
      internalError(State, InternalErrorReason::InvalidExecutorState,
                    "basic block ended after PHI nodes without a terminator");
    return true;
  }

  std::optional<SymbolicValue> binaryOperands(llvm::BinaryOperator &Binary,
                                              ExecutionState &State,
                                              SymbolicValue &Right) {
    std::optional<SymbolicValue> Left = resolve(Binary.getOperand(0), State);
    if (!Left)
      return std::nullopt;
    std::optional<SymbolicValue> RightValue =
        resolve(Binary.getOperand(1), State);
    if (!RightValue)
      return std::nullopt;
    Right = std::move(*RightValue);
    return Left;
  }

  void executeBinary(llvm::BinaryOperator &Binary, ExecutionState &State) {
    SymbolicValue Right(Expressions.constant(1, 0));
    std::optional<SymbolicValue> Left =
        binaryOperands(Binary, State, Right);
    if (!Left)
      return;
    ExprRef L = Left->bitVector();
    ExprRef R = Right.bitVector();
    const unsigned Width = Left->width();

    try {
      switch (Binary.getOpcode()) {
      case llvm::Instruction::UDiv:
      case llvm::Instruction::SDiv:
      case llvm::Instruction::URem:
      case llvm::Instruction::SRem: {
        ExprRef NonZero = Expressions.logicalNot(
            Expressions.eq(R, Expressions.constant(Width, 0)));
        if (!enforceDefined(State, NonZero,
                            IncompletenessReason::DivideByZero,
                            "integer division/remainder by zero"))
          return;
        if (Binary.getOpcode() == llvm::Instruction::SDiv) {
          ExprRef IsMin = Expressions.eq(
              L, Expressions.constant(llvm::APInt::getSignedMinValue(Width)));
          ExprRef IsMinusOne = Expressions.eq(
              R, Expressions.constant(llvm::APInt::getAllOnes(Width)));
          ExprRef Overflow =
              Expressions.select(IsMin, IsMinusOne, Expressions.boolean(false));
          if (!enforceDefined(
                  State, Expressions.logicalNot(Overflow),
                  IncompletenessReason::SignedDivisionOverflow,
                  "sdiv INT_MIN / -1 is not modeled as an ordinary value"))
            return;
        }
        break;
      }
      case llvm::Instruction::Shl:
      case llvm::Instruction::LShr:
      case llvm::Instruction::AShr: {
        ExprRef InRange =
            Expressions.ult(R, Expressions.constant(llvm::APInt(Width, Width)));
        if (!enforceDefined(State, InRange,
                            IncompletenessReason::Overshift,
                            "LLVM shift amount is greater than or equal to "
                            "the value width"))
          return;
        break;
      }
      default:
        break;
      }

      ExprRef Result;
      switch (Binary.getOpcode()) {
      case llvm::Instruction::Add:
        Result = Expressions.add(L, R);
        break;
      case llvm::Instruction::Sub:
        Result = Expressions.sub(L, R);
        break;
      case llvm::Instruction::Mul:
        Result = Expressions.mul(L, R);
        break;
      case llvm::Instruction::UDiv:
        Result = Expressions.udiv(L, R);
        break;
      case llvm::Instruction::SDiv:
        Result = Expressions.sdiv(L, R);
        break;
      case llvm::Instruction::URem:
        Result = Expressions.urem(L, R);
        break;
      case llvm::Instruction::SRem:
        Result = Expressions.srem(L, R);
        break;
      case llvm::Instruction::And:
        Result = Expressions.bitAnd(L, R);
        break;
      case llvm::Instruction::Or:
        Result = Expressions.bitOr(L, R);
        break;
      case llvm::Instruction::Xor:
        Result = Expressions.bitXor(L, R);
        break;
      case llvm::Instruction::Shl:
        Result = Expressions.shl(L, R);
        break;
      case llvm::Instruction::LShr:
        Result = Expressions.lshr(L, R);
        break;
      case llvm::Instruction::AShr:
        Result = Expressions.ashr(L, R);
        break;
      default:
        incomplete(State, IncompletenessReason::UnsupportedInstruction,
                   "unsupported binary opcode " +
                       std::string(Binary.getOpcodeName()));
        return;
      }
      bindResult(Binary, State, SymbolicValue(std::move(Result)));
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
    }
  }

  void executeICmp(llvm::ICmpInst &Compare, ExecutionState &State) {
    std::optional<SymbolicValue> Left = resolve(Compare.getOperand(0), State);
    if (!Left)
      return;
    std::optional<SymbolicValue> Right = resolve(Compare.getOperand(1), State);
    if (!Right)
      return;
    try {
      if (Left->isPointer() || Right->isPointer()) {
        if (!Left->isPointer() || !Right->isPointer() ||
            (Compare.getPredicate() != llvm::ICmpInst::ICMP_EQ &&
             Compare.getPredicate() != llvm::ICmpInst::ICMP_NE)) {
          incomplete(State, IncompletenessReason::UnsupportedType,
                     "M4 supports pointer eq/ne only");
          return;
        }
        const PointerValue &A = Left->pointer();
        const PointerValue &B = Right->pointer();
        if (A.AddressSpace != 0 || B.AddressSpace != 0 ||
            A.Width != B.Width) {
          incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                     "pointer comparison address spaces differ");
          return;
        }
        if (!pointerObjectsBelongToState(A, State) ||
            !pointerObjectsBelongToState(B, State)) {
          incomplete(State, IncompletenessReason::PointerProvenanceLost,
                     "pointer comparison uses an object outside this state");
          return;
        }
        for (const PointerValue *Pointer : {&A, &B})
          for (const PointerCandidate &Candidate : Pointer->Candidates)
            if (Candidate.Object &&
                (Candidate.Object->Kind == AllocationKind::Heap ||
                 Candidate.Object->Kind == AllocationKind::New) &&
                !State.addressSpace().find(Candidate.Object->Id)->State->live()) {
              incomplete(State,
                         IncompletenessReason::PointerComparisonIncomplete,
                         "comparison of a freed heap pointer is unsupported");
              return;
            }
        ExprRef Equal = Expressions.boolean(false);
        for (const PointerCandidate &ACandidate : A.Candidates) {
          for (const PointerCandidate &BCandidate : B.Candidates) {
            const bool DifferentObjects =
                ACandidate.Object && BCandidate.Object &&
                ACandidate.Object.get() != BCandidate.Object.get();
            if (DifferentObjects &&
                ACandidate.Object->Kind == AllocationKind::Global &&
                BCandidate.Object->Kind == AllocationKind::Global &&
                ACandidate.Object->ReadOnly && BCandidate.Object->ReadOnly &&
                (ACandidate.Object->MergeableGlobalAddress ||
                 BCandidate.Object->MergeableGlobalAddress)) {
              incomplete(State,
                         IncompletenessReason::PointerComparisonIncomplete,
                         "constant global addresses may be merged");
              return;
            }
            const bool ObjectVsNull =
                static_cast<bool>(ACandidate.Object) !=
                static_cast<bool>(BCandidate.Object);
            const auto IsBase = [](const PointerCandidate &Candidate) {
              return Candidate.Offset && Candidate.Offset->isConstant() &&
                     Candidate.Offset->constantValue().isZero();
            };
            const auto UnprovenZeroSizedAddress =
                [](const PointerCandidate &Candidate) {
                  return Candidate.Object && Candidate.Object->Size == 0 &&
                         Candidate.Object->Kind != AllocationKind::Heap &&
                         Candidate.Object->Kind != AllocationKind::New;
                };
            if ((DifferentObjects || ObjectVsNull) &&
                (!IsBase(ACandidate) || !IsBase(BCandidate) ||
                 UnprovenZeroSizedAddress(ACandidate) ||
                 UnprovenZeroSizedAddress(BCandidate))) {
              incomplete(State, IncompletenessReason::PointerComparisonIncomplete,
                         "cross-provenance equality may depend on target addresses");
              return;
            }
            ExprRef Same = Expressions.boolean(false);
            if (!ACandidate.Object && !BCandidate.Object)
              Same = Expressions.eq(ACandidate.Offset, BCandidate.Offset);
            else if (ACandidate.Object && BCandidate.Object &&
                     ACandidate.Object.get() == BCandidate.Object.get())
              Same = Expressions.eq(ACandidate.Offset, BCandidate.Offset);
            if (ACandidate.Guard)
              Same = Expressions.select(ACandidate.Guard, Same,
                                        Expressions.boolean(false));
            if (BCandidate.Guard)
              Same = Expressions.select(BCandidate.Guard, Same,
                                        Expressions.boolean(false));
            Equal = Expressions.select(Same, Expressions.boolean(true), Equal);
          }
        }
        if (Compare.getPredicate() == llvm::ICmpInst::ICMP_NE)
          Equal = Expressions.logicalNot(Equal);
        bindResult(Compare, State, boolToI1(Equal));
        return;
      }
      ExprRef Condition;
      switch (Compare.getPredicate()) {
      case llvm::ICmpInst::ICMP_EQ:
        Condition = Expressions.eq(Left->bitVector(), Right->bitVector());
        break;
      case llvm::ICmpInst::ICMP_NE:
        Condition = Expressions.logicalNot(
            Expressions.eq(Left->bitVector(), Right->bitVector()));
        break;
      case llvm::ICmpInst::ICMP_ULT:
        Condition = Expressions.ult(Left->bitVector(), Right->bitVector());
        break;
      case llvm::ICmpInst::ICMP_ULE:
        Condition = Expressions.ule(Left->bitVector(), Right->bitVector());
        break;
      case llvm::ICmpInst::ICMP_UGT:
        Condition = Expressions.ult(Right->bitVector(), Left->bitVector());
        break;
      case llvm::ICmpInst::ICMP_UGE:
        Condition = Expressions.ule(Right->bitVector(), Left->bitVector());
        break;
      case llvm::ICmpInst::ICMP_SLT:
        Condition = Expressions.slt(Left->bitVector(), Right->bitVector());
        break;
      case llvm::ICmpInst::ICMP_SLE:
        Condition = Expressions.sle(Left->bitVector(), Right->bitVector());
        break;
      case llvm::ICmpInst::ICMP_SGT:
        Condition = Expressions.slt(Right->bitVector(), Left->bitVector());
        break;
      case llvm::ICmpInst::ICMP_SGE:
        Condition = Expressions.sle(Right->bitVector(), Left->bitVector());
        break;
      default:
        incomplete(State, IncompletenessReason::UnsupportedInstruction,
                   "unsupported icmp predicate");
        return;
      }
      bindResult(Compare, State, boolToI1(Condition));
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
    }
  }

  void executeCast(llvm::CastInst &Cast, ExecutionState &State) {
    if (Cast.getOpcode() == llvm::Instruction::IntToPtr) {
      incomplete(State, IncompletenessReason::PointerProvenanceLost,
                 "inttoptr cannot reconstruct object provenance");
      return;
    }
    if (!Cast.getOperand(0)->getType()->isIntegerTy() ||
        !Cast.getType()->isIntegerTy()) {
      incomplete(State, IncompletenessReason::UnsupportedCast,
                 "M3 rejects pointer/integer and non-integer casts");
      return;
    }
    std::optional<SymbolicValue> Input = resolve(Cast.getOperand(0), State);
    if (!Input)
      return;
    unsigned ResultWidth =
        llvm::cast<llvm::IntegerType>(Cast.getType())->getBitWidth();
    try {
      ExprRef Result;
      switch (Cast.getOpcode()) {
      case llvm::Instruction::Trunc:
        Result = Expressions.trunc(Input->bitVector(), ResultWidth);
        break;
      case llvm::Instruction::ZExt:
        Result = Expressions.zext(Input->bitVector(), ResultWidth);
        break;
      case llvm::Instruction::SExt:
        Result = Expressions.sext(Input->bitVector(), ResultWidth);
        break;
      case llvm::Instruction::BitCast:
        if (Input->width() != ResultWidth) {
          incomplete(State, IncompletenessReason::UnsupportedCast,
                     "integer bitcast changes bit width");
          return;
        }
        Result = Input->bitVector();
        break;
      default:
        incomplete(State, IncompletenessReason::UnsupportedCast,
                   "unsupported integer cast opcode " +
                       std::string(Cast.getOpcodeName()));
        return;
      }
      bindResult(Cast, State, SymbolicValue(std::move(Result)));
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
    }
  }

  void executeSelect(llvm::SelectInst &Select, ExecutionState &State) {
    std::optional<SymbolicValue> Condition =
        resolve(Select.getCondition(), State);
    if (!Condition)
      return;
    std::optional<SymbolicValue> TrueValue =
        resolve(Select.getTrueValue(), State);
    if (!TrueValue)
      return;
    std::optional<SymbolicValue> FalseValue =
        resolve(Select.getFalseValue(), State);
    if (!FalseValue)
      return;
    try {
      if (TrueValue->isPointer() || FalseValue->isPointer()) {
        if (!TrueValue->isPointer() || !FalseValue->isPointer()) {
          internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                        "select pointer arms have different value kinds");
          return;
        }
        PointerValue Result = TrueValue->pointer();
        const PointerValue &Other = FalseValue->pointer();
        if (Result.Candidates.size() > Options.MaxPointerCandidates ||
            Other.Candidates.size() >
                Options.MaxPointerCandidates - Result.Candidates.size()) {
          ++Statistics.PointerCapHits;
          incomplete(State,
                     IncompletenessReason::PointerResolutionIncomplete,
                     "pointer select exceeds candidate cap");
          return;
        }
        if (Result.Width != Other.Width ||
            Result.AddressSpace != Other.AddressSpace) {
          incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                     "select pointer arms have incompatible layouts");
          return;
        }
        ExprRef Guard = toBool(*Condition);
        ExprRef NotGuard = Expressions.logicalNot(Guard);
        for (PointerCandidate &Candidate : Result.Candidates)
          Candidate.Guard = Expressions.select(
              Guard,
              Candidate.Guard ? Candidate.Guard : Expressions.boolean(true),
              Expressions.boolean(false));
        for (PointerCandidate Candidate : Other.Candidates) {
          Candidate.Guard = Expressions.select(
              NotGuard,
              Candidate.Guard ? Candidate.Guard : Expressions.boolean(true),
              Expressions.boolean(false));
          Result.Candidates.push_back(std::move(Candidate));
        }
        Result.Numeric = Result.Numeric && Other.Numeric
                             ? Expressions.select(Guard, Result.Numeric,
                                                  Other.Numeric)
                             : nullptr;
        bindResult(Select, State, SymbolicValue(std::move(Result)));
        return;
      }
      // Select is a value expression.  It never enters the state scheduler and
      // therefore does not add path constraints or increment fork/query stats.
      ExprRef Result = Expressions.select(toBool(*Condition),
                                          TrueValue->bitVector(),
                                          FalseValue->bitVector());
      bindResult(Select, State, SymbolicValue(std::move(Result)));
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
    }
  }

  void executeAlloca(llvm::AllocaInst &Alloca, ExecutionState &State) {
    if (Alloca.getAddressSpace() != 0) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "alloca uses a non-default address space");
      return;
    }
    auto ElementSize =
        fixedSize(layout(State).getTypeAllocSize(Alloca.getAllocatedType()));
    if (!ElementSize) {
      incomplete(State, IncompletenessReason::ScalableType,
                 "alloca element has scalable TypeSize");
      return;
    }
    auto Count = resolve(Alloca.getArraySize(), State);
    if (!Count)
      return;
    if (Count->isPointer() || !Count->bitVector()->isConstant()) {
      incomplete(State, IncompletenessReason::SymbolicAllocationSize,
                 "symbolic alloca element count is unsupported");
      return;
    }
    const llvm::APInt &CountValue = Count->bitVector()->constantValue();
    if (CountValue.ugt(std::numeric_limits<std::uint64_t>::max())) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "alloca element count exceeds host range");
      return;
    }
    std::uint64_t Total = 0;
    if (!checkedMul(*ElementSize, CountValue.getZExtValue(), Total)) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "alloca byte size overflows uint64");
      return;
    }
    std::uint64_t Alignment = Alloca.getAlign().value();
    auto Object = allocateObject(
        State, AllocationKind::Stack, Total, Alignment,
        locationOf(&Alloca).Function + ":" +
            std::to_string(locationOf(&Alloca).InstructionOrdinal),
        false);
    if (!Object)
      return;
    State.frame().addStackObject(Object->Id);
    unsigned Width = layout(State).getPointerSizeInBits(0);
    bindResult(Alloca, State,
               SymbolicValue(PointerValue::object(
                   Object, Expressions.constant(Width, 0), Expressions)));
  }

  void executeGEP(llvm::GetElementPtrInst &GEP, ExecutionState &State) {
    auto Base = resolve(GEP.getPointerOperand(), State);
    if (!Base)
      return;
    if (!Base->isPointer()) {
      internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                    "GEP base is not a pointer value");
      return;
    }
    auto Result = computeGEP(*llvm::cast<llvm::GEPOperator>(&GEP),
                             Base->pointer(), State);
    if (Result)
      bindResult(GEP, State, SymbolicValue(std::move(*Result)));
  }

  void executeExtractValue(llvm::ExtractValueInst &Extract,
                           ExecutionState &State) {
    auto Aggregate = resolve(Extract.getAggregateOperand(), State);
    if (!Aggregate)
      return;
    if (!Aggregate->isAggregate()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "extractvalue operand is not an aggregate");
      return;
    }
    auto Element = aggregateElement(Extract.getAggregateOperand()->getType(),
                                    Extract.getIndices(), State);
    if (!Element || Element->first != Extract.getType()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "extractvalue has unsupported element layout");
      return;
    }
    auto Size = fixedSize(layout(State).getTypeStoreSize(Element->first));
    const AggregateValue &Source = Aggregate->aggregate();
    if (!Size || Element->second > Source.Bytes.size() ||
        *Size > Source.Bytes.size() - Element->second) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "extractvalue exceeds aggregate extent");
      return;
    }
    const std::uint64_t At = Element->second;
    if (Element->first->isAggregateType()) {
      auto Nested = emptyAggregate(Element->first, State);
      if (!Nested)
        return;
      for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
        Nested->Bytes[Byte] = Source.Bytes[At + Byte];
        Nested->KnownMasks[Byte] = Source.KnownMasks[At + Byte];
        Nested->AddressSensitiveBytes[Byte] = Source.AddressSensitiveBytes[At + Byte];
      }
      for (const auto &[Start, Shadow] : Source.Shadows)
        if (Start >= At && Start - At <= *Size &&
            Shadow.Size <= *Size - (Start - At)) {
          PointerShadowEntry Copied = Shadow;
          Copied.Start -= At;
          Nested->Shadows.emplace(Copied.Start, std::move(Copied));
        }
      auto NestedLayout = aggregateLayout(Element->first, State);
      if (!NestedLayout)
        return;
      discardAggregatePadding(*Nested, *NestedLayout);
      bindResult(Extract, State, SymbolicValue(std::move(*Nested)));
      return;
    }
    if (Element->first->isPointerTy()) {
      auto It = Source.Shadows.find(At);
      if (It == Source.Shadows.end() || It->second.Size != *Size ||
          It->second.AddressSpace != 0) {
        incomplete(State, IncompletenessReason::PointerProvenanceLost,
                   "extracted pointer has no compatible provenance shadow");
        return;
      }
      bindResult(Extract, State, SymbolicValue(It->second.Pointer));
      return;
    }
    if (!Element->first->isIntegerTy()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "unsupported extractvalue scalar type");
      return;
    }
    const unsigned Width = Element->first->getIntegerBitWidth();
    ExprRef Bits;
    for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
      const std::uint64_t LogicalByte =
          layout(State).isLittleEndian() ? Byte : *Size - 1 - Byte;
      const std::uint64_t Remaining =
          Width > LogicalByte * 8 ? Width - LogicalByte * 8 : 0;
      const std::uint8_t Required = Remaining >= 8 ? 0xff
                                   : Remaining == 0 ? 0
                                   : static_cast<std::uint8_t>((1U << Remaining) - 1);
      ExprRef Known = Source.KnownMasks[At + Byte];
      ExprRef Ready = Expressions.eq(
          Expressions.bitAnd(Known, Expressions.constant(8, Required)),
          Expressions.constant(8, Required));
      ExprRef Sensitive = Source.AddressSensitiveBytes[At + Byte];
      if (!Ready->isConstant() || Ready->constantValue().isZero() ||
          !Sensitive->isConstant() || !Sensitive->constantValue().isZero()) {
        incomplete(State, IncompletenessReason::UnsupportedAggregate,
                   "extracted scalar has unknown or address-sensitive bytes");
        return;
      }
      ExprRef Part = Source.Bytes[At + Byte];
      Bits = !Bits ? Part
           : layout(State).isLittleEndian() ? Expressions.concat(Part, Bits)
                                            : Expressions.concat(Bits, Part);
    }
    if (!Bits) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "zero-size extracted scalar");
      return;
    }
    bindResult(Extract, State, SymbolicValue(
        Width == *Size * 8 ? Bits : Expressions.trunc(Bits, Width)));
  }

  void executeInsertValue(llvm::InsertValueInst &Insert,
                          ExecutionState &State) {
    auto Aggregate = resolve(Insert.getAggregateOperand(), State);
    auto Inserted = resolve(Insert.getInsertedValueOperand(), State);
    if (!Aggregate || !Inserted)
      return;
    if (!Aggregate->isAggregate()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "insertvalue operand is not an aggregate");
      return;
    }
    auto Element = aggregateElement(Insert.getType(), Insert.getIndices(), State);
    if (!Element || Element->first != Insert.getInsertedValueOperand()->getType()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "insertvalue has unsupported element layout");
      return;
    }
    AggregateValue Result = Aggregate->aggregate();
    if (!placeAggregateElement(Result, Element->first, Element->second,
                               *Inserted, State))
      return;
    bindResult(Insert, State, SymbolicValue(std::move(Result)));
  }

  QueryOutcome resolutionQuery(const ExecutionState &State,
                               const ExprRef &Guard) {
    if (Guard->isConstant())
      return {Guard->constantValue().isZero()
                  ? QueryDisposition::Infeasible
                  : QueryDisposition::Feasible,
              IncompletenessReason::None, {}};
    auto Start = std::chrono::steady_clock::now();
    ++Statistics.ResolutionSolverQueries;
    QueryOutcome Result = query(State, Guard);
    auto End = std::chrono::steady_clock::now();
    Statistics.ResolutionSolverMicroseconds +=
        static_cast<std::uint64_t>(
            std::chrono::duration_cast<std::chrono::microseconds>(End - Start)
                .count());
    return Result;
  }

  bool requireMemoryGuard(ExecutionState &State, const ExprRef &Good,
                          PathErrorReason BadError,
                          IncompletenessReason BadUnknown,
                          const char *Detail) {
    ExprRef Bad = Expressions.logicalNot(Good);
    const ExecutionState Base = State;
    QueryOutcome GoodResult = resolutionQuery(Base, Good);
    QueryOutcome BadResult = resolutionQuery(Base, Bad);
    if (GoodResult.Disposition == QueryDisposition::Infeasible)
      ++Statistics.PrunedPaths;
    if (BadResult.Disposition == QueryDisposition::Infeasible)
      ++Statistics.PrunedPaths;
    if (GoodResult.Disposition == QueryDisposition::Infeasible &&
        BadResult.Disposition == QueryDisposition::Infeasible) {
      internalError(State, InternalErrorReason::NoFeasibleSuccessor,
                    "both memory guard polarities are UNSAT");
      return false;
    }
    if (BadResult.Disposition != QueryDisposition::Infeasible) {
      ExecutionState BadState = Base;
      BadState.setId(allocateStateId());
      BadState.setDepth(Base.depth() + 1);
      ++Statistics.Forks;
      ++Statistics.PointerForks;
      if (Statistics.StatesCreated > Options.MaxStates) {
        incomplete(BadState, IncompletenessReason::StateLimit,
                   "state limit reached during memory guard split");
      } else if (addConstraint(BadState, Bad)) {
        if (BadResult.Disposition == QueryDisposition::Feasible) {
          if (BadUnknown != IncompletenessReason::None)
            incomplete(BadState, BadUnknown, Detail);
          else
            pathError(BadState, BadError, Detail);
        } else if (BadResult.Disposition == QueryDisposition::Incomplete)
          incomplete(BadState, BadResult.Reason, BadResult.Detail);
        else
          internalError(BadState, InternalErrorReason::SolverInternalError,
                        BadResult.Detail);
      }
    }
    if (GoodResult.Disposition == QueryDisposition::Feasible)
      return addConstraint(State, Good);
    if (GoodResult.Disposition == QueryDisposition::Incomplete)
      incomplete(State, GoodResult.Reason, GoodResult.Detail);
    else if (GoodResult.Disposition == QueryDisposition::InternalError)
      internalError(State, InternalErrorReason::SolverInternalError,
                    GoodResult.Detail);
    else
      State.setStatus(ExecutionStateStatus::Scheduled);
    return false;
  }

  void executeMemoryAccess(llvm::Instruction &Instruction,
                           ExecutionState &State) {
    auto *Load = llvm::dyn_cast<llvm::LoadInst>(&Instruction);
    auto *Store = llvm::dyn_cast<llvm::StoreInst>(&Instruction);
    const bool IsStore = Store != nullptr;
    if (!Load && !Store) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "memory access dispatcher received non-memory opcode");
      return;
    }
    if ((Load && (Load->isAtomic() || Load->isVolatile())) ||
        (Store && (Store->isAtomic() || Store->isVolatile()))) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "atomic or volatile memory access is unsupported");
      return;
    }
    llvm::Type *ValueType =
        IsStore ? Store->getValueOperand()->getType() : Load->getType();
    if (!ValueType->isIntegerTy() && !ValueType->isPointerTy() &&
        !ValueType->isAggregateType()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "load/store type is unsupported");
      return;
    }
    if (auto *PointerType = llvm::dyn_cast<llvm::PointerType>(ValueType);
        PointerType && PointerType->getAddressSpace() != 0) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "non-default pointer value address space");
      return;
    }
    auto Size = fixedSize(layout(State).getTypeStoreSize(ValueType));
    if (!Size) {
      incomplete(State, IncompletenessReason::ScalableType,
                 "load/store type has scalable store size");
      return;
    }
    if (*Size == 0 || *Size > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::UnsupportedType,
                 "load/store type has zero or excessive size");
      return;
    }
    std::optional<AggregateLayout> AggregateTypeLayout;
    if (ValueType->isAggregateType()) {
      AggregateTypeLayout = aggregateLayout(ValueType, State);
      if (!AggregateTypeLayout)
        return;
      if (AggregateTypeLayout->SemanticMasks.size() != *Size) {
        incomplete(State, IncompletenessReason::UnsupportedAggregate,
                   "aggregate load/store extent does not match field layout");
        return;
      }
    }
    unsigned Alignment =
        IsStore ? Store->getAlign().value() : Load->getAlign().value();
    auto Pointer = resolve(IsStore ? Store->getPointerOperand()
                                   : Load->getPointerOperand(),
                           State);
    if (!Pointer)
      return;
    if (!Pointer->isPointer()) {
      internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                    "load/store pointer operand lost pointer type");
      return;
    }
    std::optional<SymbolicValue> Stored;
    if (IsStore) {
      Stored = resolve(Store->getValueOperand(), State);
      if (!Stored)
        return;
    }
    const PointerValue &Address = Pointer->pointer();
    if (Address.AddressSpace != 0) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "memory access uses a non-default address space");
      return;
    }
    ++Statistics.PointerResolutions;
    Statistics.PointerCandidates += Address.Candidates.size();
    if (Address.Candidates.empty()) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "pointer has no provenance candidates");
      return;
    }
    if (Address.Candidates.size() > Options.MaxPointerCandidates) {
      ++Statistics.PointerCapHits;
      incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                 "pointer candidate cap reached");
      return;
    }
    ExecutionState Base = std::move(State);
    bool UsedParentId = false;
    std::vector<ExecutionState> Successors;
    for (const PointerCandidate &Candidate : Address.Candidates) {
      ExecutionState Child = Address.Candidates.size() == 1
                                 ? std::move(Base)
                                 : ExecutionState(Base);
      if (UsedParentId)
        Child.setId(allocateStateId());
      else
        UsedParentId = true;
      if (Address.Candidates.size() > 1)
        Child.setDepth(Base.depth() + 1);
      if (Statistics.StatesCreated > Options.MaxStates) {
        incomplete(Child, IncompletenessReason::StateLimit,
                   "state limit reached during pointer resolution");
        continue;
      }
      if (Candidate.Guard) {
        QueryOutcome Outcome = resolutionQuery(Child, Candidate.Guard);
        if (Outcome.Disposition == QueryDisposition::Infeasible) {
          ++Statistics.PrunedPaths;
          continue;
        }
        if (!addConstraint(Child, Candidate.Guard))
          continue;
        if (Outcome.Disposition == QueryDisposition::Incomplete) {
          incomplete(Child, Outcome.Reason, Outcome.Detail);
          continue;
        }
        if (Outcome.Disposition == QueryDisposition::InternalError) {
          internalError(Child, InternalErrorReason::SolverInternalError,
                        Outcome.Detail);
          continue;
        }
      }
      if (!Candidate.Object) {
        if (Candidate.Offset && Candidate.Offset->isConstant() &&
            Candidate.Offset->constantValue().isZero()) {
          if (Instruction.getFunction()->hasFnAttribute(
                  llvm::Attribute::NullPointerIsValid))
            incomplete(Child, IncompletenessReason::PointerProvenanceLost,
                       "null address is valid in this function, but its memory is not modeled");
          else
            pathError(Child, PathErrorReason::NullDereference,
                      "nonzero memory access through null pointer");
        } else
          incomplete(Child, IncompletenessReason::PointerProvenanceLost,
                     "null-derived nonzero address has no recoverable object provenance");
        continue;
      }
      const AddressSpace::Entry *Entry =
          Child.addressSpace().find(Candidate.Object->Id);
      if (!Entry || Entry->Object.get() != Candidate.Object.get()) {
        incomplete(Child, IncompletenessReason::PointerProvenanceLost,
                   "pointer object is not bound in this address space");
        continue;
      }
      if (!Entry->State->live()) {
        pathError(Child, (Entry->Object->Kind == AllocationKind::Heap ||
                          Entry->Object->Kind == AllocationKind::New)
                             ? PathErrorReason::UseAfterFree
                             : PathErrorReason::DeadObjectAccess,
                  "memory access to a dead object", Entry->Object->Id);
        continue;
      }
      const MemoryObject &Object = *Entry->Object;
      ExprRef Offset = Candidate.Offset;
      ExprRef InRange = Expressions.boolean(false);
      if (*Size <= Object.Size) {
        ExprRef Nonnegative = Expressions.logicalNot(Expressions.slt(
            Offset, Expressions.constant(Address.Width, 0)));
        ExprRef BeforeEnd = Expressions.ule(
            Offset, Expressions.constant(Address.Width, Object.Size - *Size));
        InRange = Expressions.select(Nonnegative, BeforeEnd,
                                     Expressions.boolean(false));
      }
      if (!requireMemoryGuard(Child, InRange, PathErrorReason::OutOfBounds,
                              IncompletenessReason::None,
                              "memory access crosses object bounds"))
        continue;
      if (Object.Alignment < Alignment) {
        incomplete(Child, IncompletenessReason::AlignmentUndetermined,
                   "declared access alignment exceeds guaranteed object alignment");
        continue;
      }
      ExprRef Aligned = Expressions.boolean(true);
      if (Alignment > 1) {
        ExprRef Low = Expressions.bitAnd(
            Offset, Expressions.constant(Address.Width, Alignment - 1));
        Aligned = Expressions.eq(
            Low, Expressions.constant(Address.Width, 0));
      }
      if (!requireMemoryGuard(Child, Aligned,
                              PathErrorReason::MisalignedAccess,
                              IncompletenessReason::None,
                              "memory access violates declared alignment"))
        continue;
      if (IsStore && Object.ReadOnly) {
        pathError(Child, PathErrorReason::ReadOnlyWrite,
                  "write to constant global object");
        continue;
      }
      if (!IsStore && !ValueType->isAggregateType()) {
        ExprRef Initialized = Expressions.boolean(true);
        for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
          std::uint8_t RequiredMask = 0xff;
          if (ValueType->isIntegerTy()) {
            const std::uint64_t LogicalByte =
                layout(Child).isLittleEndian() ? Byte : *Size - 1 - Byte;
            const unsigned Bits =
                llvm::cast<llvm::IntegerType>(ValueType)->getBitWidth();
            const std::uint64_t Remaining =
                Bits > LogicalByte * 8 ? Bits - LogicalByte * 8 : 0;
            if (Remaining < 8)
              RequiredMask = Remaining == 0
                                 ? 0
                                 : static_cast<std::uint8_t>(
                                       (1U << Remaining) - 1);
          }
          ExprRef ByteReady = Entry->State->initializedAt(
              byteOffset(Offset, Byte), Expressions, RequiredMask);
          Initialized = Expressions.select(
              Initialized, ByteReady, Expressions.boolean(false));
        }
        if (!requireMemoryGuard(Child, Initialized,
                                PathErrorReason::None,
                                Object.ExternallyInitialized
                                    ? IncompletenessReason::UnsupportedGlobal
                                    : IncompletenessReason::UninitializedRead,
                                Object.ExternallyInitialized
                                    ? "read of an externally initialized global with unknown entry value"
                                    : "read of an uninitialized byte"))
          continue;
      }
      if (IsStore) {
        bool Copied = false;
        auto Writable =
            Child.addressSpace().getWriteable(Object.Id, Copied);
        if (!Writable) {
          internalError(Child, InternalErrorReason::MemoryModelError,
                        "writable object disappeared");
          continue;
        }
        if (Copied) {
          ++Statistics.COWCopies;
          ++Statistics.ObjectStates;
        }
        if (Stored->isAggregate()) {
          AggregateValue Aggregate = Stored->aggregate();
          if (Aggregate.Type != ValueType || Aggregate.Bytes.size() < *Size) {
            incomplete(Child, IncompletenessReason::UnsupportedAggregate,
                       "stored aggregate extent does not match its type");
            continue;
          }
          discardAggregatePadding(Aggregate, *AggregateTypeLayout);
          for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
            Writable->writeByteWithMask(
                byteOffset(Offset, Byte), Aggregate.Bytes[Byte],
                Aggregate.KnownMasks[Byte], Expressions, false,
                Aggregate.AddressSensitiveBytes[Byte]);
            ++Statistics.ByteWriteExprs;
          }
          auto Start = concreteUnsigned(Offset);
          if (!Aggregate.Shadows.empty() && !Start) {
            incomplete(Child, IncompletenessReason::PointerResolutionIncomplete,
                       "aggregate pointer shadows require a concrete store offset");
            continue;
          }
          for (const auto &[Relative, Shadow] : Aggregate.Shadows) {
            if (Relative > *Size || Shadow.Size > *Size - Relative)
              continue;
            PointerShadowEntry CopiedShadow = Shadow;
            CopiedShadow.Start += *Start;
            Writable->installShadow(std::move(CopiedShadow));
          }
        } else {
          ExprRef Bits = Stored->isPointer() ? Stored->pointer().Numeric
                                             : Stored->bitVector();
          if (!Bits) {
            incomplete(Child, IncompletenessReason::PointerProvenanceLost,
                       "stored pointer has no numeric representation");
            continue;
          }
          writeIntegerBytes(*Writable, Offset, Bits, *Size,
                            layout(Child).isLittleEndian(), false,
                            Expressions.boolean(
                                Stored->isPointer() &&
                                std::any_of(
                                    Stored->pointer().Candidates.begin(),
                                    Stored->pointer().Candidates.end(),
                                    [](const PointerCandidate &Candidate) {
                                      return static_cast<bool>(Candidate.Object);
                                    })));
          if (Stored->isPointer()) {
          auto Start = concreteUnsigned(Offset);
          if (Start)
            Writable->installShadow(
                {*Start, *Size, Stored->width(),
                 Stored->pointer().AddressSpace, Stored->pointer(), Bits});
          }
        }
        Child.pc().advance();
      } else if (ValueType->isAggregateType()) {
        auto Aggregate = emptyAggregate(ValueType, Child);
        if (!Aggregate)
          continue;
        for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
          if (AggregateTypeLayout->SemanticMasks[Byte] == 0)
            continue;
          ExprRef At = byteOffset(Offset, Byte);
          Aggregate->Bytes[Byte] = Entry->State->readByte(At, Expressions);
          Aggregate->KnownMasks[Byte] =
              Entry->State->knownMaskAt(At, Expressions);
          Aggregate->AddressSensitiveBytes[Byte] =
              Entry->State->addressSensitiveAt(At, Expressions);
          ++Statistics.ByteReadExprs;
        }
        auto Start = concreteUnsigned(Offset);
        if (Start)
          for (const auto &[ShadowAt, Shadow] : Entry->State->shadows())
            if (ShadowAt >= *Start && ShadowAt - *Start <= *Size &&
                Shadow.Size <= *Size - (ShadowAt - *Start)) {
              PointerShadowEntry CopiedShadow = Shadow;
              CopiedShadow.Start -= *Start;
              Aggregate->Shadows.emplace(CopiedShadow.Start,
                                         std::move(CopiedShadow));
            }
        discardAggregatePadding(*Aggregate, *AggregateTypeLayout);
        bindResult(Instruction, Child, SymbolicValue(std::move(*Aggregate)));
      } else if (ValueType->isPointerTy()) {
        auto Start = concreteUnsigned(Offset);
        const PointerShadowEntry *Shadow =
            Start ? Entry->State->findShadow(*Start, *Size,
                                              layout(Child).getPointerSizeInBits(0),
                                              0)
                  : nullptr;
        if (!Shadow) {
          incomplete(Child, IncompletenessReason::PointerProvenanceLost,
                     "pointer bytes have no compatible provenance shadow");
          continue;
        }
        bindResult(Instruction, Child, SymbolicValue(Shadow->Pointer));
      } else {
        ExprRef AddressSensitive = Expressions.boolean(false);
        for (std::uint64_t Byte = 0; Byte < *Size; ++Byte) {
          ExprRef Tainted = Entry->State->addressSensitiveAt(
              byteOffset(Offset, Byte), Expressions);
          AddressSensitive = Expressions.select(
              AddressSensitive, Expressions.boolean(true), Tainted);
        }
        if (!requireMemoryGuard(
                Child, Expressions.logicalNot(AddressSensitive),
                PathErrorReason::None,
                IncompletenessReason::PointerProvenanceLost,
                "integer load observes target-dependent pointer bytes"))
          continue;
        unsigned Width = llvm::cast<llvm::IntegerType>(ValueType)->getBitWidth();
        ExprRef Bits = readIntegerBytes(*Entry->State, Offset, *Size, Width,
                                        layout(Child).isLittleEndian());
        bindResult(Instruction, Child, SymbolicValue(std::move(Bits)));
      }
      if (Child.status() == ExecutionStateStatus::Running)
        if (Options.EnableRegistration && !IsStore) {
          if (!AggregateTypeLayout)
            Child.memoryReads().push_back({Object.Id, Offset, *Size});
          else
            for (std::uint64_t Byte = 0; Byte < *Size;) {
              if (AggregateTypeLayout->SemanticMasks[Byte] == 0) {
                ++Byte;
                continue;
              }
              const std::uint64_t Start = Byte;
              while (Byte < *Size &&
                     AggregateTypeLayout->SemanticMasks[Byte] != 0)
                ++Byte;
              Child.memoryReads().push_back(
                  {Object.Id, byteOffset(Offset, Start), Byte - Start});
            }
        }
      if (Child.status() == ExecutionStateStatus::Running)
        Successors.push_back(std::move(Child));
    }
    if (Successors.size() > 1) {
      Statistics.Forks += Successors.size() - 1;
      Statistics.PointerForks += Successors.size() - 1;
    }
    for (auto It = Successors.rbegin(); It != Successors.rend(); ++It)
      Worklist.schedule(std::move(*It));
    State.setStatus(ExecutionStateStatus::Scheduled);
  }

  void executeMemTransfer(llvm::MemTransferInst &Copy,
                          ExecutionState &State) {
    if (llvm::isa<llvm::MemCpyInlineInst>(Copy)) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "llvm.memcpy.inline is outside the M6 intrinsic contract");
      return;
    }
    const bool IsMove = llvm::isa<llvm::MemMoveInst>(Copy);
    if (hasUnsupportedMemcpyParamAttributes(Copy)) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "unsupported llvm.memcpy parameter attribute");
      return;
    }
    if (IsMove &&
        (Copy.paramHasAttr(0, llvm::Attribute::NoAlias) ||
         Copy.paramHasAttr(1, llvm::Attribute::NoAlias) ||
         Copy.getCalledFunction()->getAttributes().hasParamAttr(
             0, llvm::Attribute::NoAlias) ||
         Copy.getCalledFunction()->getAttributes().hasParamAttr(
             1, llvm::Attribute::NoAlias))) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "memmove noalias preconditions are not modeled");
      return;
    }
    if (Copy.isVolatile()) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "volatile memcpy is unsupported");
      return;
    }
    auto LengthValue = resolve(Copy.getLength(), State);
    if (!LengthValue)
      return;
    auto Length = LengthValue->isPointer()
                      ? std::optional<std::uint64_t>{}
                      : concreteUnsigned(LengthValue->bitVector());
    if (!Length || *Length > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "memcpy requires a bounded concrete byte count");
      return;
    }
    for (unsigned Arg = 0; Arg < Copy.arg_size(); ++Arg)
      if (Copy.paramHasAttr(Arg, llvm::Attribute::NoUndef) &&
          !resolve(Copy.getArgOperand(Arg), State))
        return;
    if (*Length == 0) {
      const auto AlignmentIsProven = [](llvm::MaybeAlign Alignment,
                                       const llvm::Value *Pointer) {
        return !Alignment || Alignment->value() == 1 ||
               llvm::isa<llvm::ConstantPointerNull>(Pointer);
      };
      if (!AlignmentIsProven(Copy.getDestAlign(), Copy.getRawDest()) ||
          !AlignmentIsProven(Copy.getSourceAlign(), Copy.getRawSource())) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "zero-length memcpy pointer alignment attribute is unproved");
        return;
      }
      State.pc().advance();
      return;
    }
    auto Dest = resolve(Copy.getRawDest(), State);
    auto Source = resolve(Copy.getRawSource(), State);
    if (!Dest || !Source)
      return;
    if (!Dest->isPointer() || !Source->isPointer()) {
      internalError(State, InternalErrorReason::InvalidSymbolicBinding,
                    "memcpy pointer operand has non-pointer binding");
      return;
    }
    const PointerValue &DestPointer = Dest->pointer();
    const PointerValue &SourcePointer = Source->pointer();
    if (DestPointer.AddressSpace != 0 || SourcePointer.AddressSpace != 0) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "memcpy requires default address space");
      return;
    }
    Statistics.PointerResolutions += 2;
    Statistics.PointerCandidates += DestPointer.Candidates.size() +
                                    SourcePointer.Candidates.size();
    if (DestPointer.Candidates.size() != 1 ||
        SourcePointer.Candidates.size() != 1 ||
        DestPointer.Candidates.front().Guard ||
        SourcePointer.Candidates.front().Guard) {
      incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                 "memcpy pointer candidates require further resolution");
      return;
    }
    const PointerCandidate &D = DestPointer.Candidates.front();
    const PointerCandidate &S = SourcePointer.Candidates.front();
    if (!D.Object || !S.Object) {
      const PointerCandidate &NullCandidate = !D.Object ? D : S;
      if (NullCandidate.Offset && NullCandidate.Offset->isConstant() &&
          NullCandidate.Offset->constantValue().isZero()) {
        if (Copy.getFunction()->hasFnAttribute(
                llvm::Attribute::NullPointerIsValid))
          incomplete(State, IncompletenessReason::PointerProvenanceLost,
                     "null address is valid in this function, but its memory is not modeled");
        else
          pathError(State, PathErrorReason::NullDereference,
                    "nonzero memcpy through null pointer");
      } else
        incomplete(State, IncompletenessReason::PointerProvenanceLost,
                   "null-derived memcpy address has no object provenance");
      return;
    }
    auto DestAt = concreteUnsigned(D.Offset);
    auto SourceAt = concreteUnsigned(S.Offset);
    if (!DestAt || !SourceAt) {
      incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                 "memcpy requires concrete object offsets");
      return;
    }
    const AddressSpace::Entry *DestEntry = State.addressSpace().find(D.Object->Id);
    const AddressSpace::Entry *SourceEntry = State.addressSpace().find(S.Object->Id);
    if (!DestEntry || !SourceEntry || DestEntry->Object.get() != D.Object.get() ||
        SourceEntry->Object.get() != S.Object.get()) {
      incomplete(State, IncompletenessReason::PointerProvenanceLost,
                 "memcpy pointer object is not bound");
      return;
    }
    if (!DestEntry->State->live() || !SourceEntry->State->live()) {
      pathError(State, (((D.Object->Kind == AllocationKind::Heap ||
                          D.Object->Kind == AllocationKind::New) &&
                         !DestEntry->State->live()) ||
                        ((S.Object->Kind == AllocationKind::Heap ||
                          S.Object->Kind == AllocationKind::New) &&
                         !SourceEntry->State->live()))
                           ? PathErrorReason::UseAfterFree
                           : PathErrorReason::DeadObjectAccess,
                "memcpy accesses a dead object",
                !DestEntry->State->live() ? D.Object->Id : S.Object->Id);
      return;
    }
    if (*DestAt > D.Object->Size || *Length > D.Object->Size - *DestAt ||
        *SourceAt > S.Object->Size || *Length > S.Object->Size - *SourceAt) {
      pathError(State, PathErrorReason::OutOfBounds,
                "memcpy range crosses object bounds");
      return;
    }
    const std::uint64_t DestAlign = Copy.getDestAlign().value_or(llvm::Align(1)).value();
    const std::uint64_t SourceAlign = Copy.getSourceAlign().value_or(llvm::Align(1)).value();
    if (D.Object->Alignment < DestAlign || S.Object->Alignment < SourceAlign) {
      incomplete(State, IncompletenessReason::AlignmentUndetermined,
                 "memcpy alignment exceeds object guarantee");
      return;
    }
    if (*DestAt % DestAlign != 0 || *SourceAt % SourceAlign != 0) {
      pathError(State, PathErrorReason::MisalignedAccess,
                "memcpy pointer violates declared alignment");
      return;
    }
    if (D.Object->ReadOnly) {
      pathError(State, PathErrorReason::ReadOnlyWrite,
                "memcpy writes a constant global object");
      return;
    }
    if (!IsMove && D.Object->Id == S.Object->Id &&
        *DestAt < *SourceAt + *Length &&
        *SourceAt < *DestAt + *Length) {
      pathError(State, PathErrorReason::OverlappingMemcpy,
                "overlapping memcpy has undefined behavior");
      return;
    }
    std::vector<ExprRef> Bytes;
    std::vector<ExprRef> KnownMasks;
    std::vector<ExprRef> AddressSensitiveBytes;
    Bytes.reserve(static_cast<std::size_t>(*Length));
    KnownMasks.reserve(static_cast<std::size_t>(*Length));
    AddressSensitiveBytes.reserve(static_cast<std::size_t>(*Length));
    for (std::uint64_t Byte = 0; Byte < *Length; ++Byte) {
      ExprRef Offset = Expressions.constant(SourcePointer.Width, *SourceAt + Byte);
      Bytes.push_back(SourceEntry->State->readByte(Offset, Expressions));
      KnownMasks.push_back(SourceEntry->State->knownMaskAt(Offset, Expressions));
      AddressSensitiveBytes.push_back(
          SourceEntry->State->addressSensitiveAt(Offset, Expressions));
      ++Statistics.ByteReadExprs;
    }
    std::vector<PointerShadowEntry> Shadows;
    for (const auto &[Start, Shadow] : SourceEntry->State->shadows()) {
      if (Start >= *SourceAt && Start - *SourceAt <= *Length &&
          Shadow.Size <= *Length - (Start - *SourceAt)) {
        PointerShadowEntry Copied = Shadow;
        Copied.Start = *DestAt + (Start - *SourceAt);
        Shadows.push_back(std::move(Copied));
      }
    }
    bool Copied = false;
    auto Writable = State.addressSpace().getWriteable(D.Object->Id, Copied);
    if (!Writable) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "memcpy destination disappeared");
      return;
    }
    if (Copied) {
      ++Statistics.COWCopies;
      ++Statistics.ObjectStates;
    }
    for (std::uint64_t Byte = 0; Byte < *Length; ++Byte) {
      Writable->writeByteWithMask(
          Expressions.constant(DestPointer.Width, *DestAt + Byte),
          Bytes[static_cast<std::size_t>(Byte)],
          KnownMasks[static_cast<std::size_t>(Byte)], Expressions, false,
          AddressSensitiveBytes[static_cast<std::size_t>(Byte)]);
      ++Statistics.ByteWriteExprs;
    }
    for (PointerShadowEntry &Shadow : Shadows)
      Writable->installShadow(std::move(Shadow));
    Statistics.MaxUpdateListLength = std::max<std::uint64_t>(
        Statistics.MaxUpdateListLength, Writable->bytes().size());
    if (Options.EnableRegistration)
      State.memoryReads().push_back(
          {S.Object->Id, Expressions.constant(SourcePointer.Width, *SourceAt),
           *Length});
    State.pc().advance();
  }

  void executeMemSet(llvm::MemSetInst &Set, ExecutionState &State) {
    if (llvm::isa<llvm::MemSetInlineInst>(Set)) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "llvm.memset.inline is outside the M6 intrinsic contract");
      return;
    }
    if (Set.isVolatile()) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "volatile memset is unsupported");
      return;
    }
    const llvm::Function *Callee = Set.getCalledFunction();
    if (!Callee) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "memset has no direct intrinsic declaration");
      return;
    }
    for (unsigned Arg = 0; Arg < Set.arg_size(); ++Arg) {
      for (llvm::Attribute Attr : Set.getAttributes().getParamAttrs(Arg))
        if (!supportedMemcpyParamAttribute(Arg, Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported memset parameter attribute");
          return;
        }
      for (llvm::Attribute Attr : Callee->getAttributes().getParamAttrs(Arg))
        if (!supportedMemcpyParamAttribute(Arg, Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported memset declaration parameter attribute");
          return;
        }
    }
    auto LengthValue = resolve(Set.getLength(), State);
    auto ByteValue = resolve(Set.getValue(), State);
    if (!LengthValue || !ByteValue)
      return;
    auto Length = LengthValue->isPointer()
                      ? std::optional<std::uint64_t>{}
                      : concreteUnsigned(LengthValue->bitVector());
    if (!Length || *Length > Options.MaxObjectBytes || ByteValue->isPointer() ||
        ByteValue->width() != 8) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "memset requires a bounded concrete count and an i8 value");
      return;
    }
    if (*Length == 0) {
      if (Set.getDestAlign() && Set.getDestAlign()->value() > 1 &&
          !llvm::isa<llvm::ConstantPointerNull>(Set.getRawDest())) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "zero-length memset alignment is unproved");
        return;
      }
      State.pc().advance();
      return;
    }
    auto Dest = resolve(Set.getRawDest(), State);
    if (!Dest)
      return;
    if (!Dest->isPointer() || Dest->pointer().AddressSpace != 0) {
      incomplete(State, IncompletenessReason::UnsupportedAddressSpace,
                 "memset requires a default-address-space pointer");
      return;
    }
    const PointerValue &Pointer = Dest->pointer();
    ++Statistics.PointerResolutions;
    Statistics.PointerCandidates += Pointer.Candidates.size();
    if (Pointer.Candidates.size() != 1 || Pointer.Candidates.front().Guard) {
      incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                 "memset pointer candidates require further resolution");
      return;
    }
    const PointerCandidate &Candidate = Pointer.Candidates.front();
    if (!Candidate.Object) {
      if (Candidate.Offset && Candidate.Offset->isConstant() &&
          Candidate.Offset->constantValue().isZero() &&
          !Set.getFunction()->hasFnAttribute(llvm::Attribute::NullPointerIsValid))
        pathError(State, PathErrorReason::NullDereference,
                  "nonzero memset through null pointer");
      else
        incomplete(State, IncompletenessReason::PointerProvenanceLost,
                   "memset destination has no object provenance");
      return;
    }
    auto At = concreteUnsigned(Candidate.Offset);
    if (!At) {
      incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                 "memset requires a concrete object offset");
      return;
    }
    const AddressSpace::Entry *Entry =
        State.addressSpace().find(Candidate.Object->Id);
    if (!Entry || Entry->Object.get() != Candidate.Object.get()) {
      incomplete(State, IncompletenessReason::PointerProvenanceLost,
                 "memset destination object is not bound");
      return;
    }
    if (!Entry->State->live()) {
      pathError(State, (Candidate.Object->Kind == AllocationKind::Heap ||
                        Candidate.Object->Kind == AllocationKind::New)
                           ? PathErrorReason::UseAfterFree
                           : PathErrorReason::DeadObjectAccess,
                "memset writes a dead object", Candidate.Object->Id);
      return;
    }
    if (*At > Candidate.Object->Size ||
        *Length > Candidate.Object->Size - *At) {
      pathError(State, PathErrorReason::OutOfBounds,
                "memset range crosses object bounds");
      return;
    }
    const std::uint64_t Align = Set.getDestAlign().value_or(llvm::Align(1)).value();
    if (Candidate.Object->Alignment < Align) {
      incomplete(State, IncompletenessReason::AlignmentUndetermined,
                 "memset alignment exceeds object guarantee");
      return;
    }
    if (*At % Align != 0) {
      pathError(State, PathErrorReason::MisalignedAccess,
                "memset pointer violates declared alignment");
      return;
    }
    if (Candidate.Object->ReadOnly) {
      pathError(State, PathErrorReason::ReadOnlyWrite,
                "memset writes a constant global object");
      return;
    }
    bool Copied = false;
    auto Writable = State.addressSpace().getWriteable(Candidate.Object->Id, Copied);
    if (!Writable) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "memset destination disappeared");
      return;
    }
    if (Copied) {
      ++Statistics.COWCopies;
      ++Statistics.ObjectStates;
    }
    for (std::uint64_t Index = 0; Index < *Length; ++Index) {
      Writable->writeByte(Expressions.constant(Pointer.Width, *At + Index),
                          ByteValue->bitVector(), Expressions);
      ++Statistics.ByteWriteExprs;
    }
    Statistics.MaxUpdateListLength = std::max<std::uint64_t>(
        Statistics.MaxUpdateListLength, Writable->bytes().size());
    State.pc().advance();
  }

  void executeBranch(llvm::BranchInst &Branch, ExecutionState &State) {
    if (Branch.isUnconditional()) {
      State.setDepth(State.depth() + 1);
      transfer(State, Branch.getSuccessor(0));
      return;
    }
    std::optional<SymbolicValue> Condition =
        resolve(Branch.getCondition(), State);
    if (!Condition)
      return;
    try {
      ExprRef TrueGuard = toBool(*Condition);
      ExprRef FalseGuard = Expressions.logicalNot(TrueGuard);
      forkToDestinations(
          State, {{TrueGuard, Branch.getSuccessor(0)},
                  {FalseGuard, Branch.getSuccessor(1)}});
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
    }
  }

  void executeSwitch(llvm::SwitchInst &Switch, ExecutionState &State) {
    std::optional<SymbolicValue> Condition =
        resolve(Switch.getCondition(), State);
    if (!Condition)
      return;
    try {
      std::vector<GuardedDestination> Destinations;
      Destinations.reserve(Switch.getNumCases() + 1);
      ExprRef DefaultGuard = Expressions.boolean(true);
      for (const auto &Case : Switch.cases()) {
        ExprRef CaseConstant =
            Expressions.constant(Case.getCaseValue()->getValue());
        ExprRef CaseGuard =
            Expressions.eq(Condition->bitVector(), CaseConstant);
        Destinations.push_back({CaseGuard, Case.getCaseSuccessor()});
        DefaultGuard = Expressions.select(CaseGuard,
                                          Expressions.boolean(false),
                                          DefaultGuard);
      }
      Destinations.push_back({DefaultGuard, Switch.getDefaultDest()});
      forkToDestinations(State, Destinations);
    } catch (const std::exception &Error) {
      internalError(State, InternalErrorReason::ExpressionConstructionFailure,
                    Error.what());
    }
  }

  void executeTerminationCall(llvm::CallInst &Call,
                              ExecutionState &State) {
    llvm::Function *Callee = Call.getCalledFunction();
    if (!Callee || !Callee->isDeclaration() ||
        !Callee->hasExternalLinkage() || Callee->isVarArg() ||
        Call.hasOperandBundles() ||
        Call.isNoBuiltin() ||
        Callee->getCallingConv() != llvm::CallingConv::C ||
        Call.getCallingConv() != llvm::CallingConv::C ||
        !Call.getType()->isVoidTy()) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "termination builtin has an unsupported call ABI");
      return;
    }
    const bool IsAssert = Callee->getName() == "__assert_fail";
    if ((IsAssert &&
         (Call.arg_size() != 4 ||
          !Call.getArgOperand(0)->getType()->isPointerTy() ||
          !Call.getArgOperand(1)->getType()->isPointerTy() ||
          !Call.getArgOperand(2)->getType()->isIntegerTy(32) ||
          !Call.getArgOperand(3)->getType()->isPointerTy())) ||
        (!IsAssert && Call.arg_size() != 0)) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "termination builtin signature is unsupported");
      return;
    }
    for (unsigned Index = 0; Index < Call.arg_size(); ++Index) {
      for (llvm::Attribute Attr : Call.getAttributes().getParamAttrs(Index))
        if (!Attr.hasKindAsEnum() ||
            Attr.getKindAsEnum() != llvm::Attribute::NoUndef) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported termination call parameter attribute");
          return;
        }
      for (llvm::Attribute Attr :
           Callee->getAttributes().getParamAttrs(Index))
        if (!Attr.hasKindAsEnum() ||
            Attr.getKindAsEnum() != llvm::Attribute::NoUndef) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported termination declaration parameter attribute");
          return;
        }
      if (!resolve(Call.getArgOperand(Index), State))
        return;
    }
    pathError(State, IsAssert ? PathErrorReason::AssertionFailed
                              : PathErrorReason::AbortCalled,
              IsAssert ? "__assert_fail was reached" : "abort was reached");
  }

  void executeLibcMemoryCall(llvm::CallInst &Call,
                             ExecutionState &State) {
    llvm::Function *Callee = Call.getCalledFunction();
    if (!Callee || !Callee->isDeclaration() ||
        !Callee->hasExternalLinkage() || Callee->isVarArg() ||
        Call.hasOperandBundles() ||
        Call.isNoBuiltin() ||
        Callee->getCallingConv() != llvm::CallingConv::C ||
        Call.getCallingConv() != llvm::CallingConv::C ||
        !Call.getType()->isPointerTy() || Call.arg_size() != 3) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "libc byte builtin has an unsupported call ABI");
      return;
    }
    const llvm::StringRef Name = Callee->getName();
    const bool IsSet = Name == "memset";
    const bool IsMove = Name == "memmove";
    const unsigned Width = layout(State).getPointerSizeInBits(0);
    if (!Call.getArgOperand(0)->getType()->isPointerTy() ||
        !(IsSet ? Call.getArgOperand(1)->getType()->isIntegerTy(32)
                : Call.getArgOperand(1)->getType()->isPointerTy()) ||
        !Call.getArgOperand(2)->getType()->isIntegerTy(Width)) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "libc byte builtin signature does not match target ABI");
      return;
    }
    auto AllowedParam = [&](unsigned Index, llvm::Attribute Attr) {
      if (!Attr.hasKindAsEnum())
        return false;
      switch (Attr.getKindAsEnum()) {
      case llvm::Attribute::NoUndef:
      case llvm::Attribute::NoCapture:
        return true;
      case llvm::Attribute::WriteOnly:
        return Index == 0;
      case llvm::Attribute::ReadOnly:
        return !IsSet && Index == 1;
      case llvm::Attribute::NoAlias:
        return !IsSet && !IsMove && Index < 2;
      default:
        return false;
      }
    };
    for (unsigned Index = 0; Index < Call.arg_size(); ++Index) {
      for (llvm::Attribute Attr : Call.getAttributes().getParamAttrs(Index))
        if (!AllowedParam(Index, Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported libc byte call parameter attribute");
          return;
        }
      for (llvm::Attribute Attr :
           Callee->getAttributes().getParamAttrs(Index))
        if (!AllowedParam(Index, Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported libc byte declaration parameter attribute");
          return;
        }
    }
    for (llvm::Attribute Attr : Call.getAttributes().getRetAttrs())
      if (!Attr.hasKindAsEnum() ||
          Attr.getKindAsEnum() != llvm::Attribute::NoUndef) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "unsupported libc byte call return attribute");
        return;
      }
    for (llvm::Attribute Attr : Callee->getAttributes().getRetAttrs())
      if (!Attr.hasKindAsEnum() ||
          Attr.getKindAsEnum() != llvm::Attribute::NoUndef) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "unsupported libc byte declaration return attribute");
        return;
      }
    auto LengthValue = resolve(Call.getArgOperand(2), State);
    auto DestValue = resolve(Call.getArgOperand(0), State);
    if (!LengthValue || !DestValue)
      return;
    auto Length = LengthValue->isPointer()
                      ? std::optional<std::uint64_t>{}
                      : concreteUnsigned(LengthValue->bitVector());
    if (!Length || *Length > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "libc byte builtin needs a bounded concrete length");
      return;
    }
    auto ConcretePointer = [&](const SymbolicValue &Value)
        -> std::optional<PointerCandidate> {
      if (!Value.isPointer() || Value.pointer().AddressSpace != 0 ||
          Value.pointer().Candidates.size() != 1)
        return std::nullopt;
      const PointerCandidate &Candidate = Value.pointer().Candidates.front();
      if (Candidate.Guard || !Candidate.Offset ||
          !Candidate.Offset->isConstant())
        return std::nullopt;
      return Candidate;
    };
    auto Dest = ConcretePointer(*DestValue);
    std::optional<SymbolicValue> SourceValue;
    std::optional<PointerCandidate> Source;
    std::optional<SymbolicValue> SetValue;
    if (IsSet) {
      SetValue = resolve(Call.getArgOperand(1), State);
      if (!SetValue)
        return;
    } else {
      SourceValue = resolve(Call.getArgOperand(1), State);
      if (!SourceValue)
        return;
      Source = ConcretePointer(*SourceValue);
    }
    if (!Dest || (!IsSet && !Source)) {
      incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                 "libc byte builtin needs concrete pointer candidates");
      return;
    }
    if (!Dest->Object || (!IsSet && !Source->Object)) {
      if (*Length == 0)
        incomplete(State, IncompletenessReason::PointerProvenanceLost,
                   "zero-length libc byte call has an unproven pointer");
      else
        pathError(State, PathErrorReason::NullDereference,
                  "libc byte builtin uses a null pointer");
      return;
    }
    auto DestAt = concreteUnsigned(Dest->Offset);
    auto SourceAt = Source ? concreteUnsigned(Source->Offset)
                           : std::optional<std::uint64_t>{};
    const auto *DestEntry = State.addressSpace().find(Dest->Object->Id);
    const auto *SourceEntry = Source
                                  ? State.addressSpace().find(Source->Object->Id)
                                  : nullptr;
    if (!DestAt || (Source && !SourceAt) || !DestEntry ||
        DestEntry->Object.get() != Dest->Object.get() ||
        (Source && (!SourceEntry ||
                    SourceEntry->Object.get() != Source->Object.get()))) {
      incomplete(State, IncompletenessReason::PointerProvenanceLost,
                 "libc byte builtin pointer has no live object identity");
      return;
    }
    const std::uint64_t SourceOffset =
        Source ? Source->Offset->constantValue().getZExtValue() : 0;
    if (!DestEntry->State->live() ||
        (SourceEntry && !SourceEntry->State->live())) {
      const bool HeapDead =
          ((Dest->Object->Kind == AllocationKind::Heap ||
            Dest->Object->Kind == AllocationKind::New) &&
           !DestEntry->State->live()) ||
          (Source && (Source->Object->Kind == AllocationKind::Heap ||
                      Source->Object->Kind == AllocationKind::New) &&
           !SourceEntry->State->live());
      pathError(State, HeapDead ? PathErrorReason::UseAfterFree
                                : PathErrorReason::DeadObjectAccess,
                "libc byte builtin accesses dead memory",
                !DestEntry->State->live() ? Dest->Object->Id
                                         : Source->Object->Id);
      return;
    }
    if (*DestAt > Dest->Object->Size ||
        *Length > Dest->Object->Size - *DestAt ||
        (Source && (SourceOffset > Source->Object->Size ||
                    *Length > Source->Object->Size - SourceOffset))) {
      pathError(State, PathErrorReason::OutOfBounds,
                "libc byte builtin crosses object bounds");
      return;
    }
    if (Dest->Object->ReadOnly) {
      pathError(State, PathErrorReason::ReadOnlyWrite,
                "libc byte builtin writes a constant object");
      return;
    }
    if (Source && !IsMove &&
        Dest->Object->Id == Source->Object->Id && *Length != 0 &&
        *DestAt < SourceOffset + *Length &&
        SourceOffset < *DestAt + *Length) {
      pathError(State, PathErrorReason::OverlappingMemcpy,
                "overlapping libc memcpy has undefined behavior");
      return;
    }
    std::vector<ExprRef> Bytes;
    std::vector<ExprRef> Masks;
    std::vector<ExprRef> Sensitive;
    std::vector<PointerShadowEntry> Shadows;
    ExprRef SetByte;
    if (IsSet) {
      if (SetValue->isPointer() || SetValue->isAggregate()) {
        incomplete(State, IncompletenessReason::UnsupportedType,
                   "libc memset fill value is not an integer");
        return;
      }
      SetByte = Expressions.trunc(SetValue->bitVector(), 8);
    } else {
      for (std::uint64_t Byte = 0; Byte < *Length; ++Byte) {
        ExprRef At = Expressions.constant(Width, SourceOffset + Byte);
        Bytes.push_back(SourceEntry->State->readByte(At, Expressions));
        Masks.push_back(SourceEntry->State->knownMaskAt(At, Expressions));
        Sensitive.push_back(
            SourceEntry->State->addressSensitiveAt(At, Expressions));
        ++Statistics.ByteReadExprs;
      }
      for (const auto &[Start, Shadow] : SourceEntry->State->shadows())
        if (Start >= SourceOffset && Start - SourceOffset <= *Length &&
            Shadow.Size <= *Length - (Start - SourceOffset)) {
          PointerShadowEntry Copy = Shadow;
          Copy.Start = *DestAt + Start - SourceOffset;
          Shadows.push_back(std::move(Copy));
        }
    }
    bool Copied = false;
    auto Writable = State.addressSpace().getWriteable(Dest->Object->Id, Copied);
    if (!Writable) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "libc byte destination disappeared");
      return;
    }
    if (Copied) {
      ++Statistics.COWCopies;
      ++Statistics.ObjectStates;
    }
    for (std::uint64_t Byte = 0; Byte < *Length; ++Byte) {
      ExprRef At = Expressions.constant(Width, *DestAt + Byte);
      if (IsSet)
        Writable->writeByte(At, SetByte, Expressions);
      else
        Writable->writeByteWithMask(At, Bytes[Byte], Masks[Byte], Expressions,
                                    false, Sensitive[Byte]);
      ++Statistics.ByteWriteExprs;
    }
    for (PointerShadowEntry &Shadow : Shadows)
      Writable->installShadow(std::move(Shadow));
    Statistics.MaxUpdateListLength = std::max<std::uint64_t>(
        Statistics.MaxUpdateListLength, Writable->bytes().size());
    if (Options.EnableRegistration && Source)
      State.memoryReads().push_back(
          {Source->Object->Id, Expressions.constant(Width, SourceOffset),
           *Length});
    bindResult(Call, State, std::move(*DestValue));
  }

  void executeHeapCall(llvm::CallInst &Call, ExecutionState &State) {
    llvm::Function *Callee = Call.getCalledFunction();
    if (!Callee || !Callee->isDeclaration() ||
        !Callee->hasExternalLinkage() ||
        Call.isNoBuiltin() ||
        Callee->getCallingConv() != llvm::CallingConv::C ||
        Call.getCallingConv() != llvm::CallingConv::C ||
        Callee->isVarArg() || Call.hasOperandBundles() ||
        Call.doesNotReturn() || Callee->doesNotReturn()) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "heap builtin has an unsupported declaration or ABI");
      return;
    }
    const llvm::StringRef Name = Callee->getName();
    const unsigned Width = layout(State).getPointerSizeInBits(0);
    llvm::StringRef TargetTriple =
        State.pc().function()->getParent()->getTargetTriple();
    if (!TargetTriple.starts_with("x86_64-") ||
        !TargetTriple.contains("linux")) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "heap ABI is modeled only for x86_64 Linux");
      return;
    }
    const bool IsFree = Name == "free";
    const bool IsDelete = Name == "_ZdlPv" || Name == "_ZdaPv" ||
                          Name == "_ZdlPvm" || Name == "_ZdaPvm";
    const bool IsSizedDelete = Name == "_ZdlPvm" || Name == "_ZdaPvm";
    const bool IsNew = Name == "_Znwm" || Name == "_Znam";
    const bool IsCalloc = Name == "calloc";
    const bool IsRealloc = Name == "realloc";
    const unsigned ExpectedArgs =
        IsSizedDelete || IsCalloc || IsRealloc ? 2 : 1;
    if (Call.arg_size() != ExpectedArgs ||
        (IsFree || IsDelete
             ? !Call.getType()->isVoidTy() ||
                      !Call.getArgOperand(0)->getType()->isPointerTy()
                : !Call.getType()->isPointerTy() ||
                      !(IsRealloc
                            ? Call.getArgOperand(0)->getType()->isPointerTy()
                            : Call.getArgOperand(0)->getType()->isIntegerTy(
                                  Width))) ||
        ((IsCalloc || IsSizedDelete) &&
         !Call.getArgOperand(1)->getType()->isIntegerTy(Width))) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "heap builtin signature does not match target size_t ABI");
      return;
    }
    auto ParamSupported = [](llvm::Attribute Attr) {
      return Attr.hasKindAsEnum() &&
             (Attr.getKindAsEnum() == llvm::Attribute::NoUndef ||
              Attr.getKindAsEnum() == llvm::Attribute::NoCapture);
    };
    for (unsigned Index = 0; Index < Call.arg_size(); ++Index) {
      for (llvm::Attribute Attr : Call.getAttributes().getParamAttrs(Index))
        if (!ParamSupported(Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported heap call parameter attribute");
          return;
        }
      for (llvm::Attribute Attr :
           Callee->getAttributes().getParamAttrs(Index))
        if (!ParamSupported(Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported heap declaration parameter attribute");
          return;
        }
    }
    auto ReturnSupported = [IsNew](llvm::Attribute Attr) {
      return Attr.hasKindAsEnum() &&
             (Attr.getKindAsEnum() == llvm::Attribute::NoUndef ||
              Attr.getKindAsEnum() == llvm::Attribute::NoAlias ||
              (IsNew && Attr.getKindAsEnum() == llvm::Attribute::NonNull));
    };
    for (llvm::Attribute Attr : Call.getAttributes().getRetAttrs())
      if (!ReturnSupported(Attr)) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "unsupported heap call return attribute");
        return;
      }
    for (llvm::Attribute Attr : Callee->getAttributes().getRetAttrs())
      if (!ReturnSupported(Attr)) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "unsupported heap declaration return attribute");
        return;
      }
    if (IsFree || IsDelete) {
      auto Value = resolve(Call.getArgOperand(0), State);
      if (!Value)
        return;
      if (!Value->isPointer() || Value->pointer().AddressSpace != 0 ||
          Value->pointer().Candidates.size() > Options.MaxPointerCandidates) {
        incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                   "free pointer candidates are unsupported");
        return;
      }
      std::optional<std::uint64_t> SizedDeleteBytes;
      if (IsSizedDelete) {
        auto SizeValue = resolve(Call.getArgOperand(1), State);
        if (!SizeValue)
          return;
        SizedDeleteBytes = SizeValue->isPointer()
                               ? std::optional<std::uint64_t>{}
                               : concreteUnsigned(SizeValue->bitVector());
        if (!SizedDeleteBytes) {
          incomplete(State, IncompletenessReason::UnsupportedCall,
                     "sized delete requires a concrete allocation size");
          return;
        }
      }
      const ExecutionState Base = State;
      bool First = true;
      for (const PointerCandidate &Candidate : Value->pointer().Candidates) {
        ExecutionState Child = Base;
        if (!First)
          Child.setId(allocateStateId());
        First = false;
        if (Statistics.StatesCreated > Options.MaxStates) {
          incomplete(Child, IncompletenessReason::StateLimit,
                     "state limit reached while resolving free pointer");
          continue;
        }
        if (Candidate.Guard) {
          QueryOutcome Outcome = resolutionQuery(Child, Candidate.Guard);
          if (Outcome.Disposition == QueryDisposition::Infeasible) {
            ++Statistics.PrunedPaths;
            continue;
          }
          if (Outcome.Disposition == QueryDisposition::Incomplete) {
            incomplete(Child, Outcome.Reason, Outcome.Detail);
            continue;
          }
          if (Outcome.Disposition == QueryDisposition::InternalError) {
            internalError(Child, InternalErrorReason::SolverInternalError,
                          Outcome.Detail);
            continue;
          }
          if (!addConstraint(Child, Candidate.Guard))
            continue;
        }
        if (!Candidate.Offset || !Candidate.Offset->isConstant()) {
          incomplete(Child, IncompletenessReason::PointerResolutionIncomplete,
                     "free requires a concrete base pointer offset");
          continue;
        }
        if (!Candidate.Object && Candidate.Offset->constantValue().isZero()) {
          Child.pc().advance();
          Worklist.schedule(std::move(Child));
          continue;
        }
        const auto *Entry = Candidate.Object
                                ? Child.addressSpace().find(Candidate.Object->Id)
                                : nullptr;
        if (!Entry || Entry->Object.get() != Candidate.Object.get() ||
            Candidate.Object->Kind !=
                (IsDelete ? AllocationKind::New : AllocationKind::Heap) ||
            !Candidate.Offset->constantValue().isZero()) {
          pathError(Child, PathErrorReason::InvalidFree,
                    "deallocation requires a matching allocation base or null",
                    Candidate.Object ? Candidate.Object->Id : 0);
          continue;
        }
        if (SizedDeleteBytes &&
            *SizedDeleteBytes != Candidate.Object->Size) {
          incomplete(Child, IncompletenessReason::UnsupportedCall,
                     "sized delete disagrees with allocation size");
          continue;
        }
        if (!Entry->State->live()) {
          pathError(Child, PathErrorReason::DoubleFree,
                    "heap allocation was already freed", Candidate.Object->Id);
          continue;
        }
        bool Copied = false;
        auto Writable = Child.addressSpace().getWriteable(Entry->Object->Id,
                                                          Copied);
        if (!Writable) {
          internalError(Child, InternalErrorReason::MemoryModelError,
                        "heap allocation disappeared during free");
          continue;
        }
        if (Copied) {
          ++Statistics.COWCopies;
          ++Statistics.ObjectStates;
        }
        Writable->endLifetime();
        recordEvent(Child, "free", Name.str(), Candidate.Object->Id);
        Child.pc().advance();
        Worklist.schedule(std::move(Child));
      }
      State.setStatus(ExecutionStateStatus::Scheduled);
      return;
    }
    if (IsRealloc) {
      if (!Call.getArgOperand(1)->getType()->isIntegerTy(Width)) {
        incomplete(State, IncompletenessReason::UnsupportedCall,
                   "realloc size argument does not match target size_t");
        return;
      }
      auto OldValue = resolve(Call.getArgOperand(0), State);
      auto NewSizeValue = resolve(Call.getArgOperand(1), State);
      if (!OldValue || !NewSizeValue)
        return;
      auto NewSize = NewSizeValue->isPointer()
                         ? std::optional<std::uint64_t>{}
                         : concreteUnsigned(NewSizeValue->bitVector());
      if (!NewSize || *NewSize == 0 || *NewSize > Options.MaxObjectBytes) {
        incomplete(State, IncompletenessReason::SymbolicAllocationSize,
                   "realloc needs a positive concrete modeled size");
        return;
      }
      if (!OldValue->isPointer() ||
          OldValue->pointer().Candidates.empty() ||
          OldValue->pointer().Candidates.size() >
              Options.MaxPointerCandidates ||
          OldValue->pointer().AddressSpace != 0) {
        incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                   "realloc pointer candidates exceed the supported bound");
        return;
      }
      if (OldValue->pointer().Candidates.size() > 1) {
        const ExecutionState Base = State;
        bool FirstCandidate = true;
        for (const PointerCandidate &Candidate :
             OldValue->pointer().Candidates) {
          ExecutionState Child = Base;
          if (!FirstCandidate) {
            Child.setId(allocateStateId());
            ++Statistics.Forks;
          }
          FirstCandidate = false;
          if (Statistics.StatesCreated > Options.MaxStates) {
            incomplete(Child, IncompletenessReason::StateLimit,
                       "state limit reached while resolving realloc pointer");
            continue;
          }
          if (Candidate.Guard) {
            QueryOutcome Outcome = resolutionQuery(Child, Candidate.Guard);
            if (Outcome.Disposition == QueryDisposition::Infeasible) {
              ++Statistics.PrunedPaths;
              continue;
            }
            if (Outcome.Disposition == QueryDisposition::Incomplete) {
              incomplete(Child, Outcome.Reason, Outcome.Detail);
              continue;
            }
            if (Outcome.Disposition == QueryDisposition::InternalError) {
              internalError(Child, InternalErrorReason::SolverInternalError,
                            Outcome.Detail);
              continue;
            }
            if (!addConstraint(Child, Candidate.Guard))
              continue;
          }
          PointerValue Selected = OldValue->pointer();
          Selected.Candidates = {Candidate};
          Selected.Candidates.front().Guard = nullptr;
          Child.frame().bind(Call.getArgOperand(0),
                             SymbolicValue(std::move(Selected)));
          executeHeapCall(Call, Child);
          if (Child.status() == ExecutionStateStatus::Running)
            Worklist.schedule(std::move(Child));
        }
        State.setStatus(ExecutionStateStatus::Scheduled);
        return;
      }
      const PointerCandidate &Old = OldValue->pointer().Candidates.front();
      if (Old.Guard &&
          (!Old.Guard->isConstant() || Old.Guard->constantValue().isZero())) {
        incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                   "realloc pointer candidate guard is ambiguous");
        return;
      }
      auto OldOffset = Old.Offset ? concreteUnsigned(Old.Offset)
                                  : std::optional<std::uint64_t>{};
      if (!OldOffset) {
        incomplete(State, IncompletenessReason::PointerResolutionIncomplete,
                   "realloc needs a concrete base pointer offset");
        return;
      }
      const AddressSpace::Entry *OldEntry =
          Old.Object ? State.addressSpace().find(Old.Object->Id) : nullptr;
      if ((Old.Object &&
           (!OldEntry || OldEntry->Object.get() != Old.Object.get() ||
            Old.Object->Kind != AllocationKind::Heap || *OldOffset != 0)) ||
          (!Old.Object && *OldOffset != 0)) {
        pathError(State, PathErrorReason::InvalidFree,
                  "realloc requires a heap allocation base or null",
                  Old.Object ? Old.Object->Id : 0);
        return;
      }
      if (OldEntry && !OldEntry->State->live()) {
        pathError(State, PathErrorReason::DoubleFree,
                  "realloc accesses an already freed allocation", Old.Object->Id);
        return;
      }
      if (Options.ModelAllocationFailure) {
        ExecutionState Failure = State;
        Failure.setId(allocateStateId());
        if (Statistics.StatesCreated > Options.MaxStates)
          incomplete(Failure, IncompletenessReason::StateLimit,
                     "state limit reached for realloc failure path");
        else {
          recordEvent(Failure, "realloc_failure", Name.str(), 0,
                      Old.Object ? Old.Object->Id : 0, *NewSize);
          bindResult(Call, Failure,
                     SymbolicValue(PointerValue::null(Width, Expressions)));
          Worklist.schedule(std::move(Failure));
          ++Statistics.Forks;
        }
      } else {
        assumeAllocationSuccess(State, Name);
      }
      auto NewObject = allocateObject(
          State, AllocationKind::Heap, *NewSize, 16,
          "realloc at " + locationOf(&Call).Function + ":" +
              std::to_string(locationOf(&Call).InstructionOrdinal),
          false);
      if (!NewObject)
        return;
      if (OldEntry) {
        const std::uint64_t CopyBytes =
            std::min(Old.Object->Size, *NewSize);
        bool Copied = false;
        auto Writable = State.addressSpace().getWriteable(NewObject->Id,
                                                          Copied);
        if (!Writable) {
          internalError(State, InternalErrorReason::MemoryModelError,
                        "realloc destination disappeared");
          return;
        }
        if (Copied) {
          ++Statistics.COWCopies;
          ++Statistics.ObjectStates;
        }
        for (std::uint64_t Byte = 0; Byte < CopyBytes; ++Byte) {
          ExprRef At = Expressions.constant(Width, Byte);
          Writable->writeByteWithMask(
              At, OldEntry->State->readByte(At, Expressions),
              OldEntry->State->knownMaskAt(At, Expressions), Expressions,
              false, OldEntry->State->addressSensitiveAt(At, Expressions));
          ++Statistics.ByteReadExprs;
          ++Statistics.ByteWriteExprs;
        }
        for (const auto &[Start, Shadow] : OldEntry->State->shadows())
          if (Start < CopyBytes && Shadow.Size <= CopyBytes - Start)
            Writable->installShadow(Shadow);
        Writable = {};
        bool OldCopied = false;
        auto OldWritable = State.addressSpace().getWriteable(Old.Object->Id,
                                                             OldCopied);
        if (!OldWritable) {
          internalError(State, InternalErrorReason::MemoryModelError,
                        "realloc source disappeared");
          return;
        }
        if (OldCopied) {
          ++Statistics.COWCopies;
          ++Statistics.ObjectStates;
        }
        OldWritable->endLifetime();
      }
      recordEvent(State, "realloc_success", Name.str(), NewObject->Id,
                  Old.Object ? Old.Object->Id : 0, *NewSize);
      bindResult(Call, State,
                 SymbolicValue(PointerValue::object(
                     NewObject, Expressions.constant(Width, 0),
                     Expressions)));
      return;
    }
    auto First = resolve(Call.getArgOperand(0), State);
    if (!First)
      return;
    std::optional<SymbolicValue> Second;
    if (IsCalloc) {
      Second = resolve(Call.getArgOperand(1), State);
      if (!Second)
        return;
    }
    auto Count = First->isPointer() ? std::optional<std::uint64_t>{}
                                    : concreteUnsigned(First->bitVector());
    auto ElementSize = Second && !Second->isPointer()
                           ? concreteUnsigned(Second->bitVector())
                           : std::optional<std::uint64_t>{};
    std::uint64_t Size = 0;
    if (!Count || (IsCalloc && !ElementSize)) {
      incomplete(State, IncompletenessReason::SymbolicAllocationSize,
                 "heap allocation needs concrete size without overflow");
      return;
    }
    const std::uint64_t CountValue =
        First->bitVector()->constantValue().getZExtValue();
    if (IsCalloc) {
      const std::uint64_t ElementValue =
          Second->bitVector()->constantValue().getZExtValue();
      if (!checkedMul(CountValue, ElementValue, Size)) {
        incomplete(State, IncompletenessReason::SymbolicAllocationSize,
                   "heap allocation needs concrete size without overflow");
        return;
      }
    }
    if (!IsCalloc)
      Size = CountValue;
    if (Size > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::AllocationLimit,
                 "heap allocation exceeds modeled object limit");
      return;
    }
    if (Options.ModelAllocationFailure || IsNew) {
      ExecutionState Failure = State;
      Failure.setId(allocateStateId());
      if (Statistics.StatesCreated > Options.MaxStates)
        incomplete(Failure, IncompletenessReason::StateLimit,
                   "state limit reached for allocation failure path");
      else if (IsNew) {
        incomplete(Failure,
                   IncompletenessReason::UnsupportedExceptionInstruction,
                   "throwing operator new failure requires a C++ exception or "
                   "termination model");
        ++Statistics.Forks;
      } else {
        recordEvent(Failure, "allocation_failure", Name.str(), 0, 0, Size);
        bindResult(Call, Failure,
                   SymbolicValue(PointerValue::null(Width, Expressions)));
        Worklist.schedule(std::move(Failure));
        ++Statistics.Forks;
      }
    } else {
      assumeAllocationSuccess(State, Name);
    }
    auto Object = allocateObject(State,
                                 IsNew ? AllocationKind::New
                                       : AllocationKind::Heap,
                                 Size, 16,
                                 Name.str() + " at " +
                                     locationOf(&Call).Function + ":" +
                                     std::to_string(
                                         locationOf(&Call).InstructionOrdinal),
                                 false);
    if (!Object)
      return;
    if (IsCalloc) {
      bool Copied = false;
      auto Writable = State.addressSpace().getWriteable(Object->Id, Copied);
      if (!Writable) {
        internalError(State, InternalErrorReason::MemoryModelError,
                      "calloc object disappeared");
        return;
      }
      if (Copied) {
        ++Statistics.COWCopies;
        ++Statistics.ObjectStates;
      }
      for (std::uint64_t Byte = 0; Byte < Size; ++Byte) {
        Writable->writeByte(Expressions.constant(Width, Byte),
                            Expressions.constant(8, 0), Expressions);
        ++Statistics.ByteWriteExprs;
      }
    }
    bindResult(Call, State,
               SymbolicValue(PointerValue::object(
                   Object, Expressions.constant(Width, 0), Expressions)));
  }

  void executeDefinedCall(llvm::CallInst &Call, ExecutionState &State) {
    std::string ResolutionFailure;
    const llvm::Function *Resolved =
        resolveExactFunctionAlias(Call.getCalledOperand(), ResolutionFailure);
    llvm::Function *Callee = const_cast<llvm::Function *>(Resolved);
    if (!Callee) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 std::move(ResolutionFailure));
      return;
    }
    if (Callee->isDeclaration()) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "unmodeled external call: " + Callee->getName().str());
      return;
    }
    if (!Callee->hasExactDefinition() || Callee->isInterposable()) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "direct callee lacks an exact, non-interposable definition: " +
                     Callee->getName().str());
      return;
    }
    if (Callee->isVarArg() || Call.arg_size() != Callee->arg_size() ||
        Call.hasOperandBundles() || Call.isMustTailCall() ||
        Call.getCallingConv() != llvm::CallingConv::C ||
        Callee->getCallingConv() != llvm::CallingConv::C) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "unsupported direct-call signature, calling convention, "
                 "operand bundle, or musttail ABI: " + Callee->getName().str());
      return;
    }
    if (Call.getMemoryEffects() != llvm::MemoryEffects::unknown() ||
        Callee->getMemoryEffects() != llvm::MemoryEffects::unknown()) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "defined-call memory effects require a transitive proof");
      return;
    }
    if (State.frames().size() >= Options.MaxCallDepth) {
      incomplete(State, IncompletenessReason::CallDepthLimit,
                 "direct-call depth bound reached at " +
                     Callee->getName().str());
      return;
    }
    auto SupportedAttribute = [](llvm::Attribute Attr) {
      return Attr.hasKindAsEnum() &&
             (Attr.getKindAsEnum() == llvm::Attribute::NoUndef ||
              Attr.getKindAsEnum() == llvm::Attribute::ZExt ||
              Attr.getKindAsEnum() == llvm::Attribute::SExt ||
              Attr.getKindAsEnum() == llvm::Attribute::Alignment ||
              Attr.getKindAsEnum() == llvm::Attribute::Dereferenceable ||
              Attr.getKindAsEnum() == llvm::Attribute::NonNull ||
              Attr.getKindAsEnum() == llvm::Attribute::NoAlias ||
              Attr.getKindAsEnum() == llvm::Attribute::ByVal ||
              Attr.getKindAsEnum() == llvm::Attribute::StructRet ||
              Attr.getKindAsEnum() == llvm::Attribute::ReadOnly ||
              Attr.getKindAsEnum() == llvm::Attribute::WriteOnly ||
              Attr.getKindAsEnum() == llvm::Attribute::Writable ||
              Attr.getKindAsEnum() == llvm::Attribute::DeadOnUnwind);
    };
    for (unsigned Index = 0; Index < Call.arg_size(); ++Index) {
      for (llvm::Attribute Attr : Call.getAttributes().getParamAttrs(Index))
        if (!SupportedAttribute(Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported call-site parameter ABI attribute at " +
                         Callee->getName().str());
          return;
        }
      for (llvm::Attribute Attr :
           Callee->getAttributes().getParamAttrs(Index))
        if (!SupportedAttribute(Attr)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported callee parameter ABI attribute at " +
                         Callee->getName().str());
          return;
        }
      for (llvm::Attribute::AttrKind Extension :
           {llvm::Attribute::ZExt, llvm::Attribute::SExt})
        if (Call.getAttributes().hasParamAttr(Index, Extension) !=
            Callee->hasParamAttribute(Index, Extension)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "call-site and callee integer argument extensions differ");
          return;
        }
    }
    auto SupportedReturnAttribute = [](llvm::Attribute Attr) {
      return Attr.hasKindAsEnum() &&
             (Attr.getKindAsEnum() == llvm::Attribute::NoUndef ||
              Attr.getKindAsEnum() == llvm::Attribute::ZExt ||
              Attr.getKindAsEnum() == llvm::Attribute::SExt);
    };
    for (llvm::Attribute Attr : Call.getAttributes().getRetAttrs())
      if (!SupportedReturnAttribute(Attr)) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "unsupported call-site return ABI attribute at " +
                       Callee->getName().str());
        return;
      }
    for (llvm::Attribute Attr : Callee->getAttributes().getRetAttrs())
      if (!SupportedReturnAttribute(Attr)) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "unsupported callee return ABI attribute at " +
                       Callee->getName().str());
        return;
      }
    for (llvm::Attribute::AttrKind Extension :
         {llvm::Attribute::ZExt, llvm::Attribute::SExt})
      if (Call.getAttributes().hasRetAttr(Extension) !=
          Callee->getAttributes().hasRetAttr(Extension)) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "call-site and callee integer return extensions differ");
        return;
      }
    if (Call.getType() != Callee->getReturnType() ||
        (!Call.getType()->isVoidTy() &&
         !Call.getType()->isIntegerTy() &&
         !Call.getType()->isPointerTy() &&
         !Call.getType()->isAggregateType())) {
      incomplete(State, IncompletenessReason::UnsupportedType,
                 "direct-call return type is unsupported");
      return;
    }
    std::vector<SymbolicValue> Values;
    Values.reserve(Call.arg_size());
    for (unsigned Index = 0; Index < Call.arg_size(); ++Index) {
      llvm::Type *Type = Callee->getArg(Index)->getType();
      if (Type != Call.getArgOperand(Index)->getType() ||
          (!Type->isIntegerTy() && !Type->isPointerTy() &&
           !Type->isAggregateType())) {
        incomplete(State, IncompletenessReason::UnsupportedType,
                   "direct-call argument type is unsupported");
        return;
      }
      auto Value = resolve(Call.getArgOperand(Index), State);
      if (!Value)
        return;
      if ((Call.paramHasAttr(Index, llvm::Attribute::NoUndef) ||
           Callee->hasParamAttribute(Index, llvm::Attribute::NoUndef)) &&
          Value->isAggregate() &&
          !requireNoundefAggregate(Value->aggregate(), Type, State,
                                   "argument"))
        return;
      Values.push_back(std::move(*Value));
    }
    StackFrame Frame(Callee);
    Frame.setId(NextFrameId++);
    Frame.setCallSite(&Call);
    Frame.setContinuation(ProgramCounter(Call.getFunction(), Call.getParent(),
                                         Call.getNextNode()));
    auto HasParam = [&](unsigned Index, llvm::Attribute::AttrKind Kind) {
      return Call.paramHasAttr(Index, Kind) ||
             Callee->hasParamAttribute(Index, Kind);
    };
    for (unsigned Index = 0; Index < Values.size(); ++Index) {
      llvm::Type *ByValType = Call.getParamByValType(Index);
      llvm::Type *SRetType = Call.getParamStructRetType(Index);
      if (ByValType && SRetType) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "parameter cannot have both byval and sret");
        return;
      }
      const bool HasPointerContract =
          ByValType || SRetType ||
          HasParam(Index, llvm::Attribute::NonNull) ||
          HasParam(Index, llvm::Attribute::NoAlias) ||
          HasParam(Index, llvm::Attribute::Dereferenceable) ||
          HasParam(Index, llvm::Attribute::Alignment) ||
          HasParam(Index, llvm::Attribute::Writable) ||
          HasParam(Index, llvm::Attribute::DeadOnUnwind);
      if (!HasPointerContract &&
          !HasParam(Index, llvm::Attribute::ReadOnly) &&
          !HasParam(Index, llvm::Attribute::WriteOnly))
        continue;
      if (!Values[Index].isPointer() ||
          Values[Index].pointer().AddressSpace != 0 ||
          Values[Index].pointer().Candidates.size() != 1) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "ABI pointer precondition needs one default-space object");
        return;
      }
      const PointerCandidate &Candidate =
          Values[Index].pointer().Candidates.front();
      if (Candidate.Guard &&
          (!Candidate.Guard->isConstant() ||
           Candidate.Guard->constantValue().isZero())) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "ABI pointer precondition has a conditional candidate");
        return;
      }
      auto Offset = Candidate.Offset
                        ? concreteUnsigned(Candidate.Offset)
                        : std::optional<std::uint64_t>{};
      const auto *Entry = Candidate.Object
                              ? State.addressSpace().find(Candidate.Object->Id)
                              : nullptr;
      if (!Candidate.Object || !Offset || !Entry ||
          Entry->Object.get() != Candidate.Object.get() ||
          !Entry->State->live()) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "ABI pointer precondition is not proven live and nonnull");
        return;
      }
      const std::uint64_t ConcreteOffset = Offset.value_or(0);
      if ((HasParam(Index, llvm::Attribute::Writable) &&
           Entry->Object->ReadOnly) ||
          (HasParam(Index, llvm::Attribute::DeadOnUnwind) &&
           !Callee->doesNotThrow())) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "writable or dead_on_unwind promise is not established");
        return;
      }
      llvm::Type *Pointee = ByValType ? ByValType : SRetType;
      std::uint64_t RequiredBytes = std::max(
          Call.getAttributes().getParamDereferenceableBytes(Index),
          Callee->getParamDereferenceableBytes(Index));
      std::uint64_t RequiredAlign = 1;
      if (auto Align = Call.getAttributes().getParamAlignment(Index))
        RequiredAlign = std::max(RequiredAlign, Align->value());
      if (auto Align = Callee->getParamAlign(Index))
        RequiredAlign = std::max(RequiredAlign, Align->value());
      if (Pointee) {
        if (!Pointee->isSized()) {
          incomplete(State, IncompletenessReason::UnsupportedType,
                     "byval/sret has an unsized pointee type");
          return;
        }
        auto Bytes = fixedSize(layout(State).getTypeAllocSize(Pointee));
        if (!Bytes || *Bytes > Options.MaxObjectBytes) {
          incomplete(State, IncompletenessReason::AllocationLimit,
                     "byval/sret pointee exceeds modeled object limit");
          return;
        }
        RequiredBytes = std::max(RequiredBytes, *Bytes);
        RequiredAlign = std::max(
            RequiredAlign, layout(State).getABITypeAlign(Pointee).value());
      }
      if (ConcreteOffset > Entry->Object->Size ||
          RequiredBytes > Entry->Object->Size - ConcreteOffset ||
          Entry->Object->Alignment < RequiredAlign ||
          ConcreteOffset % RequiredAlign != 0) {
        incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                   "ABI pointer range or alignment is not proven");
        return;
      }
      if (HasParam(Index, llvm::Attribute::NoAlias) && !ByValType) {
        if (!SRetType ||
            std::find(State.frame().stackObjects().begin(),
                      State.frame().stackObjects().end(),
                      Candidate.Object->Id) ==
                State.frame().stackObjects().end()) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "noalias requires a fresh caller stack sret object");
          return;
        }
        for (unsigned Other = 0; Other < Values.size(); ++Other) {
          if (Other == Index || !Values[Other].isPointer())
            continue;
          for (const PointerCandidate &OtherCandidate :
               Values[Other].pointer().Candidates)
            if (OtherCandidate.Object == Candidate.Object) {
              incomplete(State,
                         IncompletenessReason::PoisonGeneratingAnnotation,
                         "sret noalias conflicts with another pointer argument");
              return;
            }
        }
        for (const auto &[ObjectId, ObjectEntry] :
             State.addressSpace().entries()) {
          (void)ObjectId;
          for (const auto &[Start, Shadow] : ObjectEntry.State->shadows()) {
            (void)Start;
            for (const PointerCandidate &Stored : Shadow.Pointer.Candidates)
              if (Stored.Object == Candidate.Object) {
                incomplete(State,
                           IncompletenessReason::PoisonGeneratingAnnotation,
                           "sret noalias pointer escapes through memory");
                return;
              }
          }
        }
      }
      if (HasParam(Index, llvm::Attribute::ReadOnly) ||
          HasParam(Index, llvm::Attribute::WriteOnly)) {
        const bool ReadOnly = HasParam(Index, llvm::Attribute::ReadOnly);
        for (const llvm::BasicBlock &Block : *Callee)
          for (const llvm::Instruction &Instruction : Block)
            if (ReadOnly ? Instruction.mayWriteToMemory()
                         : Instruction.mayReadFromMemory()) {
              incomplete(State,
                         IncompletenessReason::PoisonGeneratingAnnotation,
                         "readonly/writeonly effect is not statically proven");
              return;
            }
      }
      if (ByValType) {
        const std::uint64_t Bytes =
            *fixedSize(layout(State).getTypeAllocSize(ByValType));
        auto Copy = allocateObject(State, AllocationKind::Stack, Bytes,
                                   RequiredAlign, "byval at " +
                                       Callee->getName().str(), false, false,
                                   false, Frame.id());
        if (!Copy)
          return;
        Frame.addStackObject(Copy->Id);
        bool Copied = false;
        auto Writable = State.addressSpace().getWriteable(Copy->Id, Copied);
        if (!Writable) {
          internalError(State, InternalErrorReason::MemoryModelError,
                        "byval copy object disappeared");
          return;
        }
        if (Copied) {
          ++Statistics.COWCopies;
          ++Statistics.ObjectStates;
        }
        for (std::uint64_t Byte = 0; Byte < Bytes; ++Byte) {
          ExprRef SourceAt = Expressions.constant(
              Values[Index].width(), ConcreteOffset + Byte);
          ExprRef DestAt = Expressions.constant(Values[Index].width(), Byte);
          Writable->writeByteWithMask(
              DestAt, Entry->State->readByte(SourceAt, Expressions),
              Entry->State->knownMaskAt(SourceAt, Expressions), Expressions,
              false, Entry->State->addressSensitiveAt(SourceAt, Expressions));
          ++Statistics.ByteReadExprs;
          ++Statistics.ByteWriteExprs;
        }
        for (const auto &[Start, Shadow] : Entry->State->shadows()) {
          if (Start < ConcreteOffset || Start - ConcreteOffset >= Bytes ||
              Shadow.Size > Bytes - (Start - ConcreteOffset))
            continue;
          PointerShadowEntry NewShadow = Shadow;
          NewShadow.Start -= ConcreteOffset;
          Writable->installShadow(std::move(NewShadow));
        }
        Values[Index] = SymbolicValue(PointerValue::object(
            Copy, Expressions.constant(Values[Index].width(), 0),
            Expressions));
      }
    }
    for (unsigned Index = 0; Index < Values.size(); ++Index)
      Frame.bind(Callee->getArg(Index), std::move(Values[Index]));
    Frame.markVisited(&Callee->getEntryBlock());
    State.frames().push_back(std::move(Frame));
    Statistics.MaximumCallDepthObserved =
        std::max(Statistics.MaximumCallDepthObserved, State.frames().size());
    recordEvent(State, "call_enter", Callee->getName().str());
    llvm::BasicBlock &Entry = Callee->getEntryBlock();
    State.pc().set(Callee, &Entry, &Entry.front());
  }

  void executeReturn(llvm::ReturnInst &Return, ExecutionState &State) {
    if (State.frame().function()->doesNotReturn() ||
        (State.frame().callSite() &&
         llvm::cast<llvm::CallInst>(State.frame().callSite())
             ->doesNotReturn())) {
      pathError(State, PathErrorReason::NoreturnViolation,
                "noreturn function or call-site returned normally");
      return;
    }
    std::optional<SymbolicValue> Value;
    if (Return.getNumOperands() != 0) {
      Value = resolve(Return.getReturnValue(), State);
      if (!Value)
        return;
    }
    if (Value && Value->isAggregate() &&
        (State.frame().function()->getAttributes().hasRetAttr(
             llvm::Attribute::NoUndef) ||
         (State.frame().callSite() &&
          llvm::cast<llvm::CallInst>(State.frame().callSite())
              ->getAttributes()
              .hasRetAttr(llvm::Attribute::NoUndef))) &&
        !requireNoundefAggregate(Value->aggregate(),
                                 Return.getFunction()->getReturnType(), State,
                                 "return"))
      return;
    if (Options.EnableRegistration && State.frames().size() == 1) {
      for (auto &Region : State.regions()) {
        if (Region.Input)
          continue;
        const auto *Entry = State.addressSpace().find(Region.ObjectId);
        if (!Entry || !Entry->State->live()) {
          incomplete(State, IncompletenessReason::DeadRegisteredOutput,
                     "registered output object is dead at capture");
          return;
        }
        for (std::uint64_t Index = 0; Index < Region.Size; ++Index) {
          ExprRef Offset = Expressions.constant(
              layout(State).getPointerSizeInBits(0), Region.Offset + Index);
          if (!requireMemoryGuard(
                  State, Entry->State->initializedAt(Offset, Expressions),
                  PathErrorReason::None,
                  IncompletenessReason::UninitializedRead,
                  "registered output contains an uninitialized byte"))
            return;
          if (!requireMemoryGuard(
                  State,
                  Expressions.logicalNot(
                      Entry->State->addressSensitiveAt(Offset, Expressions)),
                  PathErrorReason::None,
                  IncompletenessReason::PointerProvenanceLost,
                  "registered output contains target-dependent pointer bytes"))
            return;
          Region.Bytes.push_back(Entry->State->readByte(Offset, Expressions));
          ++Statistics.ByteReadExprs;
        }
      }
    }
    for (std::uint64_t Id : State.frame().stackObjects()) {
      bool Copied = false;
      auto Object = State.addressSpace().getWriteable(Id, Copied);
      if (!Object) {
        internalError(State, InternalErrorReason::MemoryModelError,
                      "stack object missing at return");
        return;
      }
      if (Copied) {
        ++Statistics.COWCopies;
        ++Statistics.ObjectStates;
      }
      Object->endLifetime();
      recordEvent(State, "lifetime_end", {}, Id);
    }
    if (State.frames().size() == 1) {
      completed(State, std::move(Value));
      return;
    }
    auto *Call = llvm::cast<llvm::CallInst>(State.frame().callSite());
    ProgramCounter Continuation = State.frame().continuation();
    recordEvent(State, "call_return", State.frame().function()->getName().str());
    State.frames().pop_back();
    State.pc().set(Call->getFunction(), Call->getParent(), Call);
    if (Value && !bindResult(*Call, State, std::move(*Value)))
      return;
    State.pc() = Continuation;
  }

  void executeRegistrationCall(llvm::CallInst &Call, ExecutionState &State) {
    const llvm::Function *Callee = Call.getCalledFunction();
    if (!Callee) {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "indirect call is not a registration builtin");
      return;
    }
    if (!Callee->isDeclaration() || !Callee->hasExternalLinkage() ||
        Callee->getCallingConv() != llvm::CallingConv::C ||
        Call.getCallingConv() != llvm::CallingConv::C) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "builtin must be an external C declaration and C call");
      return;
    }
    if (Call.doesNotReturn() || Callee->doesNotReturn()) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "noreturn registration builtin cannot be modeled as a "
                 "continuing call");
      return;
    }
    if (Call.getMemoryEffects() != llvm::MemoryEffects::unknown()) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "registration builtin requires unrestricted memory effects");
      return;
    }
    for (unsigned Arg = 0; Arg < Call.arg_size(); ++Arg) {
      auto Supported = [](llvm::Attribute Attribute) {
        return Attribute.hasKindAsEnum() &&
               (Attribute.getKindAsEnum() == llvm::Attribute::NoUndef ||
                Attribute.getKindAsEnum() == llvm::Attribute::NoCapture);
      };
      for (llvm::Attribute Attribute : Call.getAttributes().getParamAttrs(Arg))
        if (!Supported(Attribute)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported builtin call parameter attribute");
          return;
        }
      for (llvm::Attribute Attribute :
           Callee->getAttributes().getParamAttrs(Arg))
        if (!Supported(Attribute)) {
          incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                     "unsupported builtin declaration parameter attribute");
          return;
        }
    }
    const llvm::StringRef Name = Callee->getName();
    if (Name == "__formal_assume") {
      if (Call.arg_size() != 1 || !Call.getType()->isVoidTy() ||
          !Call.getArgOperand(0)->getType()->isIntegerTy(32)) {
        incomplete(State, IncompletenessReason::RegistrationUnsupported,
                   "__formal_assume requires the replayable void (i32) ABI");
        return;
      }
      auto Condition = resolve(Call.getArgOperand(0), State);
      if (!Condition)
        return;
      if (Condition->isPointer()) {
        incomplete(State, IncompletenessReason::RegistrationUnsupported,
                   "assume condition is not an integer");
        return;
      }
      ExprRef Nonzero = Expressions.logicalNot(Expressions.eq(
          Condition->bitVector(), Expressions.constant(Condition->width(), 0)));
      QueryOutcome Outcome = query(State, Nonzero);
      if (Outcome.Disposition == QueryDisposition::Infeasible) {
        ++Statistics.PrunedPaths;
        ++Statistics.AssumptionPrunedPaths;
        State.setStatus(ExecutionStateStatus::Scheduled);
        return;
      }
      if (Outcome.Disposition == QueryDisposition::Incomplete) {
        incomplete(State, Outcome.Reason, Outcome.Detail);
        return;
      }
      if (Outcome.Disposition == QueryDisposition::InternalError) {
        internalError(State, InternalErrorReason::SolverInternalError,
                      Outcome.Detail);
        return;
      }
      if (addConstraint(State, Nonzero))
        State.pc().advance();
      return;
    }

    const bool IsInput = Name == "__formal_register_input";
    if (!IsInput && Name != "__formal_register_output") {
      incomplete(State, IncompletenessReason::UnsupportedCall,
                 "ordinary calls require M7 semantics");
      return;
    }
    const unsigned PointerWidth = layout(State).getPointerSizeInBits(0);
    if (Call.arg_size() != 3 || !Call.getType()->isVoidTy() ||
        !Call.getArgOperand(0)->getType()->isPointerTy() ||
        !Call.getArgOperand(1)->getType()->isIntegerTy(PointerWidth) ||
        !Call.getArgOperand(2)->getType()->isPointerTy()) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "invalid registration builtin signature");
      return;
    }
    llvm::StringRef RawName;
    const llvm::Value *NameStorage = Call.getArgOperand(2)->stripPointerCasts();
    while (const auto *GEP = llvm::dyn_cast<llvm::GEPOperator>(NameStorage))
      NameStorage = GEP->getPointerOperand()->stripPointerCasts();
    const auto *NameGlobal = llvm::dyn_cast<llvm::GlobalVariable>(NameStorage);
    if (!NameGlobal || !NameGlobal->isConstant() ||
        NameGlobal->isExternallyInitialized() ||
        !llvm::getConstantStringInfo(Call.getArgOperand(2), RawName) ||
        RawName.empty() || RawName.size() > 128 ||
        !std::all_of(RawName.begin(), RawName.end(), [](unsigned char C) {
          return (C >= 'A' && C <= 'Z') || (C >= 'a' && C <= 'z') ||
                 (C >= '0' && C <= '9') || C == '_' || C == '.' || C == '-';
        })) {
      incomplete(State, IncompletenessReason::RegistrationName,
                 "registration name must be a constant ASCII identifier");
      return;
    }
    auto SizeValue = resolve(Call.getArgOperand(1), State);
    if (!SizeValue)
      return;
    auto Size = concreteUnsigned(SizeValue->bitVector());
    if (!Size) {
      incomplete(State, IncompletenessReason::SymbolicRegistrationSize,
                 "symbolic registration size requires a bounded-region model");
      return;
    }
    if (*Size == 0 || *Size > Options.MaxObjectBytes) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "zero or oversized registration is unsupported");
      return;
    }
    auto Pointer = resolve(Call.getArgOperand(0), State);
    if (!Pointer)
      return;
    if (!Pointer->isPointer() || Pointer->pointer().AddressSpace != 0 ||
        Pointer->pointer().Candidates.size() != 1) {
      incomplete(
          State, IncompletenessReason::RegistrationUnsupported,
          "registration pointer needs one resolved default-space object");
      return;
    }
    const PointerCandidate &Candidate = Pointer->pointer().Candidates.front();
    if (Candidate.Guard && (!Candidate.Guard->isConstant() ||
                            Candidate.Guard->constantValue().isZero())) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "conditional registration pointer needs path enumeration");
      return;
    }
    if (!Candidate.Object || !Candidate.Offset) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "registration pointer has no object provenance");
      return;
    }
    auto Offset = concreteUnsigned(Candidate.Offset);
    const auto *Entry = State.addressSpace().find(Candidate.Object->Id);
    if (!Offset || !Entry || Entry->Object.get() != Candidate.Object.get() ||
        !Entry->State->live() || *Size > Entry->Object->Size ||
        *Offset > Entry->Object->Size - *Size ||
        (IsInput && Entry->Object->ReadOnly)) {
      incomplete(State, IncompletenessReason::RegistrationUnsupported,
                 "registration region is not a live writable object slice");
      return;
    }
    if (IsInput) {
      const llvm::Value *Storage = Call.getArgOperand(0)->stripPointerCasts();
      while (const auto *GEP = llvm::dyn_cast<llvm::GEPOperator>(Storage))
        Storage = GEP->getPointerOperand()->stripPointerCasts();
      llvm::Type *StoredType = nullptr;
      if (const auto *Alloca = llvm::dyn_cast<llvm::AllocaInst>(Storage))
        StoredType = Alloca->getAllocatedType();
      else if (const auto *Global =
                   llvm::dyn_cast<llvm::GlobalVariable>(Storage))
        StoredType = Global->getValueType();
      if (StoredType) {
        const llvm::DataLayout &DL = layout(State);
        const auto Stride = StoredType->isSized()
                                ? fixedSize(DL.getTypeAllocSize(StoredType))
                                : std::nullopt;
        if (!Stride || *Stride == 0 ||
            Entry->Object->Size % *Stride != 0) {
          incomplete(State, IncompletenessReason::RegistrationUnsupported,
                     "input object has no fixed pointer-field layout");
          return;
        }
        const std::uint64_t Begin = *Offset;
        const std::uint64_t End = Begin + *Size;
        std::function<bool(llvm::Type *, std::uint64_t, unsigned)>
            OverlapsPointer = [&](llvm::Type *Type, std::uint64_t At,
                                  unsigned Depth) -> bool {
          if (Depth > 64 || !Type->isSized())
            return true;
          auto TypeSize = fixedSize(DL.getTypeAllocSize(Type));
          std::uint64_t TypeEnd = 0;
          if (!TypeSize || !checkedAdd(At, *TypeSize, TypeEnd))
            return true;
          if (At >= End || TypeEnd <= Begin)
            return false;
          if (Type->isPointerTy()) {
            auto PointerSize = fixedSize(DL.getTypeStoreSize(Type));
            std::uint64_t PointerEnd = 0;
            return !PointerSize || !checkedAdd(At, *PointerSize, PointerEnd) ||
                   (At < End && Begin < PointerEnd);
          }
          if (auto *Struct = llvm::dyn_cast<llvm::StructType>(Type)) {
            if (Struct->isOpaque())
              return true;
            const llvm::StructLayout *Fields = DL.getStructLayout(Struct);
            for (unsigned Index = 0; Index < Struct->getNumElements(); ++Index) {
              std::uint64_t FieldAt = 0;
              if (!checkedAdd(At, Fields->getElementOffset(Index), FieldAt) ||
                  OverlapsPointer(Struct->getElementType(Index), FieldAt,
                                  Depth + 1))
                return true;
            }
            return false;
          }
          if (const auto *Array = llvm::dyn_cast<llvm::ArrayType>(Type)) {
            if (Array->getNumElements() == 0)
              return false;
            auto ElementSize =
                fixedSize(DL.getTypeAllocSize(Array->getElementType()));
            if (!ElementSize || *ElementSize == 0)
              return !ElementSize;
            const std::uint64_t First =
                Begin > At ? (Begin - At) / *ElementSize : 0;
            const std::uint64_t Last = std::min(
                Array->getNumElements() - 1, (End - 1 - At) / *ElementSize);
            for (std::uint64_t Index = First;
                 Index < Array->getNumElements() && Index <= Last; ++Index) {
              std::uint64_t Delta = 0;
              std::uint64_t ElementAt = 0;
              if (!checkedMul(Index, *ElementSize, Delta) ||
                  !checkedAdd(At, Delta, ElementAt) ||
                  OverlapsPointer(Array->getElementType(), ElementAt,
                                  Depth + 1))
                return true;
            }
            return false;
          }
          return !Type->isIntegerTy() && !Type->isFloatingPointTy();
        };
        for (std::uint64_t Base = Begin / *Stride * *Stride; Base < End;
             Base += *Stride) {
          if (OverlapsPointer(StoredType, Base, 0)) {
            incomplete(State, IncompletenessReason::RegistrationUnsupported,
                       "input bytes overlap a pointer field");
            return;
          }
        }
      }
    }
    for (const auto &Earlier : State.regions()) {
      if (Earlier.Input != IsInput)
        continue;
      if (Earlier.Name == RawName) {
        incomplete(State, IncompletenessReason::RegistrationName,
                   "duplicate registration name for the same kind");
        return;
      }
      const bool Overlaps = *Offset <= Earlier.Offset
                                ? Earlier.Offset - *Offset < *Size
                                : *Offset - Earlier.Offset < Earlier.Size;
      if (Earlier.ObjectId == Candidate.Object->Id && Overlaps) {
        incomplete(State, IncompletenessReason::RegistrationOverlap,
                   "overlapping registrations of the same kind");
        return;
      }
    }
    if (IsInput) {
      for (const auto &Read : State.memoryReads()) {
        if (Read.ObjectId != Candidate.Object->Id)
          continue;
        auto ReadOffset = concreteUnsigned(Read.Offset);
        const bool Overlaps =
            ReadOffset &&
            (*Offset <= *ReadOffset ? *ReadOffset - *Offset < *Size
                                    : *Offset - *ReadOffset < Read.Size);
        if (!ReadOffset || Overlaps) {
          incomplete(State, IncompletenessReason::UseBeforeInput,
                     "registered input bytes were read before registration");
          return;
        }
      }
    }
    PathResult::RegisteredRegion Region;
    Region.Input = IsInput;
    Region.Name = RawName.str();
    Region.Size = *Size;
    Region.ObjectId = Candidate.Object->Id;
    Region.Offset = *Offset;
    Region.RegistrationLocation = locationOf(&Call);
    Region.RegistrationGuards = State.constraints().constraints();
    Region.CandidateGuard = Candidate.Guard;
    if (IsInput) {
      bool Copied = false;
      auto Writable =
          State.addressSpace().getWriteable(Region.ObjectId, Copied);
      if (!Writable) {
        internalError(State, InternalErrorReason::MemoryModelError,
                      "registered input object disappeared");
        return;
      }
      if (Copied) {
        ++Statistics.COWCopies;
        ++Statistics.ObjectStates;
      }
      for (std::uint64_t Index = 0; Index < *Size; ++Index) {
        ExprRef Byte = Expressions.symbol(8, "input." + Region.Name + "." +
                                                 std::to_string(Index));
        ExprRef At = Expressions.constant(PointerWidth, *Offset + Index);
        Writable->writeByte(At, Byte, Expressions);
        Region.Bytes.push_back(Byte);
        ++Statistics.ByteWriteExprs;
      }
    }
    State.regions().push_back(std::move(Region));
    State.pc().advance();
  }

  void executeStackLifetime(llvm::IntrinsicInst &Call,
                            ExecutionState &State) {
    const bool IsStart =
        Call.getIntrinsicID() == llvm::Intrinsic::lifetime_start;
    if (Call.arg_size() != 2 ||
        !Call.getArgOperand(0)->getType()->isIntegerTy(64) ||
        !Call.getArgOperand(1)->getType()->isPointerTy()) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "unsupported stack lifetime intrinsic signature");
      return;
    }
    auto SizeValue = resolve(Call.getArgOperand(0), State);
    auto Pointer = resolve(Call.getArgOperand(1), State);
    if (!SizeValue || !Pointer)
      return;
    auto Size = concreteUnsigned(SizeValue->bitVector());
    if (Size && *Size == 0) {
      State.pc().advance();
      return;
    }
    if (!Size || !Pointer->isPointer() ||
        Pointer->pointer().Candidates.size() != 1) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "stack lifetime needs one concrete full-object region");
      return;
    }
    const PointerCandidate &Candidate = Pointer->pointer().Candidates.front();
    if (!Candidate.Object || !Candidate.Offset ||
        (Candidate.Guard && (!Candidate.Guard->isConstant() ||
                             Candidate.Guard->constantValue().isZero())) ||
        !Candidate.Offset->isConstant() ||
        !Candidate.Offset->constantValue().isZero() ||
        Candidate.Object->Kind != AllocationKind::Stack ||
        Candidate.Object->OwnerFrameId != State.frame().id() ||
        (*Size != Candidate.Object->Size &&
         *Size != std::numeric_limits<std::uint64_t>::max())) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "partial or ambiguous stack lifetime is unsupported");
      return;
    }
    const auto *Entry = State.addressSpace().find(Candidate.Object->Id);
    if (!Entry || Entry->Object.get() != Candidate.Object.get() ||
        (!IsStart && !Entry->State->live())) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "stack lifetime object is absent or already dead");
      return;
    }
    bool Copied = false;
    auto Writable =
        State.addressSpace().getWriteable(Candidate.Object->Id, Copied);
    if (!Writable) {
      internalError(State, InternalErrorReason::MemoryModelError,
                    "stack lifetime object disappeared");
      return;
    }
    if (Copied) {
      ++Statistics.COWCopies;
      ++Statistics.ObjectStates;
    }
    if (IsStart)
      Writable->startLifetime(Expressions);
    else
      Writable->endLifetime();
    recordEvent(State, IsStart ? "lifetime_start" : "lifetime_end", {},
                Candidate.Object->Id, 0, Candidate.Object->Size);
    State.pc().advance();
  }

  IncompletenessReason
  unsupportedReason(const llvm::Instruction &Instruction) const {
    if (const auto *Load = llvm::dyn_cast<llvm::LoadInst>(&Instruction);
        Load && Load->isAtomic())
      return IncompletenessReason::UnsupportedAtomicInstruction;
    if (const auto *Store = llvm::dyn_cast<llvm::StoreInst>(&Instruction);
        Store && Store->isAtomic())
      return IncompletenessReason::UnsupportedAtomicInstruction;
    if (llvm::isa<llvm::AllocaInst, llvm::LoadInst, llvm::StoreInst,
                  llvm::GetElementPtrInst>(Instruction))
      return IncompletenessReason::UnsupportedMemoryInstruction;
    if (const auto *Intrinsic = llvm::dyn_cast<llvm::IntrinsicInst>(&Instruction)) {
      switch (Intrinsic->getIntrinsicID()) {
      case llvm::Intrinsic::memcpy:
      case llvm::Intrinsic::memmove:
      case llvm::Intrinsic::memset:
      case llvm::Intrinsic::lifetime_start:
      case llvm::Intrinsic::lifetime_end:
        return IncompletenessReason::UnsupportedMemoryInstruction;
      default:
        break;
      }
    }
    if (llvm::isa<llvm::CallBase>(Instruction))
      return llvm::isa<llvm::InvokeInst>(Instruction)
                 ? IncompletenessReason::UnsupportedExceptionInstruction
                 : IncompletenessReason::UnsupportedCall;
    if (llvm::isa<llvm::FenceInst, llvm::AtomicCmpXchgInst,
                  llvm::AtomicRMWInst>(Instruction))
      return IncompletenessReason::UnsupportedAtomicInstruction;
    if (hasFloatingType(Instruction))
      return IncompletenessReason::UnsupportedFloatingPoint;
    if (hasVectorType(Instruction))
      return IncompletenessReason::UnsupportedVector;
    if (hasAggregateType(Instruction))
      return IncompletenessReason::UnsupportedAggregate;
    switch (Instruction.getOpcode()) {
    case llvm::Instruction::Invoke:
    case llvm::Instruction::Resume:
    case llvm::Instruction::CleanupRet:
    case llvm::Instruction::CatchRet:
    case llvm::Instruction::CatchSwitch:
    case llvm::Instruction::CleanupPad:
    case llvm::Instruction::CatchPad:
    case llvm::Instruction::LandingPad:
      return IncompletenessReason::UnsupportedExceptionInstruction;
    default:
      return IncompletenessReason::UnsupportedInstruction;
    }
  }

  void executeInstruction(ExecutionState &State) {
    if (Options.MaxWallTimeMs != 0 &&
        static_cast<std::uint64_t>(
            std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now() - RunStart)
                .count()) >= Options.MaxWallTimeMs) {
      incomplete(State, IncompletenessReason::WallTimeLimit,
                 "run-wide wall-time limit reached");
      return;
    }
    if (executePHIs(State))
      return;
    llvm::Instruction *Instruction = State.pc().instruction();
    if (!Instruction) {
      internalError(State, InternalErrorReason::InvalidExecutorState,
                    "program counter has no instruction");
      return;
    }
    if (State.executedInstructions() >= Options.MaxInstructionsPerState) {
      incomplete(State, IncompletenessReason::InstructionLimit,
                 "per-state instruction limit reached");
      return;
    }
    State.addExecutedInstructions(1);
    ++Statistics.ExecutedInstructions;

    if (llvm::isa<llvm::FreezeInst>(Instruction)) {
      incomplete(State, IncompletenessReason::FreezeInstruction,
                 "M3 does not model freeze");
      return;
    }
    if (const auto *Overflowing =
            llvm::dyn_cast<llvm::OverflowingBinaryOperator>(Instruction);
        Overflowing &&
        (Overflowing->hasNoSignedWrap() || Overflowing->hasNoUnsignedWrap())) {
      incomplete(State, IncompletenessReason::NoWrapFlags,
                 "M3 does not erase or approximate nsw/nuw flags");
      return;
    }
    if (const auto *PossiblyExact =
            llvm::dyn_cast<llvm::PossiblyExactOperator>(Instruction);
        PossiblyExact && PossiblyExact->isExact()) {
      incomplete(State, IncompletenessReason::ExactFlag,
                 "M3 does not erase or approximate exact flags");
      return;
    }
    if (Instruction->hasPoisonGeneratingAnnotations() &&
        !llvm::isa<llvm::CallInst>(Instruction) &&
        !(llvm::isa<llvm::GetElementPtrInst>(Instruction) &&
          supportedGEPNoWrap(
              *llvm::cast<llvm::GetElementPtrInst>(Instruction)))) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "unsupported poison-generating flag, metadata, or return "
                 "attribute");
      return;
    }
    if (hasFloatingType(*Instruction)) {
      incomplete(State, IncompletenessReason::UnsupportedFloatingPoint,
                 "M3 does not support floating-point instructions or values");
      return;
    }
    if (hasVectorType(*Instruction)) {
      incomplete(State, IncompletenessReason::UnsupportedVector,
                 "M3 does not support vector instructions or values");
      return;
    }
    if (hasAggregateType(*Instruction) &&
        !llvm::isa<llvm::CallInst, llvm::ReturnInst>(Instruction) &&
        !(Options.EnableMemory &&
          llvm::isa<llvm::LoadInst, llvm::StoreInst,
                    llvm::ExtractValueInst, llvm::InsertValueInst>(Instruction))) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "aggregate instruction is outside the M6 subset");
      return;
    }

    if (Options.EnableMemory) {
      if (auto *Extract = llvm::dyn_cast<llvm::ExtractValueInst>(Instruction)) {
        executeExtractValue(*Extract, State);
        return;
      }
      if (auto *Insert = llvm::dyn_cast<llvm::InsertValueInst>(Instruction)) {
        executeInsertValue(*Insert, State);
        return;
      }
    }

    if (auto *Binary = llvm::dyn_cast<llvm::BinaryOperator>(Instruction)) {
      executeBinary(*Binary, State);
      return;
    }
    if (auto *Compare = llvm::dyn_cast<llvm::ICmpInst>(Instruction)) {
      executeICmp(*Compare, State);
      return;
    }
    if (auto *Cast = llvm::dyn_cast<llvm::CastInst>(Instruction)) {
      executeCast(*Cast, State);
      return;
    }
    if (auto *Select = llvm::dyn_cast<llvm::SelectInst>(Instruction)) {
      executeSelect(*Select, State);
      return;
    }
    if (Options.EnableMemory &&
        (llvm::isa<llvm::AllocaInst, llvm::GetElementPtrInst,
                   llvm::LoadInst, llvm::StoreInst>(Instruction))) {
      if (auto *Alloca = llvm::dyn_cast<llvm::AllocaInst>(Instruction)) {
        executeAlloca(*Alloca, State);
        return;
      }
      if (auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(Instruction)) {
        executeGEP(*GEP, State);
        return;
      }
      executeMemoryAccess(*Instruction, State);
      return;
    }
    if (Options.EnableMemory) {
      if (auto *Transfer = llvm::dyn_cast<llvm::MemTransferInst>(Instruction)) {
        executeMemTransfer(*Transfer, State);
        return;
      }
      if (auto *Set = llvm::dyn_cast<llvm::MemSetInst>(Instruction)) {
        executeMemSet(*Set, State);
        return;
      }
    }
    if (Options.EnableMemory) {
      if (auto *Intrinsic = llvm::dyn_cast<llvm::IntrinsicInst>(Instruction)) {
        if (Intrinsic->getIntrinsicID() == llvm::Intrinsic::lifetime_start ||
            Intrinsic->getIntrinsicID() == llvm::Intrinsic::lifetime_end) {
          executeStackLifetime(*Intrinsic, State);
          return;
        }
      }
    }
    if (Options.EnableRegistration) {
      if (auto *Call = llvm::dyn_cast<llvm::CallInst>(Instruction)) {
        const llvm::Function *Callee = Call->getCalledFunction();
        if (Callee && (Callee->getName() == "__formal_register_input" ||
                       Callee->getName() == "__formal_register_output" ||
                       Callee->getName() == "__formal_assume")) {
          executeRegistrationCall(*Call, State);
          return;
        }
      }
    }
    if (auto *Branch = llvm::dyn_cast<llvm::BranchInst>(Instruction)) {
      executeBranch(*Branch, State);
      return;
    }
    if (auto *Switch = llvm::dyn_cast<llvm::SwitchInst>(Instruction)) {
      executeSwitch(*Switch, State);
      return;
    }
    if (auto *Call = llvm::dyn_cast<llvm::CallInst>(Instruction)) {
      const llvm::Function *Callee = Call->getCalledFunction();
      if (Callee && Callee->isDeclaration()) {
        if (const ExternalFunctionModel *Model =
                findExternalFunctionModel(Callee->getName())) {
          if (!Callee->hasExternalLinkage() ||
              !matchesExternalModelSignature(*Model, *Call, *Callee,
                                             layout(State))) {
            incomplete(State, IncompletenessReason::UnsupportedCall,
                       "external model signature or ABI is unsupported: " +
                           Callee->getName().str());
            return;
          }
          if (Model->Kind != ExternalModelKind::Termination &&
              !Options.EnableMemory) {
            incomplete(State, IncompletenessReason::UnsupportedCall,
                       "external memory model needs M4: " +
                           Callee->getName().str());
            return;
          }
          recordEvent(State, "external_model", Model->Symbol);
          switch (Model->Kind) {
          case ExternalModelKind::Termination:
            executeTerminationCall(*Call, State);
            return;
          case ExternalModelKind::ByteMemory:
            executeLibcMemoryCall(*Call, State);
            return;
          case ExternalModelKind::Heap:
            executeHeapCall(*Call, State);
            return;
          }
        }
      }
      executeDefinedCall(*Call, State);
      return;
    }
    if (auto *Return = llvm::dyn_cast<llvm::ReturnInst>(Instruction)) {
      executeReturn(*Return, State);
      return;
    }
    if (llvm::isa<llvm::UnreachableInst>(Instruction)) {
      pathError(State, PathErrorReason::ReachedUnreachable,
                "a feasible path reached LLVM unreachable");
      return;
    }

    IncompletenessReason Reason = unsupportedReason(*Instruction);
    incomplete(State, Reason,
               "unsupported M3 opcode " +
                   std::string(Instruction->getOpcodeName()));
  }

  ExecutionResult run(llvm::Function &Function,
                      llvm::ArrayRef<ArgumentBinding> Arguments) {
    reset();
    ExecutionState Initial(NextStateId++, &Function);
    Initial.frame().setId(NextFrameId++);
    Statistics.MaximumCallDepthObserved = 1;
    Statistics.StatesCreated = 1;

    if (Function.isDeclaration()) {
      internalError(Initial, InternalErrorReason::InvalidExecutorState,
                    "executor entry must be a defined function");
      return finish();
    }
    if (Function.isVarArg()) {
      incomplete(Initial, IncompletenessReason::UnsupportedCall,
                 "variadic executor entry has no modeled argument ABI");
      return finish();
    }
    if (Options.EnableRegistration && Function.doesNotReturn()) {
      incomplete(Initial, IncompletenessReason::EntryAttributeConflict,
                 "noreturn M5 entry cannot produce a normal return relation");
      return finish();
    }
    if (Function.doesNotReturn() ||
        Function.getMemoryEffects() != llvm::MemoryEffects::unknown()) {
      incomplete(Initial, IncompletenessReason::EntryAttributeConflict,
                 "entry function effects or noreturn promise are unproved");
      return finish();
    }
    for (llvm::Attribute Attr : Function.getAttributes().getRetAttrs())
      if (!Attr.hasKindAsEnum() ||
          (Attr.getKindAsEnum() != llvm::Attribute::NoUndef &&
           !(Function.getReturnType()->isIntegerTy() &&
             (Attr.getKindAsEnum() == llvm::Attribute::ZExt ||
              Attr.getKindAsEnum() == llvm::Attribute::SExt)))) {
        incomplete(Initial, IncompletenessReason::EntryAttributeConflict,
                   "entry return ABI attribute is unsupported");
        return finish();
      }
    for (unsigned Index = 0; Index < Function.arg_size(); ++Index)
      for (llvm::Attribute Attr :
           Function.getAttributes().getParamAttrs(Index))
        if (!Attr.hasKindAsEnum() ||
            (Attr.getKindAsEnum() != llvm::Attribute::NoUndef &&
             !(Function.getArg(Index)->getType()->isIntegerTy() &&
               (Attr.getKindAsEnum() == llvm::Attribute::ZExt ||
                Attr.getKindAsEnum() == llvm::Attribute::SExt)))) {
          incomplete(Initial, IncompletenessReason::EntryAttributeConflict,
                     "entry parameter ABI attribute needs a caller proof");
          return finish();
        }
    if (!Function.getReturnType()->isVoidTy() &&
        !Function.getReturnType()->isIntegerTy()) {
      incomplete(Initial, IncompletenessReason::UnsupportedType,
                 "M3 function return type must be void or integer scalar");
      return finish();
    }
    for (const llvm::Argument &Argument : Function.args()) {
      if (!Argument.getType()->isIntegerTy() &&
          !Argument.getType()->isPointerTy()) {
        incomplete(Initial, IncompletenessReason::UnsupportedType,
                   "M4 function arguments must be integer or pointer scalars");
        return finish();
      }
    }

    std::size_t BoundArguments = 0;
    for (const ArgumentBinding &Binding : Arguments) {
      bool Matches = false;
      if (Binding.Argument && Binding.Argument->getType()->isIntegerTy() &&
          !Binding.Value.isPointer())
        Matches = Binding.Value.width() ==
                  llvm::cast<llvm::IntegerType>(Binding.Argument->getType())
                      ->getBitWidth();
      else if (Binding.Argument &&
               Binding.Argument->getType()->isPointerTy() &&
               Binding.Value.isPointer()) {
        auto *Type = llvm::cast<llvm::PointerType>(Binding.Argument->getType());
        Matches = Type->getAddressSpace() == 0 &&
                  Binding.Value.pointer().AddressSpace == 0 &&
                  Binding.Value.width() ==
                      Function.getParent()->getDataLayout().getPointerSizeInBits(0);
      }
      if (!Binding.Argument || Binding.Argument->getParent() != &Function ||
          !Matches || Initial.frame().lookup(Binding.Argument)) {
        internalError(Initial, InternalErrorReason::InvalidSymbolicBinding,
                      "argument binding is duplicate, foreign, unsupported, "
                      "or has the wrong width");
        return finish();
      }
      Initial.frame().bind(Binding.Argument, Binding.Value);
      ++BoundArguments;
    }
    if (BoundArguments != Function.arg_size()) {
      internalError(Initial, InternalErrorReason::MissingSSABinding,
                    "every M3 function argument must be pre-bound; fresh "
                    "symbols are never synthesized");
      return finish();
    }

    if (Options.EnableMemory) {
      try {
        if (!initializeGlobals(Initial))
          return finish();
      } catch (const MemoryModelException &Error) {
        internalError(Initial, InternalErrorReason::MemoryModelError,
                      Error.what());
        return finish();
      } catch (const std::exception &Error) {
        internalError(Initial,
                      InternalErrorReason::ExpressionConstructionFailure,
                      Error.what());
        return finish();
      }
    }
    for (const ArgumentBinding &Binding : Arguments) {
      if (Binding.Value.isPointer() &&
          !pointerObjectsBelongToState(Binding.Value.pointer(), Initial)) {
        incomplete(Initial, IncompletenessReason::PointerProvenanceLost,
                   "argument pointer object is outside this execution state");
        return finish();
      }
    }
    auto &EntryAnalysis = LoopAnalyses[&Function];
    // Build LLVM's structural loop view for every definition, including callees
    // that a bounded run may never reach.
    for (llvm::Function &Definition : *Function.getParent())
      if (!Definition.isDeclaration()) {
        auto &Analysis = LoopAnalyses[&Definition];
        if (!Analysis)
          Analysis = std::make_unique<LoopAnalysis>(Definition);
      }
    llvm::BasicBlock *EntryBlock = &Function.getEntryBlock();
    if (EntryAnalysis->IrreducibleBlocks.count(EntryBlock)) {
      incomplete(Initial, IncompletenessReason::IrreducibleCycle,
                 "function entry belongs to an irreducible CFG component");
      return finish();
    }
    for (llvm::Loop *Loop = EntryAnalysis->Loops.getLoopFor(EntryBlock);
         Loop; Loop = Loop->getParentLoop()) {
      if (Loop->getHeader() != EntryBlock)
        continue;
      const auto &Bound = boundFor(*EntryAnalysis, Loop);
      if (Bound.Error != IncompletenessReason::None) {
        incomplete(Initial, Bound.Error, Bound.Detail);
        return finish();
      }
      auto &Activation =
          Initial.frame().loopActivations()[Loop->getHeader()];
      Activation = {NextActivationId++, 0, Bound.Bound, Bound.Source};
      Initial.loopHistory().push_back(
          loopDiagnostic(Initial, Loop->getHeader(), Activation));
    }
    Worklist.schedule(std::move(Initial));
    while (!Worklist.empty()) {
      ExecutionState State = Worklist.takeNext();
      while (State.status() == ExecutionStateStatus::Running) {
        try {
          executeInstruction(State);
        } catch (const MemoryModelException &Error) {
          internalError(State, InternalErrorReason::MemoryModelError,
                        Error.what());
        } catch (const std::exception &Error) {
          internalError(State,
                        InternalErrorReason::ExpressionConstructionFailure,
                        std::string("uncaught executor exception: ") +
                            Error.what());
        } catch (...) {
          internalError(State, InternalErrorReason::InvalidExecutorState,
                        "uncaught non-standard executor exception");
        }
      }
    }
    return finish();
  }

  ExecutionResult finish() {
    std::stable_sort(Paths.begin(), Paths.end(),
                     [](const PathResult &Left, const PathResult &Right) {
                       return Left.StateId < Right.StateId;
                     });
    if (Options.EnableRegistration) {
      const PathResult *Reference = nullptr;
      for (PathResult &Path : Paths) {
        if (Path.Status != PathStatus::Completed)
          continue;
        if (!Reference) {
          Reference = &Path;
          continue;
        }
        bool Same = Path.Regions.size() == Reference->Regions.size();
        if (Same) {
          for (std::size_t Index = 0; Index < Path.Regions.size(); ++Index) {
            const auto &Left = Path.Regions[Index];
            const auto &Right = Reference->Regions[Index];
            if (Left.Input != Right.Input || Left.Name != Right.Name ||
                Left.Size != Right.Size) {
              Same = false;
              break;
            }
          }
        }
        if (!Same) {
          Path.Status = PathStatus::Incomplete;
          Path.IncompleteReason =
              IncompletenessReason::RegistrationDescriptorMismatch;
          Path.Detail =
              "normal return has path-varying registration descriptors";
          --Statistics.CompletedPaths;
          ++Statistics.IncompletePaths;
        }
      }
    }
    ExecutionResult Result;
    Result.Paths = std::move(Paths);
    for (const PathResult &Path : Result.Paths) {
      Result.HasLoopPolicyError |= Path.LoopPolicyError;
      Result.HasUnwindingAssumption |= Path.UnwindingAssumption;
    }
    Result.Statistics = Statistics;
    Result.Statistics.ASTNodes = Expressions.nodeCount();
    struct rusage Usage {};
    if (getrusage(RUSAGE_SELF, &Usage) == 0 && Usage.ru_maxrss >= 0)
      Result.Statistics.PeakResidentBytes =
          static_cast<std::uint64_t>(Usage.ru_maxrss) * 1024;
    if (Result.Paths.empty()) {
      Result.Status =
          Options.EnableRegistration && Statistics.AssumptionPrunedPaths != 0
              ? ExecutionStatus::Complete
              : ExecutionStatus::InternalError;
      return Result;
    }
    if (Statistics.InternalErrorPaths != 0)
      Result.Status = ExecutionStatus::InternalError;
    else if (Statistics.ErrorPaths != 0)
      Result.Status = ExecutionStatus::Error;
    else if (Statistics.IncompletePaths != 0)
      Result.Status = ExecutionStatus::Unknown;
    else
      Result.Status = ExecutionStatus::Complete;
    return Result;
  }

  ExprContext &Expressions;
  Solver &SolverBackend;
  ExecutorOptions Options;
  DFSWorklist Worklist;
  std::vector<PathResult> Paths;
  ExecutionStatistics Statistics;
  std::uint64_t NextStateId = 1;
  std::uint64_t NextFrameId = 1;
  std::uint64_t NextActivationId = 1;
  std::uint64_t NextObjectId = 1;
  std::uint64_t NextLogicalBase = 0x10000;
  std::chrono::steady_clock::time_point RunStart;
  std::map<const llvm::GlobalVariable *,
           std::shared_ptr<const MemoryObject>> GlobalObjects;
  std::map<const llvm::Function *, std::unique_ptr<LoopAnalysis>> LoopAnalyses;
};

ScalarExecutor::ScalarExecutor(ExprContext &Expressions, Solver &Solver,
                               ExecutorOptions Options)
    : Implementation(
          std::make_unique<Impl>(Expressions, Solver, std::move(Options))) {}

ScalarExecutor::~ScalarExecutor() = default;

ExecutionResult
ScalarExecutor::execute(llvm::Function &Function,
                        llvm::ArrayRef<ArgumentBinding> Arguments) {
  return Implementation->run(Function, Arguments);
}

void InstructionExecutor::execute(ExecutionState &State) {
  Executor.Implementation->executeInstruction(State);
}

} // namespace formal
