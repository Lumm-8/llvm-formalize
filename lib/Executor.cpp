#include "formal/Executor.h"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/GlobalAlias.h"

#include <algorithm>
#include <chrono>
#include <functional>
#include <limits>
#include <map>
#include <set>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
#include <sys/resource.h>

namespace formal {

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

bool hasUnsupportedMemcpyParamAttributes(const llvm::MemCpyInst &Copy) {
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
  Impl(ExprContext &Expressions, Solver &Solver, ExecutorOptions Options)
      : Expressions(Expressions), SolverBackend(Solver), Options(Options) {
    if (Options.MaxInstructionsPerState == 0 || Options.MaxStates == 0)
      throw std::invalid_argument("M3 executor limits must be positive");
  }

  void reset() {
    Worklist.clear();
    Paths.clear();
    Statistics = {};
    NextStateId = 1;
    NextObjectId = 1;
    NextLogicalBase = 0x10000;
    GlobalObjects.clear();
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

  PathResult basePath(const ExecutionState &State, PathStatus Status,
                      IRLocation Location, std::string Detail) const {
    PathResult Result;
    Result.StateId = State.id();
    Result.Depth = State.depth();
    Result.ExecutedInstructions = State.executedInstructions();
    Result.Status = Status;
    Result.Constraints = State.constraints().constraints();
    Result.Location = std::move(Location);
    Result.Detail = std::move(Detail);
    return Result;
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
    append(std::move(Result));
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
                 std::string Detail) {
    IRLocation Location = locationOf(State.pc().instruction());
    std::optional<Model> ErrorModel;
    if (Reason != PathErrorReason::ReachedUnreachable) {
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
                 bool ExternallyInitialized = false) {
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
      if (llvm::isa<llvm::GlobalAlias>(Value)) {
        incomplete(State, IncompletenessReason::UnsupportedGlobal,
                   "referenced global alias is unsupported");
        return false;
      }
      if (const auto *Global = llvm::dyn_cast<llvm::GlobalVariable>(Value)) {
        Needed.insert(Global);
        if (Global->hasInitializer() && !Global->isExternallyInitialized())
          Pending.push_back(Global->getInitializer());
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
    const unsigned Flags = GEP.getNoWrapFlags().getRaw();
    if (Flags != 0 && Flags != llvm::GEPNoWrapFlags::inBounds().getRaw()) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "M4 supports only unflagged or plain inbounds GEP");
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

  bool transfer(ExecutionState &State, llvm::BasicBlock *Destination) {
    if (!Destination) {
      internalError(State, InternalErrorReason::InvalidExecutorState,
                    "control-flow destination is null");
      return false;
    }
    if (State.hasVisited(Destination)) {
      incomplete(State, IncompletenessReason::LoopUnsupported,
                 "M3 rejects a repeated basic block/backedge");
      return false;
    }
    llvm::BasicBlock *Predecessor = State.pc().block();
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
    if (Value->getType()->isIntegerTy() && !Binding->isPointer())
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
    if (Instruction.getType()->isIntegerTy() && !Value.isPointer())
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
            if ((DifferentObjects || ObjectVsNull) &&
                (!IsBase(ACandidate) || !IsBase(BCandidate) ||
                 (ACandidate.Object && ACandidate.Object->Size == 0) ||
                 (BCandidate.Object && BCandidate.Object->Size == 0))) {
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
    if (!ValueType->isIntegerTy() && !ValueType->isPointerTy()) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "M4 load/store supports integer and pointer scalars only");
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
    if (*Size == 0) {
      incomplete(State, IncompletenessReason::UnsupportedType,
                 "zero-byte load/store type is unsupported");
      return;
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
        pathError(Child, PathErrorReason::DeadObjectAccess,
                  "memory access to a dead object");
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
      if (!IsStore) {
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
        Child.pc().advance();
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

  void executeMemCpy(llvm::MemCpyInst &Copy, ExecutionState &State) {
    if (hasUnsupportedMemcpyParamAttributes(Copy)) {
      incomplete(State, IncompletenessReason::PoisonGeneratingAnnotation,
                 "unsupported llvm.memcpy parameter attribute");
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
      pathError(State, PathErrorReason::DeadObjectAccess,
                "memcpy accesses a dead object");
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
    if (D.Object->Id == S.Object->Id &&
        *DestAt < *SourceAt + *Length &&
        *SourceAt < *DestAt + *Length) {
      incomplete(State, IncompletenessReason::UnsupportedMemoryInstruction,
                 "overlapping memcpy is outside the supported subset");
      return;
    }
    std::vector<ExprRef> Bytes;
    std::vector<ExprRef> AddressSensitiveBytes;
    Bytes.reserve(static_cast<std::size_t>(*Length));
    AddressSensitiveBytes.reserve(static_cast<std::size_t>(*Length));
    for (std::uint64_t Byte = 0; Byte < *Length; ++Byte) {
      ExprRef Offset = Expressions.constant(SourcePointer.Width, *SourceAt + Byte);
      ExprRef Ready = SourceEntry->State->initializedAt(Offset, Expressions);
      if (!Ready->isConstant() || Ready->constantValue().isZero()) {
        incomplete(State,
                   S.Object->ExternallyInitialized
                       ? IncompletenessReason::UnsupportedGlobal
                       : IncompletenessReason::UninitializedRead,
                   S.Object->ExternallyInitialized
                       ? "memcpy source is an externally initialized global with unknown entry value"
                       : "memcpy source contains an uninitialized byte");
        return;
      }
      Bytes.push_back(SourceEntry->State->readByte(Offset, Expressions));
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
      Writable->writeByte(Expressions.constant(DestPointer.Width, *DestAt + Byte),
                          Bytes[static_cast<std::size_t>(Byte)], Expressions,
                          false,
                          AddressSensitiveBytes[static_cast<std::size_t>(Byte)]);
      ++Statistics.ByteWriteExprs;
    }
    for (PointerShadowEntry &Shadow : Shadows)
      Writable->installShadow(std::move(Shadow));
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

  void executeReturn(llvm::ReturnInst &Return, ExecutionState &State) {
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
    }
    if (Return.getNumOperands() == 0) {
      completed(State, std::nullopt);
      return;
    }
    if (!Return.getReturnValue()->getType()->isIntegerTy()) {
      incomplete(State, IncompletenessReason::UnsupportedType,
                 "M3 return value must be void or an integer scalar");
      return;
    }
    std::optional<SymbolicValue> Value =
        resolve(Return.getReturnValue(), State);
    if (Value)
      completed(State, std::move(*Value));
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
        !(llvm::isa<llvm::GetElementPtrInst>(Instruction) &&
          llvm::cast<llvm::GetElementPtrInst>(Instruction)->getNoWrapFlags()
                  .getRaw() ==
              llvm::GEPNoWrapFlags::inBounds().getRaw())) {
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
    if (hasAggregateType(*Instruction)) {
      incomplete(State, IncompletenessReason::UnsupportedAggregate,
                 "M3 does not support aggregate instructions or values");
      return;
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
      if (auto *Copy = llvm::dyn_cast<llvm::MemCpyInst>(Instruction)) {
        executeMemCpy(*Copy, State);
        return;
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
    Statistics.StatesCreated = 1;

    if (Function.isDeclaration() || Function.isVarArg()) {
      internalError(Initial, InternalErrorReason::InvalidExecutorState,
                    "M3 entry must be a defined, non-variadic function");
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
    ExecutionResult Result;
    Result.Paths = std::move(Paths);
    Result.Statistics = Statistics;
    Result.Statistics.ASTNodes = Expressions.nodeCount();
    struct rusage Usage {};
    if (getrusage(RUSAGE_SELF, &Usage) == 0 && Usage.ru_maxrss >= 0)
      Result.Statistics.PeakResidentBytes =
          static_cast<std::uint64_t>(Usage.ru_maxrss) * 1024;
    if (Result.Paths.empty()) {
      Result.Status = ExecutionStatus::InternalError;
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
  std::uint64_t NextObjectId = 1;
  std::uint64_t NextLogicalBase = 0x10000;
  std::map<const llvm::GlobalVariable *,
           std::shared_ptr<const MemoryObject>> GlobalObjects;
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
