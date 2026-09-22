#include "formal/FormalPass.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugProgramInstruction.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Triple.h"

#include <algorithm>
#include <cctype>
#include <limits>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace {

class BoundedCountingStream final : public llvm::raw_ostream {
public:
  explicit BoundedCountingStream(std::size_t Limit) : Limit(Limit) {
    SetUnbuffered();
  }

  bool exceeded() const noexcept { return Exceeded; }

private:
  void write_impl(const char *, std::size_t Size) override {
    if (Size > Limit - std::min(Position, Limit)) {
      Position = Limit;
      Exceeded = true;
      return;
    }
    Position += Size;
  }

  std::uint64_t current_pos() const override { return Position; }

  const std::size_t Limit;
  std::size_t Position = 0;
  bool Exceeded = false;
};

std::vector<std::string> moduleProducerEntries(const llvm::Module &M) {
  const llvm::NamedMDNode *Ident = M.getNamedMetadata("llvm.ident");
  if (!Ident)
    return {};
  std::vector<std::string> Result;
  for (const llvm::MDNode *Node : Ident->operands()) {
    if (!Node || Node->getNumOperands() == 0)
      continue;
    const auto *Text = llvm::dyn_cast<llvm::MDString>(Node->getOperand(0));
    if (!Text)
      continue;
    Result.push_back(Text->getString().str());
  }
  return Result;
}

std::string joinProducerEntries(const std::vector<std::string> &Entries) {
  std::string Result;
  for (const std::string &Entry : Entries) {
    if (!Result.empty())
      Result += "; ";
    Result += Entry;
  }
  return Result;
}

std::optional<unsigned> versionMajor(llvm::StringRef Version) {
  unsigned Major = 0;
  if (Version.consumeInteger(10, Major))
    return std::nullopt;
  return Major;
}

std::optional<std::string> clangVersion(llvm::StringRef Producer) {
  constexpr llvm::StringLiteral Prefix("clang version ");
  const size_t Position = Producer.find(Prefix);
  if (Position == llvm::StringRef::npos)
    return std::nullopt;
  Producer = Producer.drop_front(Position + Prefix.size());
  llvm::StringRef Version = Producer.take_until([](char Character) {
    return std::isspace(static_cast<unsigned char>(Character)) ||
           Character == ';';
  });
  if (Version.empty())
    return std::nullopt;
  return Version.str();
}

formal::PreflightResult failure(formal::PreflightCode Code, llvm::Twine Message,
                                llvm::Function *Entry = nullptr) {
  return {Code, Entry, Message.str(), {}, {}};
}

template <typename Predicate>
bool typeTreeSatisfies(llvm::Type *Root, Predicate Matches) {
  std::vector<llvm::Type *> Worklist{Root};
  std::set<llvm::Type *> Visited;
  while (!Worklist.empty()) {
    llvm::Type *Type = Worklist.back();
    Worklist.pop_back();
    if (!Visited.insert(Type).second)
      continue;
    if (Matches(Type))
      return true;
    for (llvm::Type *Subtype : Type->subtypes())
      Worklist.push_back(Subtype);
  }
  return false;
}

bool typeUsesUnsupportedAddressSpace(llvm::Type *Type) {
  return typeTreeSatisfies(Type, [](llvm::Type *Candidate) {
    const auto *Pointer = llvm::dyn_cast<llvm::PointerType>(Candidate);
    return Pointer && Pointer->getAddressSpace() != 0;
  });
}

enum class VectorUse { None, Fixed, Scalable };

VectorUse typeVectorUse(llvm::Type *Type) {
  VectorUse Result = VectorUse::None;
  typeTreeSatisfies(Type, [&](llvm::Type *Candidate) {
    const auto *Vector = llvm::dyn_cast<llvm::VectorType>(Candidate);
    if (!Vector)
      return false;
    Result = Vector->getElementCount().isScalable() ? VectorUse::Scalable
                                                    : VectorUse::Fixed;
    return Result == VectorUse::Scalable;
  });
  return Result;
}

bool typeUsesFloatingPoint(llvm::Type *Type) {
  return typeTreeSatisfies(Type, [](llvm::Type *Candidate) {
    return Candidate->isFloatingPointTy();
  });
}

std::optional<formal::PreflightCode>
unsupportedSemanticType(llvm::Type *Type) {
  VectorUse Vectors = typeVectorUse(Type);
  if (Vectors == VectorUse::Scalable)
    return formal::PreflightCode::ScalableVector;
  if (Vectors == VectorUse::Fixed)
    return formal::PreflightCode::FixedVector;
  if (typeUsesFloatingPoint(Type))
    return formal::PreflightCode::FloatingPoint;
  if (typeUsesUnsupportedAddressSpace(Type))
    return formal::PreflightCode::UnsupportedAddressSpace;
  return std::nullopt;
}

llvm::SmallVector<llvm::Type *, 2>
opaquePointerSemanticTypes(const llvm::Value &Value) {
  llvm::SmallVector<llvm::Type *, 2> Types;
  if (const auto *Allocation = llvm::dyn_cast<llvm::AllocaInst>(&Value))
    Types.push_back(Allocation->getAllocatedType());
  if (const auto *GEP = llvm::dyn_cast<llvm::GEPOperator>(&Value))
    Types.push_back(GEP->getSourceElementType());
  return Types;
}

template <typename Predicate>
bool instructionTypeSatisfies(const llvm::Instruction &Instruction,
                              Predicate Matches) {
  if (!Instruction.getType()->isVoidTy() && Matches(Instruction.getType()))
    return true;
  for (const llvm::Use &Operand : Instruction.operands())
    if (Matches(Operand->getType()))
      return true;
  for (llvm::Type *HiddenType : opaquePointerSemanticTypes(Instruction))
    if (Matches(HiddenType))
      return true;
  return false;
}

VectorUse instructionVectorUse(const llvm::Instruction &Instruction) {
  VectorUse Result = VectorUse::None;
  instructionTypeSatisfies(Instruction, [&](llvm::Type *Type) {
    VectorUse Current = typeVectorUse(Type);
    if (Current == VectorUse::Scalable)
      Result = VectorUse::Scalable;
    else if (Current == VectorUse::Fixed && Result == VectorUse::None)
      Result = VectorUse::Fixed;
    return Result == VectorUse::Scalable;
  });
  return Result;
}

bool instructionUsesFloatingPoint(const llvm::Instruction &Instruction) {
  return instructionTypeSatisfies(Instruction, typeUsesFloatingPoint);
}

bool acceptedOpcode(unsigned Opcode) {
  switch (Opcode) {
  case llvm::Instruction::Ret:
  case llvm::Instruction::Br:
  case llvm::Instruction::Switch:
  case llvm::Instruction::Unreachable:
  case llvm::Instruction::Add:
  case llvm::Instruction::Sub:
  case llvm::Instruction::Mul:
  case llvm::Instruction::UDiv:
  case llvm::Instruction::SDiv:
  case llvm::Instruction::URem:
  case llvm::Instruction::SRem:
  case llvm::Instruction::Shl:
  case llvm::Instruction::LShr:
  case llvm::Instruction::AShr:
  case llvm::Instruction::And:
  case llvm::Instruction::Or:
  case llvm::Instruction::Xor:
  case llvm::Instruction::Alloca:
  case llvm::Instruction::Load:
  case llvm::Instruction::Store:
  case llvm::Instruction::GetElementPtr:
  case llvm::Instruction::Trunc:
  case llvm::Instruction::ZExt:
  case llvm::Instruction::SExt:
  case llvm::Instruction::PtrToInt:
  case llvm::Instruction::IntToPtr:
  case llvm::Instruction::BitCast:
  case llvm::Instruction::AddrSpaceCast:
  case llvm::Instruction::ICmp:
  case llvm::Instruction::PHI:
  case llvm::Instruction::Call:
  case llvm::Instruction::Select:
  case llvm::Instruction::ExtractValue:
  case llvm::Instruction::InsertValue:
    return true;

  // These are admitted here only so the dedicated atomic gate below can
  // return ATOMIC_INSTRUCTION rather than the generic unsupported-opcode
  // reason. They are never VALIDATED.
  case llvm::Instruction::Fence:
  case llvm::Instruction::AtomicCmpXchg:
  case llvm::Instruction::AtomicRMW:
    return true;
  default:
    return false;
  }
}

std::optional<formal::PreflightCode>
unsupportedConstantTree(const llvm::Constant &Root) {
  std::vector<const llvm::Constant *> Worklist{&Root};
  std::set<const llvm::Constant *> Visited;
  while (!Worklist.empty()) {
    const llvm::Constant *Constant = Worklist.back();
    Worklist.pop_back();
    if (!Visited.insert(Constant).second)
      continue;
    if (llvm::isa<llvm::UndefValue>(Constant) ||
        llvm::isa<llvm::PoisonValue>(Constant))
      return formal::PreflightCode::UnsupportedValueSemantics;
    if (const auto *Expression = llvm::dyn_cast<llvm::ConstantExpr>(Constant)) {
      const auto *Operation = llvm::cast<llvm::Operator>(Expression);
      if (Operation->hasPoisonGeneratingFlags())
        return formal::PreflightCode::UnsupportedValueSemantics;
      if (!acceptedOpcode(Expression->getOpcode()))
        return formal::PreflightCode::UnsupportedOpcode;
    }
    if (std::optional<formal::PreflightCode> Unsupported =
            unsupportedSemanticType(Constant->getType()))
      return Unsupported;
    for (llvm::Type *HiddenType : opaquePointerSemanticTypes(*Constant))
      if (std::optional<formal::PreflightCode> Unsupported =
              unsupportedSemanticType(HiddenType))
        return Unsupported;
    for (const llvm::Use &Operand : Constant->operands())
      if (const auto *Child = llvm::dyn_cast<llvm::Constant>(Operand.get()))
        Worklist.push_back(Child);
  }
  return std::nullopt;
}

bool addWithinLimit(std::size_t &Total, std::size_t Amount, std::size_t Limit) {
  if (Amount > Limit - std::min(Total, Limit))
    return false;
  Total += Amount;
  return true;
}

std::optional<formal::PreflightResult>
validateResourceBounds(llvm::Module &M, formal::PreflightLimits Limits) {
  struct TypeWorkItem {
    llvm::Type *Type;
    unsigned AggregateDepth;
  };
  std::vector<TypeWorkItem> TypeWorklist;
  std::size_t TypeWork = 0;
  auto AddTypeAtDepth = [&](llvm::Type *Type, unsigned AggregateDepth) {
    if (TypeWork >= Limits.MaxTypeWork ||
        TypeWorklist.size() >= Limits.MaxTypeWork - TypeWork)
      return false;
    TypeWorklist.push_back({Type, AggregateDepth});
    return true;
  };
  auto AddType = [&](llvm::Type *Type) { return AddTypeAtDepth(Type, 0); };

  for (llvm::GlobalVariable &Global : M.globals())
    if (!AddType(Global.getValueType()))
      return failure(formal::PreflightCode::ResourceLimit,
                     "type traversal work limit exceeded");
  for (llvm::Function &Function : M) {
    if (!AddType(Function.getFunctionType()))
      return failure(formal::PreflightCode::ResourceLimit,
                     "type traversal work limit exceeded");
    for (llvm::Instruction &Instruction : llvm::instructions(Function)) {
      if (!AddType(Instruction.getType()))
        return failure(formal::PreflightCode::ResourceLimit,
                       "type traversal work limit exceeded");
      for (llvm::Use &Operand : Instruction.operands())
        if (!AddType(Operand->getType()))
          return failure(formal::PreflightCode::ResourceLimit,
                         "type traversal work limit exceeded");
      for (llvm::Type *HiddenType :
           opaquePointerSemanticTypes(Instruction))
        if (!AddType(HiddenType))
          return failure(formal::PreflightCode::ResourceLimit,
                         "type traversal work limit exceeded");
    }
  }

  std::unordered_map<llvm::Type *, unsigned> GreatestTypeDepth;
  auto DrainTypeWorklist = [&]() -> std::optional<formal::PreflightResult> {
    while (!TypeWorklist.empty()) {
      TypeWorkItem Item = TypeWorklist.back();
      TypeWorklist.pop_back();
      if (++TypeWork > Limits.MaxTypeWork)
        return failure(formal::PreflightCode::ResourceLimit,
                       "type traversal work limit exceeded");
      auto Existing = GreatestTypeDepth.find(Item.Type);
      if (Existing != GreatestTypeDepth.end() &&
          Existing->second >= Item.AggregateDepth)
        continue;
      GreatestTypeDepth[Item.Type] = Item.AggregateDepth;
      if (const auto *Integer = llvm::dyn_cast<llvm::IntegerType>(Item.Type))
        if (Integer->getBitWidth() > Limits.MaxIntegerBitWidth)
          return failure(formal::PreflightCode::ResourceLimit,
                         "LLVM integer bit width limit exceeded");
      unsigned ChildDepth =
          Item.AggregateDepth + (Item.Type->isAggregateType() ? 1U : 0U);
      if (ChildDepth > Limits.MaxAggregateDepth)
        return failure(formal::PreflightCode::ResourceLimit,
                       "LLVM aggregate type nesting limit exceeded");
      for (llvm::Type *Subtype : Item.Type->subtypes())
        if (!AddTypeAtDepth(Subtype, ChildDepth))
          return failure(formal::PreflightCode::ResourceLimit,
                         "type traversal work limit exceeded");
    }
    return std::nullopt;
  };
  if (std::optional<formal::PreflightResult> Error = DrainTypeWorklist())
    return Error;

  std::size_t GlobalInitializerBytes = 0;
  struct ConstantWorkItem {
    const llvm::Constant *Value;
    unsigned Depth;
  };
  std::vector<ConstantWorkItem> ConstantWorklist;
  std::size_t ConstantWork = 0;
  auto AddConstant = [&](const llvm::Constant *Constant, unsigned Depth) {
    if (ConstantWork >= Limits.MaxConstantNodes ||
        ConstantWorklist.size() >= Limits.MaxConstantNodes - ConstantWork)
      return false;
    ConstantWorklist.push_back({Constant, Depth});
    return true;
  };
  for (llvm::GlobalVariable &Global : M.globals()) {
    if (!Global.hasInitializer())
      continue;
    llvm::TypeSize Size =
        M.getDataLayout().getTypeAllocSize(Global.getValueType());
    if (!addWithinLimit(GlobalInitializerBytes, Size.getKnownMinValue(),
                        Limits.MaxGlobalInitializerBytes))
      return failure(formal::PreflightCode::ResourceLimit,
                     "global initializer size limit exceeded");
    if (!AddConstant(Global.getInitializer(), 0))
      return failure(formal::PreflightCode::ResourceLimit,
                     "constant traversal work limit exceeded");
  }
  for (llvm::Function &Function : M)
    for (llvm::Instruction &Instruction : llvm::instructions(Function))
      for (llvm::Use &Operand : Instruction.operands())
        if (const auto *Constant =
                llvm::dyn_cast<llvm::Constant>(Operand.get())) {
          if (!AddConstant(Constant, 0))
            return failure(formal::PreflightCode::ResourceLimit,
                           "constant traversal work limit exceeded");
        }

  std::set<const llvm::Constant *> VisitedConstants;
  std::unordered_map<const llvm::Constant *, unsigned> GreatestConstantDepth;
  std::size_t ConstantBytes = 0;
  auto DrainConstantWorklist = [&]() -> std::optional<formal::PreflightResult> {
    while (!ConstantWorklist.empty()) {
      ConstantWorkItem Item = ConstantWorklist.back();
      ConstantWorklist.pop_back();
      if (++ConstantWork > Limits.MaxConstantNodes)
        return failure(formal::PreflightCode::ResourceLimit,
                       "constant traversal work limit exceeded");
      auto ExistingDepth = GreatestConstantDepth.find(Item.Value);
      if (ExistingDepth != GreatestConstantDepth.end() &&
          ExistingDepth->second >= Item.Depth)
        continue;
      GreatestConstantDepth[Item.Value] = Item.Depth;
      if (Item.Depth > Limits.MaxConstantDepth)
        return failure(formal::PreflightCode::ResourceLimit,
                       "constant graph nesting limit exceeded");

      if (VisitedConstants.insert(Item.Value).second) {
        if (VisitedConstants.size() > Limits.MaxConstantNodes)
          return failure(formal::PreflightCode::ResourceLimit,
                         "constant node limit exceeded");
        llvm::Type *Type = Item.Value->getType();
        if (!AddType(Type))
          return failure(formal::PreflightCode::ResourceLimit,
                         "type traversal work limit exceeded");
        for (llvm::Type *HiddenType :
             opaquePointerSemanticTypes(*Item.Value))
          if (!AddType(HiddenType))
            return failure(formal::PreflightCode::ResourceLimit,
                           "type traversal work limit exceeded");
        std::size_t Bytes = 0;
        if (Type->isSized()) {
          llvm::TypeSize Size = M.getDataLayout().getTypeStoreSize(Type);
          Bytes = Size.getKnownMinValue();
        }
        if (!addWithinLimit(ConstantBytes, Bytes, Limits.MaxConstantBytes))
          return failure(formal::PreflightCode::ResourceLimit,
                         "constant payload size limit exceeded");
      }

      for (const llvm::Use &Operand : Item.Value->operands())
        if (const auto *Child = llvm::dyn_cast<llvm::Constant>(Operand.get()))
          if (!AddConstant(Child, Item.Depth + 1))
            return failure(formal::PreflightCode::ResourceLimit,
                           "constant traversal work limit exceeded");
    }
    return std::nullopt;
  };
  if (std::optional<formal::PreflightResult> Error = DrainConstantWorklist())
    return Error;

  const llvm::NamedMDNode *Ident = M.getNamedMetadata("llvm.ident");
  std::size_t ProducerBytes = 0;
  if (Ident) {
    if (Ident->getNumOperands() > Limits.MaxProducerEntries)
      return failure(formal::PreflightCode::ResourceLimit,
                     "llvm.ident entry count limit exceeded");
    for (const llvm::MDNode *Node : Ident->operands()) {
      if (!Node || Node->getNumOperands() == 0)
        continue;
      const auto *Text = llvm::dyn_cast<llvm::MDString>(Node->getOperand(0));
      if (Text && !addWithinLimit(ProducerBytes, Text->getString().size(),
                                  Limits.MaxProducerBytes))
        return failure(formal::PreflightCode::ResourceLimit,
                       "llvm.ident string byte limit exceeded");
    }
  }

  std::vector<const llvm::Metadata *> MetadataWorklist;
  std::vector<const llvm::Metadata *> MetadataRoots;
  std::vector<
      std::pair<const llvm::Metadata *, const llvm::Metadata *>>
      MetadataEdges;
  std::size_t MetadataRootCount = 0;
  std::size_t MetadataTraversalWork = 0;
  auto ScheduleMetadata = [&](const llvm::Metadata *Value) {
    if (!Value)
      return true;
    if (!addWithinLimit(MetadataTraversalWork, 1,
                        Limits.MaxMetadataNodes))
      return false;
    MetadataWorklist.push_back(Value);
    return true;
  };
  auto AddMetadataRoot = [&](const llvm::Metadata *Value) {
    if (!Value)
      return true;
    if (!addWithinLimit(MetadataRootCount, 1, Limits.MaxMetadataNodes))
      return false;
    if (!ScheduleMetadata(Value))
      return false;
    MetadataRoots.push_back(Value);
    return true;
  };
  for (const llvm::NamedMDNode &Named : M.named_metadata()) {
    if (!addWithinLimit(MetadataRootCount, 1, Limits.MaxMetadataNodes))
      return failure(formal::PreflightCode::ResourceLimit,
                     "metadata node limit exceeded");
    for (const llvm::MDNode *Operand : Named.operands())
      if (!AddMetadataRoot(Operand))
        return failure(formal::PreflightCode::ResourceLimit,
                       "metadata root limit exceeded");
  }
  auto AddAttachments = [&](const auto &Value) {
    llvm::SmallVector<std::pair<unsigned, llvm::MDNode *>, 8> Attachments;
    Value.getAllMetadata(Attachments);
    for (const auto &[Kind, Node] : Attachments) {
      (void)Kind;
      if (!AddMetadataRoot(Node))
        return false;
    }
    return true;
  };
  for (const llvm::GlobalVariable &Global : M.globals())
    if (!AddAttachments(Global))
      return failure(formal::PreflightCode::ResourceLimit,
                     "metadata root limit exceeded");
  for (const llvm::Function &Function : M) {
    if (!AddAttachments(Function))
      return failure(formal::PreflightCode::ResourceLimit,
                     "metadata root limit exceeded");
    for (const llvm::Instruction &Instruction : llvm::instructions(Function)) {
      if (!AddAttachments(Instruction))
        return failure(formal::PreflightCode::ResourceLimit,
                       "metadata root limit exceeded");
      for (const llvm::DbgRecord &Record : Instruction.getDbgRecordRange()) {
        if (!addWithinLimit(MetadataRootCount, 1, Limits.MaxMetadataNodes) ||
            !AddMetadataRoot(Record.getDebugLoc().getAsMDNode()))
          return failure(formal::PreflightCode::ResourceLimit,
                         "debug record limit exceeded");
        if (const auto *Label = llvm::dyn_cast<llvm::DbgLabelRecord>(&Record)) {
          if (!AddMetadataRoot(Label->getRawLabel()))
            return failure(formal::PreflightCode::ResourceLimit,
                           "debug record metadata limit exceeded");
          continue;
        }
        const auto &Variable = llvm::cast<llvm::DbgVariableRecord>(Record);
        if (!AddMetadataRoot(Variable.getRawLocation()) ||
            !AddMetadataRoot(Variable.getRawVariable()) ||
            !AddMetadataRoot(Variable.getRawExpression()) ||
            (Variable.isDbgAssign() &&
             (!AddMetadataRoot(Variable.getRawAddress()) ||
              !AddMetadataRoot(Variable.getRawAssignID()) ||
              !AddMetadataRoot(Variable.getRawAddressExpression()))))
          return failure(formal::PreflightCode::ResourceLimit,
                         "debug record metadata limit exceeded");
      }
    }
  }

  std::set<const llvm::Metadata *> VisitedMetadata;
  std::size_t MetadataBytes = 0;
  while (!MetadataWorklist.empty()) {
    const llvm::Metadata *Value = MetadataWorklist.back();
    MetadataWorklist.pop_back();
    if (!VisitedMetadata.insert(Value).second)
      continue;
    if (VisitedMetadata.size() > Limits.MaxMetadataNodes)
      return failure(formal::PreflightCode::ResourceLimit,
                     "metadata node limit exceeded");
    if (const auto *Text = llvm::dyn_cast<llvm::MDString>(Value)) {
      if (!addWithinLimit(MetadataBytes, Text->getString().size(),
                          Limits.MaxMetadataBytes))
        return failure(formal::PreflightCode::ResourceLimit,
                       "metadata string byte limit exceeded");
      continue;
    }
    if (const auto *ValueMetadata =
            llvm::dyn_cast<llvm::ValueAsMetadata>(Value)) {
      llvm::Value *WrappedValue = ValueMetadata->getValue();
      if (!AddType(WrappedValue->getType()))
        return failure(formal::PreflightCode::ResourceLimit,
                       "type traversal work limit exceeded");
      if (const auto *Constant = llvm::dyn_cast<llvm::Constant>(WrappedValue))
        if (!AddConstant(Constant, 0))
          return failure(formal::PreflightCode::ResourceLimit,
                         "constant traversal work limit exceeded");
      continue;
    }
    if (const auto *Node = llvm::dyn_cast<llvm::MDNode>(Value)) {
      for (const llvm::MDOperand &Operand : Node->operands()) {
        if (const llvm::Metadata *Child = Operand.get()) {
          if (!ScheduleMetadata(Child))
            return failure(formal::PreflightCode::ResourceLimit,
                           "metadata traversal work limit exceeded");
          else
            MetadataEdges.emplace_back(Value, Child);
        }
      }
      continue;
    }
    if (const auto *Arguments = llvm::dyn_cast<llvm::DIArgList>(Value)) {
      for (const llvm::ValueAsMetadata *Argument : Arguments->getArgs())
        if (!ScheduleMetadata(Argument))
          return failure(formal::PreflightCode::ResourceLimit,
                         "metadata traversal work limit exceeded");
        else
          MetadataEdges.emplace_back(Value, Argument);
    }
  }

  // LLVM metadata is a graph, not a tree: self-referential llvm.loop nodes and
  // recursive debug/type metadata are valid. Compute nesting over the SCC
  // condensation DAG so a cycle is one component, while distinct paths through
  // a shared acyclic graph still contribute their full greatest depth.
  std::vector<const llvm::Metadata *> MetadataNodes(VisitedMetadata.begin(),
                                                     VisitedMetadata.end());
  std::unordered_map<const llvm::Metadata *, std::size_t> MetadataIndex;
  MetadataIndex.reserve(MetadataNodes.size());
  for (std::size_t Index = 0; Index != MetadataNodes.size(); ++Index)
    MetadataIndex.emplace(MetadataNodes[Index], Index);

  std::vector<std::vector<std::size_t>> MetadataSuccessors(
      MetadataNodes.size());
  std::vector<std::vector<std::size_t>> MetadataPredecessors(
      MetadataNodes.size());
  for (const auto &[Parent, Child] : MetadataEdges) {
    const std::size_t ParentIndex = MetadataIndex.at(Parent);
    const std::size_t ChildIndex = MetadataIndex.at(Child);
    MetadataSuccessors[ParentIndex].push_back(ChildIndex);
    MetadataPredecessors[ChildIndex].push_back(ParentIndex);
  }

  struct MetadataDFSFrame {
    std::size_t Node;
    std::size_t NextSuccessor;
  };
  std::vector<unsigned char> MetadataVisitState(MetadataNodes.size(), 0);
  std::vector<std::size_t> MetadataFinishOrder;
  MetadataFinishOrder.reserve(MetadataNodes.size());
  std::vector<MetadataDFSFrame> MetadataDFS;
  for (std::size_t Start = 0; Start != MetadataNodes.size(); ++Start) {
    if (MetadataVisitState[Start] != 0)
      continue;
    MetadataVisitState[Start] = 1;
    MetadataDFS.push_back({Start, 0});
    while (!MetadataDFS.empty()) {
      MetadataDFSFrame &Frame = MetadataDFS.back();
      if (Frame.NextSuccessor != MetadataSuccessors[Frame.Node].size()) {
        const std::size_t Child =
            MetadataSuccessors[Frame.Node][Frame.NextSuccessor++];
        if (MetadataVisitState[Child] == 0) {
          MetadataVisitState[Child] = 1;
          MetadataDFS.push_back({Child, 0});
        }
        continue;
      }
      MetadataVisitState[Frame.Node] = 2;
      MetadataFinishOrder.push_back(Frame.Node);
      MetadataDFS.pop_back();
    }
  }

  const std::size_t NoComponent = std::numeric_limits<std::size_t>::max();
  std::vector<std::size_t> MetadataComponent(MetadataNodes.size(), NoComponent);
  std::vector<std::size_t> ComponentWorklist;
  std::size_t ComponentCount = 0;
  for (auto Position = MetadataFinishOrder.rbegin();
       Position != MetadataFinishOrder.rend(); ++Position) {
    if (MetadataComponent[*Position] != NoComponent)
      continue;
    MetadataComponent[*Position] = ComponentCount;
    ComponentWorklist.push_back(*Position);
    while (!ComponentWorklist.empty()) {
      const std::size_t Node = ComponentWorklist.back();
      ComponentWorklist.pop_back();
      for (const std::size_t Parent : MetadataPredecessors[Node]) {
        if (MetadataComponent[Parent] != NoComponent)
          continue;
        MetadataComponent[Parent] = ComponentCount;
        ComponentWorklist.push_back(Parent);
      }
    }
    ++ComponentCount;
  }

  std::vector<std::pair<std::size_t, std::size_t>> ComponentEdgePairs;
  ComponentEdgePairs.reserve(MetadataEdges.size());
  for (const auto &[Parent, Child] : MetadataEdges) {
    const std::size_t ParentComponent =
        MetadataComponent[MetadataIndex.at(Parent)];
    const std::size_t ChildComponent =
        MetadataComponent[MetadataIndex.at(Child)];
    if (ParentComponent != ChildComponent)
      ComponentEdgePairs.emplace_back(ParentComponent, ChildComponent);
  }
  llvm::sort(ComponentEdgePairs);
  ComponentEdgePairs.erase(
      std::unique(ComponentEdgePairs.begin(), ComponentEdgePairs.end()),
      ComponentEdgePairs.end());

  std::vector<std::vector<std::size_t>> ComponentSuccessors(ComponentCount);
  std::vector<std::size_t> ComponentInDegree(ComponentCount, 0);
  for (const auto &[Parent, Child] : ComponentEdgePairs) {
    ComponentSuccessors[Parent].push_back(Child);
    ++ComponentInDegree[Child];
  }

  std::vector<unsigned char> ComponentHasDepth(ComponentCount, 0);
  std::vector<unsigned> ComponentDepth(ComponentCount, 0);
  for (const llvm::Metadata *Root : MetadataRoots) {
    const std::size_t RootComponent =
        MetadataComponent[MetadataIndex.at(Root)];
    ComponentHasDepth[RootComponent] = 1;
  }

  std::vector<std::size_t> ReadyComponents;
  ReadyComponents.reserve(ComponentCount);
  for (std::size_t Component = 0; Component != ComponentCount; ++Component)
    if (ComponentInDegree[Component] == 0)
      ReadyComponents.push_back(Component);
  for (std::size_t ReadyIndex = 0; ReadyIndex != ReadyComponents.size();
       ++ReadyIndex) {
    const std::size_t Component = ReadyComponents[ReadyIndex];
    for (const std::size_t Child : ComponentSuccessors[Component]) {
      if (ComponentHasDepth[Component]) {
        if (ComponentDepth[Component] >= Limits.MaxMetadataDepth)
          return failure(formal::PreflightCode::ResourceLimit,
                         "metadata nesting depth limit exceeded");
        const unsigned ChildDepth = ComponentDepth[Component] + 1;
        if (!ComponentHasDepth[Child] || ComponentDepth[Child] < ChildDepth) {
          ComponentHasDepth[Child] = 1;
          ComponentDepth[Child] = ChildDepth;
        }
      }
      if (--ComponentInDegree[Child] == 0)
        ReadyComponents.push_back(Child);
    }
  }

  if (std::optional<formal::PreflightResult> Error = DrainTypeWorklist())
    return Error;
  if (std::optional<formal::PreflightResult> Error = DrainConstantWorklist())
    return Error;
  if (std::optional<formal::PreflightResult> Error = DrainTypeWorklist())
    return Error;

  BoundedCountingStream ModuleText(Limits.MaxModuleTextBytes);
  M.print(ModuleText, nullptr);
  if (ModuleText.exceeded())
    return failure(formal::PreflightCode::ResourceLimit,
                   "canonical module text byte limit exceeded");
  return std::nullopt;
}

std::optional<formal::PreflightCode>
unsupportedInstruction(const llvm::Instruction &Instruction) {
  VectorUse Vectors = instructionVectorUse(Instruction);
  if (Vectors == VectorUse::Scalable)
    return formal::PreflightCode::ScalableVector;
  if (Vectors == VectorUse::Fixed)
    return formal::PreflightCode::FixedVector;
  if (instructionUsesFloatingPoint(Instruction))
    return formal::PreflightCode::FloatingPoint;

  switch (Instruction.getOpcode()) {
  case llvm::Instruction::Invoke:
  case llvm::Instruction::Resume:
  case llvm::Instruction::CleanupRet:
  case llvm::Instruction::CatchRet:
  case llvm::Instruction::CatchSwitch:
  case llvm::Instruction::CleanupPad:
  case llvm::Instruction::CatchPad:
  case llvm::Instruction::LandingPad:
    return formal::PreflightCode::ExceptionHandling;
  case llvm::Instruction::Freeze:
    return formal::PreflightCode::FreezeInstruction;
  case llvm::Instruction::IndirectBr:
  case llvm::Instruction::CallBr:
  case llvm::Instruction::UserOp1:
  case llvm::Instruction::UserOp2:
  case llvm::Instruction::VAArg:
    return formal::PreflightCode::UnsupportedOpcode;
  default:
    break;
  }

  if (Instruction.hasPoisonGeneratingAnnotations())
    return formal::PreflightCode::UnsupportedValueSemantics;

  if (!acceptedOpcode(Instruction.getOpcode()))
    return formal::PreflightCode::UnsupportedOpcode;

  for (const llvm::Use &Operand : Instruction.operands())
    if (const auto *Constant = llvm::dyn_cast<llvm::Constant>(Operand.get()))
      if (std::optional<formal::PreflightCode> Unsupported =
              unsupportedConstantTree(*Constant))
        return Unsupported;
  return std::nullopt;
}

bool acceptedIntrinsic(llvm::Intrinsic::ID Id) {
  switch (Id) {
  case llvm::Intrinsic::dbg_assign:
  case llvm::Intrinsic::dbg_declare:
  case llvm::Intrinsic::dbg_label:
  case llvm::Intrinsic::dbg_value:
  case llvm::Intrinsic::lifetime_start:
  case llvm::Intrinsic::lifetime_end:
  case llvm::Intrinsic::memcpy:
  case llvm::Intrinsic::memmove:
  case llvm::Intrinsic::memset:
    return true;
  default:
    return false;
  }
}

bool isRegistrationBuiltin(llvm::StringRef Name) {
  return Name == "__formal_register_input" ||
         Name == "__formal_register_output" || Name == "__formal_assume";
}

std::optional<std::string>
validateRegistrationCall(const llvm::CallBase &Call,
                         const llvm::Function &Callee,
                         std::set<std::string> &RegistrationNames) {
  if (!isRegistrationBuiltin(Callee.getName()))
    return std::nullopt;
  llvm::FunctionType *Type = Callee.getFunctionType();
  if (!Type->getReturnType()->isVoidTy() || Type->isVarArg())
    return "formal registration builtin must return void and not be variadic";

  if (Callee.getName() == "__formal_assume") {
    if (Call.arg_size() != 1 ||
        !Call.getArgOperand(0)->getType()->isIntegerTy(32))
      return "__formal_assume must have exactly one i32 argument";
    return std::string();
  }

  if (Call.arg_size() != 3 ||
      !Call.getArgOperand(0)->getType()->isPointerTy() ||
      !Call.getArgOperand(1)->getType()->isIntegerTy(64) ||
      !Call.getArgOperand(2)->getType()->isPointerTy())
    return "formal input/output registration must have type void (ptr, i64, "
           "ptr)";

  llvm::StringRef Name;
  if (!llvm::getConstantStringInfo(Call.getArgOperand(2), Name, true) ||
      Name.empty())
    return "formal registration name must be a non-empty constant string";
  if (Name.size() > 1024)
    return "formal registration name exceeds the 1024-byte limit";
  for (unsigned char Character : Name)
    if (Character < 0x20 || Character > 0x7e)
      return "formal registration name must be printable ASCII";
  if (!RegistrationNames.insert(Name.str()).second)
    return "formal registration names must be globally unique";
  return std::string();
}

} // namespace

formal::EntryValidationResult
formal::validateEntryFunction(llvm::Module &M, llvm::StringRef EntryName) {
  llvm::Function *Entry = M.getFunction(EntryName);
  if (!Entry)
    return {nullptr, validateEntryProperties(false, false, false, 0, false)};

  llvm::FunctionType *Type = Entry->getFunctionType();
  EntryValidationError Error = validateEntryProperties(
      true, Entry->isDeclarationForLinker(), Type->getReturnType()->isVoidTy(),
      Type->getNumParams(), Type->isVarArg());
  if (Error == EntryValidationError::None &&
      (Entry->hasLocalLinkage() || !Entry->hasExactDefinition() ||
       Entry->isInterposable()))
    Error = EntryValidationError::InvalidLinkage;
  if (Error == EntryValidationError::None &&
      Entry->getCallingConv() != llvm::CallingConv::C)
    Error = EntryValidationError::InvalidCallingConvention;
  return {Entry, Error};
}

llvm::StringRef formal::preflightCodeName(PreflightCode Code) {
  switch (Code) {
  case PreflightCode::Valid:
    return "VALID";
  case PreflightCode::MalformedIR:
    return "MALFORMED_IR";
  case PreflightCode::MissingTargetTriple:
    return "MISSING_TARGET_TRIPLE";
  case PreflightCode::MissingDataLayout:
    return "MISSING_DATA_LAYOUT";
  case PreflightCode::UnsupportedTarget:
    return "UNSUPPORTED_TARGET";
  case PreflightCode::IncompatibleDataLayout:
    return "INCOMPATIBLE_DATA_LAYOUT";
  case PreflightCode::IncompatibleProducer:
    return "INCOMPATIBLE_PRODUCER";
  case PreflightCode::UnversionedProducer:
    return "UNVERSIONED_PRODUCER";
  case PreflightCode::HostToolMismatch:
    return "HOST_TOOL_MISMATCH";
  case PreflightCode::EntryNotFound:
    return "ENTRY_NOT_FOUND";
  case PreflightCode::EntryDeclarationOnly:
    return "ENTRY_DECLARATION_ONLY";
  case PreflightCode::EntryInvalidSignature:
    return "ENTRY_INVALID_SIGNATURE";
  case PreflightCode::EntryInvalidLinkage:
    return "ENTRY_INVALID_LINKAGE";
  case PreflightCode::EntryInvalidCallingConvention:
    return "ENTRY_INVALID_CALLING_CONVENTION";
  case PreflightCode::DynamicGlobalInitializers:
    return "DYNAMIC_GLOBAL_INITIALIZERS";
  case PreflightCode::ReachableDeclaration:
    return "REACHABLE_DECLARATION";
  case PreflightCode::ReachableInexactDefinition:
    return "REACHABLE_INEXACT_DEFINITION";
  case PreflightCode::IndirectCall:
    return "INDIRECT_CALL";
  case PreflightCode::InlineAssembly:
    return "INLINE_ASSEMBLY";
  case PreflightCode::AtomicInstruction:
    return "ATOMIC_INSTRUCTION";
  case PreflightCode::ScalableVector:
    return "SCALABLE_VECTOR";
  case PreflightCode::UnsupportedAddressSpace:
    return "UNSUPPORTED_ADDRESS_SPACE";
  case PreflightCode::UnsupportedCallingConvention:
    return "UNSUPPORTED_CALLING_CONVENTION";
  case PreflightCode::ExceptionHandling:
    return "EXCEPTION_HANDLING";
  case PreflightCode::FloatingPoint:
    return "FLOATING_POINT";
  case PreflightCode::FixedVector:
    return "FIXED_VECTOR";
  case PreflightCode::FreezeInstruction:
    return "FREEZE_INSTRUCTION";
  case PreflightCode::UnsupportedValueSemantics:
    return "UNSUPPORTED_VALUE_SEMANTICS";
  case PreflightCode::UnsupportedOpcode:
    return "UNSUPPORTED_OPCODE";
  case PreflightCode::UnsupportedIntrinsic:
    return "UNSUPPORTED_INTRINSIC";
  case PreflightCode::InvalidRegistration:
    return "INVALID_REGISTRATION";
  case PreflightCode::ResourceLimit:
    return "RESOURCE_LIMIT";
  }
  llvm_unreachable("unknown preflight code");
}

bool formal::PreflightResult::incomplete() const noexcept {
  switch (Code) {
  case PreflightCode::ReachableDeclaration:
  case PreflightCode::IndirectCall:
  case PreflightCode::InlineAssembly:
  case PreflightCode::AtomicInstruction:
  case PreflightCode::ScalableVector:
  case PreflightCode::UnsupportedAddressSpace:
  case PreflightCode::UnsupportedCallingConvention:
  case PreflightCode::ExceptionHandling:
  case PreflightCode::FloatingPoint:
  case PreflightCode::FixedVector:
  case PreflightCode::FreezeInstruction:
  case PreflightCode::UnsupportedValueSemantics:
  case PreflightCode::UnsupportedOpcode:
  case PreflightCode::UnsupportedIntrinsic:
    return true;
  default:
    return false;
  }
}

formal::PreflightResult
formal::validateModule(llvm::Module &M, llvm::StringRef EntryName,
                       unsigned ExpectedLLVMVersionMajor,
                       llvm::StringRef ExpectedLLVMVersion,
                       llvm::StringRef ExpectedLLVMRevision,
                       bool AllowUnversionedIR, PreflightLimits Limits) {
  std::string VerificationMessage;
  llvm::raw_string_ostream VerificationStream(VerificationMessage);
  if (llvm::verifyModule(M, &VerificationStream)) {
    VerificationStream.flush();
    return failure(PreflightCode::MalformedIR,
                   "LLVM Verifier rejected the module: " + VerificationMessage);
  }

  llvm::Triple Target(M.getTargetTriple());
  const llvm::DataLayout &Layout = M.getDataLayout();
  PreflightCode TargetCode = validateTargetProperties(
      !M.getTargetTriple().empty(), !M.getDataLayoutStr().empty(),
      Target.getArch() == llvm::Triple::x86_64 &&
          Target.getOS() == llvm::Triple::Linux,
      Layout.isLittleEndian(), Layout.getPointerSizeInBits(0));
  if (TargetCode == PreflightCode::MissingTargetTriple)
    return failure(TargetCode, "module target triple is empty");
  if (TargetCode == PreflightCode::MissingDataLayout)
    return failure(TargetCode, "module DataLayout is empty");
  if (TargetCode == PreflightCode::UnsupportedTarget)
    return failure(TargetCode, llvm::Twine("target triple '") +
                                   M.getTargetTriple() +
                                   "' is outside the pinned x86_64 Linux ABI");
  if (TargetCode == PreflightCode::IncompatibleDataLayout)
    return failure(TargetCode,
                   "DataLayout must describe a little-endian 64-bit default "
                   "address space");

  std::size_t FunctionCount = 0;
  std::size_t BasicBlockCount = 0;
  std::size_t InstructionCount = 0;
  for (const llvm::Function &Function : M) {
    if (!addWithinLimit(FunctionCount, 1, Limits.MaxFunctions) ||
        !addWithinLimit(BasicBlockCount, Function.size(),
                        Limits.MaxBasicBlocks))
      return failure(PreflightCode::ResourceLimit,
                     "module exceeds a configured preflight size limit");
    for (const llvm::BasicBlock &Block : Function)
      if (!addWithinLimit(InstructionCount, Block.size(),
                          Limits.MaxInstructions))
        return failure(PreflightCode::ResourceLimit,
                       "module exceeds a configured preflight size limit");
  }
  if (std::optional<PreflightResult> ResourceFailure =
          validateResourceBounds(M, Limits))
    return std::move(*ResourceFailure);

  std::vector<std::string> ProducerEntries = moduleProducerEntries(M);
  std::string Producer = joinProducerEntries(ProducerEntries);
  bool HasRecognizedProducer = false;
  for (const std::string &ProducerEntry : ProducerEntries) {
    std::optional<std::string> ProducerVersion = clangVersion(ProducerEntry);
    if (!ProducerVersion)
      continue;
    HasRecognizedProducer = true;
    std::optional<unsigned> ProducerMajor = versionMajor(*ProducerVersion);
    if ((ProducerMajor && *ProducerMajor != ExpectedLLVMVersionMajor) ||
        *ProducerVersion != ExpectedLLVMVersion ||
        !llvm::StringRef(ProducerEntry).contains(ExpectedLLVMRevision)) {
      PreflightResult Result = failure(
          PreflightCode::IncompatibleProducer,
          llvm::Twine("bitcode producer '") + ProducerEntry +
              "' does not match pinned LLVM/Clang '" + ExpectedLLVMVersion +
              "' revision '" + ExpectedLLVMRevision + "'");
      Result.Producer = Producer;
      return Result;
    }
  }
  if (!HasRecognizedProducer && !AllowUnversionedIR) {
    PreflightResult Result =
        failure(PreflightCode::UnversionedProducer,
                "module has no recognizable versioned Clang producer; pass "
                "-formal-allow-unversioned-ir only for explicitly trusted "
                "hand-written IR");
    Result.Producer = Producer;
    return Result;
  }

  EntryValidationResult EntryResult = validateEntryFunction(M, EntryName);
  if (EntryResult.Error != EntryValidationError::None) {
    PreflightCode Code = PreflightCode::EntryInvalidSignature;
    llvm::StringRef Detail = "must have type void ()";
    switch (EntryResult.Error) {
    case EntryValidationError::NotFound:
      Code = PreflightCode::EntryNotFound;
      Detail = "was not found";
      break;
    case EntryValidationError::DeclarationOnly:
      Code = PreflightCode::EntryDeclarationOnly;
      Detail = "has no definition";
      break;
    case EntryValidationError::InvalidSignature:
      break;
    case EntryValidationError::InvalidLinkage:
      Code = PreflightCode::EntryInvalidLinkage;
      Detail = "must have an exact, non-interposable definition";
      break;
    case EntryValidationError::InvalidCallingConvention:
      Code = PreflightCode::EntryInvalidCallingConvention;
      Detail = "must use the C calling convention";
      break;
    case EntryValidationError::None:
      llvm_unreachable("handled above");
    }
    PreflightResult Result = failure(
        Code, llvm::Twine("entry function '") + EntryName + "' " + Detail,
        EntryResult.Entry);
    Result.Producer = Producer;
    return Result;
  }

  if (const llvm::GlobalVariable *Ctors =
          M.getGlobalVariable("llvm.global_ctors")) {
    if (Ctors->hasInitializer() && !Ctors->getInitializer()->isNullValue()) {
      PreflightResult Result = failure(
          PreflightCode::DynamicGlobalInitializers,
          "llvm.global_ctors is present; M0 does not run dynamic initializers",
          EntryResult.Entry);
      Result.Producer = Producer;
      return Result;
    }
  }

  for (const llvm::GlobalVariable &Global : M.globals())
    if (Global.hasInitializer())
      if (std::optional<PreflightCode> Unsupported =
              unsupportedConstantTree(*Global.getInitializer()))
        return failure(*Unsupported,
                       llvm::Twine("global initializer '") + Global.getName() +
                           "' contains unsupported constant type or semantics",
                       EntryResult.Entry);

  std::vector<llvm::Function *> Worklist{EntryResult.Entry};
  std::set<llvm::Function *> Visited;
  std::set<std::string> RegistrationNames;
  while (!Worklist.empty()) {
    llvm::Function *Function = Worklist.back();
    Worklist.pop_back();
    if (!Visited.insert(Function).second)
      continue;
    VectorUse FunctionVectors = typeVectorUse(Function->getFunctionType());
    if (FunctionVectors != VectorUse::None)
      return failure(
          FunctionVectors == VectorUse::Scalable ? PreflightCode::ScalableVector
                                                 : PreflightCode::FixedVector,
          "reachable function uses an unsupported vector type", Function);
    if (typeUsesFloatingPoint(Function->getFunctionType()))
      return failure(PreflightCode::FloatingPoint,
                     "reachable function uses a floating-point type", Function);
    if (Function->getCallingConv() != llvm::CallingConv::C)
      return failure(PreflightCode::UnsupportedCallingConvention,
                     "reachable function uses a non-C calling convention",
                     Function);
    if (Function->getAddressSpace() != 0 ||
        typeUsesUnsupportedAddressSpace(Function->getFunctionType()))
      return failure(PreflightCode::UnsupportedAddressSpace,
                     "reachable function type uses a non-default address space",
                     Function);

    for (llvm::Instruction &Instruction : llvm::instructions(Function)) {
      if (std::optional<PreflightCode> Unsupported =
              unsupportedInstruction(Instruction))
        return failure(*Unsupported,
                       llvm::Twine("reachable instruction '") +
                           Instruction.getOpcodeName() +
                           "' is outside the explicit M0/M1 semantic manifest",
                       EntryResult.Entry);
      if (typeUsesUnsupportedAddressSpace(Instruction.getType()))
        return failure(PreflightCode::UnsupportedAddressSpace,
                       "reachable instruction uses a non-default address space",
                       EntryResult.Entry);
      for (const llvm::Use &Operand : Instruction.operands())
        if (typeUsesUnsupportedAddressSpace(Operand->getType()))
          return failure(PreflightCode::UnsupportedAddressSpace,
                         "reachable operand uses a non-default address space",
                         EntryResult.Entry);
      for (llvm::Type *HiddenType :
           opaquePointerSemanticTypes(Instruction))
        if (typeUsesUnsupportedAddressSpace(HiddenType))
          return failure(
              PreflightCode::UnsupportedAddressSpace,
              "reachable opaque-pointer semantic type uses a non-default "
              "address space",
              EntryResult.Entry);
      if (Instruction.isAtomic() || llvm::isa<llvm::FenceInst>(Instruction))
        return failure(PreflightCode::AtomicInstruction,
                       "reachable atomic instruction is outside M0/M1",
                       EntryResult.Entry);
      auto *Call = llvm::dyn_cast<llvm::CallBase>(&Instruction);
      if (!Call)
        continue;
      if (Call->getCallingConv() != llvm::CallingConv::C)
        return failure(PreflightCode::UnsupportedCallingConvention,
                       "reachable call site uses a non-C calling convention",
                       EntryResult.Entry);
      if (llvm::isa<llvm::InlineAsm>(Call->getCalledOperand()))
        return failure(PreflightCode::InlineAssembly,
                       "reachable inline assembly is unsupported",
                       EntryResult.Entry);
      llvm::Value *CalledTarget = Call->getCalledOperand();
      std::set<const llvm::GlobalAlias *> AliasChain;
      while (true) {
        CalledTarget = CalledTarget->stripPointerCasts();
        auto *Alias = llvm::dyn_cast<llvm::GlobalAlias>(CalledTarget);
        if (!Alias)
          break;
        if (!AliasChain.insert(Alias).second)
          return failure(PreflightCode::IndirectCall,
                         "reachable call contains a cyclic alias chain",
                         EntryResult.Entry);
        if (!Alias->hasExactDefinition() || Alias->isInterposable())
          return failure(
              PreflightCode::ReachableInexactDefinition,
              llvm::Twine("reachable function alias '") + Alias->getName() +
                  "' does not have an exact, non-interposable definition",
              EntryResult.Entry);
        CalledTarget = Alias->getAliasee();
      }
      auto *Callee = llvm::dyn_cast<llvm::Function>(
          CalledTarget->stripPointerCasts());
      if (!Callee)
        return failure(PreflightCode::IndirectCall,
                       "reachable indirect call cannot be resolved in M0",
                       EntryResult.Entry);
      if (isRegistrationBuiltin(Callee->getName())) {
        std::optional<std::string> RegistrationError =
            validateRegistrationCall(*Call, *Callee, RegistrationNames);
        if (RegistrationError && !RegistrationError->empty())
          return failure(PreflightCode::InvalidRegistration, *RegistrationError,
                         EntryResult.Entry);
        continue;
      }
      if (Callee->isIntrinsic()) {
        if (!acceptedIntrinsic(Callee->getIntrinsicID()))
          return failure(PreflightCode::UnsupportedIntrinsic,
                         llvm::Twine("reachable intrinsic '") +
                             Callee->getName() + "' is not modeled",
                         EntryResult.Entry);
        continue;
      }
      if (Callee->isDeclarationForLinker())
        return failure(PreflightCode::ReachableDeclaration,
                       llvm::Twine("reachable function '") + Callee->getName() +
                           "' has no definition",
                       EntryResult.Entry);
      if (!Callee->hasExactDefinition() || Callee->isInterposable())
        return failure(
            PreflightCode::ReachableInexactDefinition,
            llvm::Twine("reachable function '") + Callee->getName() +
                "' does not have an exact, non-interposable definition",
            EntryResult.Entry);
      Worklist.push_back(Callee);
    }
  }

  PreflightResult Result;
  Result.Entry = EntryResult.Entry;
  Result.Producer = Producer;
  if (!HasRecognizedProducer)
    Result.Warnings.emplace_back(
        "trusted unversioned/manual IR mode bypassed producer authentication");
  if (!M.getNamedMetadata("llvm.dbg.cu"))
    Result.Warnings.emplace_back(
        "module has no compile-unit debug information");
  return Result;
}
