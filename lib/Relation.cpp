#include "formal/Relation.h"

#include "formal/SMTLibWriter.h"

#include <set>
#include <sstream>
#include <stdexcept>

namespace formal {
namespace {

bool collectClosed(const ExprRef &Expression,
                   const std::set<std::string> &Allowed,
                   std::set<const Expr *> &Visited,
                   std::set<std::string> &Used) {
  if (!Expression || !Visited.insert(Expression.get()).second)
    return static_cast<bool>(Expression);
  if (Expression->kind() == ExprKind::Symbol) {
    if (!Allowed.contains(Expression->symbolName()) ||
        !Expression->sort().isBitVector() || Expression->width() != 8)
      return false;
    Used.insert(Expression->symbolName());
  }
  // A read from a root array has unconstrained bytes unless a separate
  // quantified elimination step proves otherwise. Reject it before export.
  if (Expression->kind() == ExprKind::Read)
    return false;
  for (const ExprRef &Operand : Expression->operands())
    if (!collectClosed(Operand, Allowed, Visited, Used))
      return false;
  return true;
}

ExprRef andExpr(ExprContext &Expressions, ExprRef Left, ExprRef Right) {
  return Expressions.select(Left, Right, Expressions.boolean(false));
}

ExprRef orExpr(ExprContext &Expressions, ExprRef Left, ExprRef Right) {
  return Expressions.select(Left, Expressions.boolean(true), Right);
}

std::string descriptorJSON(const PathResult::RegisteredRegion &Region,
                           std::size_t Ordinal) {
  std::ostringstream Out;
  Out << "{\"name\":\"" << Region.Name << "\",\"size\":" << Region.Size
      << ",\"ordinal\":" << Ordinal << "}";
  return Out.str();
}

} // namespace

static RelationArtifact exportRelationImpl(const ExecutionResult &Result,
                                           ExprContext &Expressions) {
  RelationArtifact Artifact;
  if (Result.Status == ExecutionStatus::InternalError) {
    Artifact.Reason = "execution has an internal error";
    return Artifact;
  }
  const PathResult *Reference = nullptr;
  std::vector<const PathResult *> NormalPaths;
  bool UnderApproximation = Result.Status == ExecutionStatus::Unknown;
  for (const PathResult &Path : Result.Paths) {
    if (Path.Status == PathStatus::InternalError) {
      Artifact.Reason = "internal-error path cannot be exported";
      return Artifact;
    }
    if (Path.Status == PathStatus::Incomplete) {
      UnderApproximation = true;
      continue;
    }
    if (Path.Status == PathStatus::Error)
      continue;
    if (!Reference)
      Reference = &Path;
    else {
      bool Same = Path.Regions.size() == Reference->Regions.size();
      if (Same) {
        for (std::size_t Index = 0; Index < Path.Regions.size(); ++Index) {
          const auto &A = Path.Regions[Index];
          const auto &B = Reference->Regions[Index];
          if (A.Input != B.Input || A.Name != B.Name || A.Size != B.Size)
            Same = false;
        }
      }
      if (!Same) {
        UnderApproximation = true;
        continue;
      }
    }
    NormalPaths.push_back(&Path);
  }
  if (UnderApproximation)
    Artifact.Reason = "one or more feasible paths are incomplete";
  if (!Reference) {
    Artifact.SMTLib = SMTLibWriter::write(Expressions.boolean(false),
                                          "formal_relation", false);
    Artifact.SMTLib += "(assert |formal_relation|)\n(check-sat)\n";
    if (Result.HasUnwindingAssumption)
      Artifact.SMTLib.insert(0, "; formal-unwinding-assumption: true\n");
    Artifact.JSON =
        "{\"format\":\"formal-m5-relation-v1\","
        "\"status\":\"" +
        std::string(UnderApproximation ? "UNDER_APPROXIMATION" : "COMPLETE") +
        "\",\"paths\":0," +
        std::string(Result.HasUnwindingAssumption
                        ? "\"unwinding_assumption\":true,"
                        : "") +
        "\"inputs\":[],\"outputs\":[]}\n";
    Artifact.UnderApproximation = UnderApproximation;
    Artifact.Complete = !UnderApproximation;
    return Artifact;
  }
  std::set<std::string> Allowed;
  for (std::size_t Ordinal = 0; Ordinal < Reference->Regions.size();
       ++Ordinal) {
    const auto &Region = Reference->Regions[Ordinal];
    if (!Region.Input)
      continue;
    for (std::uint64_t Index = 0; Index < Region.Size; ++Index)
      Allowed.insert("input." + Region.Name + "." + std::to_string(Index));
  }
  ExprRef Relation = Expressions.boolean(false);
  std::set<std::string> Used;
  for (const PathResult *Path : NormalPaths) {
    ExprRef Clause = Expressions.boolean(true);
    std::set<const Expr *> Visited;
    for (const ExprRef &Constraint : Path->Constraints) {
      if (!collectClosed(Constraint, Allowed, Visited, Used)) {
        Artifact.Reason = "undeclared internal symbol in path constraint";
        return Artifact;
      }
      Clause = andExpr(Expressions, Clause, Constraint);
    }
    for (const auto &Region : Path->Regions) {
      if (Region.Input)
        continue;
      if (Region.Bytes.size() != Region.Size) {
        Artifact.Reason = "output snapshot has incomplete byte coverage";
        return Artifact;
      }
      for (std::uint64_t Index = 0; Index < Region.Size; ++Index) {
        if (!collectClosed(Region.Bytes[Index], Allowed, Visited, Used)) {
          Artifact.Reason = "undeclared internal symbol in output snapshot";
          return Artifact;
        }
        ExprRef Symbol = Expressions.symbol(8, "output." + Region.Name + "." +
                                                   std::to_string(Index));
        Clause = andExpr(Expressions, Clause,
                         Expressions.eq(Symbol, Region.Bytes[Index]));
      }
    }
    Relation = orExpr(Expressions, Relation, Clause);
  }
  try {
    Artifact.SMTLib = SMTLibWriter::write(Relation, "formal_relation", false);
    std::string MissingDeclarations;
    for (const std::string &Name : Allowed) {
      if (!Used.contains(Name))
        MissingDeclarations += "(declare-fun |" + Name + "| () (_ BitVec 8))\n";
    }
    if (!MissingDeclarations.empty()) {
      std::size_t AfterLogic = Artifact.SMTLib.find('\n');
      if (AfterLogic == std::string::npos)
        throw std::logic_error("SMT writer omitted set-logic line");
      Artifact.SMTLib.insert(AfterLogic + 1, MissingDeclarations);
    }
    Artifact.SMTLib += "(assert |formal_relation|)\n(check-sat)\n";
    if (Result.HasUnwindingAssumption)
      Artifact.SMTLib.insert(0, "; formal-unwinding-assumption: true\n");
  } catch (const std::exception &Error) {
    Artifact.Reason =
        std::string("relation SMT export failed: ") + Error.what();
    return Artifact;
  }
  std::ostringstream JSON;
  JSON << "{\"format\":\"formal-m5-relation-v1\",\"status\":\""
       << (UnderApproximation ? "UNDER_APPROXIMATION" : "COMPLETE")
       << "\",\"paths\":" << NormalPaths.size();
  if (Result.HasUnwindingAssumption)
    JSON << ",\"unwinding_assumption\":true";
  JSON << ",\"inputs\":[";
  bool First = true;
  for (std::size_t Ordinal = 0; Ordinal < Reference->Regions.size();
       ++Ordinal) {
    const auto &Region = Reference->Regions[Ordinal];
    if (!Region.Input)
      continue;
    if (!First)
      JSON << ',';
    First = false;
    JSON << descriptorJSON(Region, Ordinal);
  }
  JSON << "],\"outputs\":[";
  First = true;
  for (std::size_t Ordinal = 0; Ordinal < Reference->Regions.size();
       ++Ordinal) {
    const auto &Region = Reference->Regions[Ordinal];
    if (Region.Input)
      continue;
    if (!First)
      JSON << ',';
    First = false;
    JSON << descriptorJSON(Region, Ordinal);
  }
  JSON << "]}\n";
  Artifact.JSON = JSON.str();
  Artifact.UnderApproximation = UnderApproximation;
  Artifact.Complete = !UnderApproximation;
  return Artifact;
}

RelationArtifact exportRelation(const ExecutionResult &Result,
                                ExprContext &Expressions) {
  try {
    return exportRelationImpl(Result, Expressions);
  } catch (const std::exception &Error) {
    RelationArtifact Artifact;
    Artifact.Reason =
        std::string("relation construction failed: ") + Error.what();
    return Artifact;
  }
}

bool hasClosedRegisteredErrorPath(const PathResult &Path) {
  if (Path.Status != PathStatus::Error || !Path.ErrorModel)
    return false;
  std::set<std::string> Allowed;
  for (const auto &Region : Path.Regions) {
    if (!Region.Input)
      continue;
    for (std::uint64_t Index = 0; Index < Region.Size; ++Index)
      Allowed.insert("input." + Region.Name + "." + std::to_string(Index));
  }
  std::set<const Expr *> Visited;
  std::set<std::string> Used;
  for (const ExprRef &Constraint : Path.Constraints)
    if (!collectClosed(Constraint, Allowed, Visited, Used))
      return false;
  return true;
}

std::optional<std::string>
exportCounterexampleJSON(const ExecutionResult &Result) {
  for (const PathResult &Path : Result.Paths) {
    if (!hasClosedRegisteredErrorPath(Path))
      continue;
    std::ostringstream JSON;
    JSON << "{\"format\":\"formal-m5-counterexample-v1\","
            "\"state_id\":"
         << Path.StateId << ",\"reason\":\"" << toReasonCode(Path.ErrorReason)
         << "\",\"inputs\":[";
    bool FirstRegion = true;
    for (const auto &Region : Path.Regions) {
      if (!Region.Input)
        continue;
      if (!FirstRegion)
        JSON << ',';
      FirstRegion = false;
      JSON << "{\"name\":\"" << Region.Name << "\",\"bytes\":[";
      for (std::uint64_t Index = 0; Index < Region.Size; ++Index) {
        if (Index)
          JSON << ',';
        const std::string Symbol =
            "input." + Region.Name + "." + std::to_string(Index);
        unsigned Byte = 0;
        for (const auto &Scalar : Path.ErrorModel->Scalars) {
          if (Scalar.Name != Symbol)
            continue;
          if (!Scalar.ValueSort.isBitVector() ||
              Scalar.ValueSort.bitWidth() != 8)
            return std::nullopt;
          Byte = static_cast<unsigned>(Scalar.Value.getZExtValue());
          break;
        }
        JSON << Byte;
      }
      JSON << "]}";
    }
    JSON << "]}\n";
    return JSON.str();
  }
  return std::nullopt;
}

} // namespace formal
