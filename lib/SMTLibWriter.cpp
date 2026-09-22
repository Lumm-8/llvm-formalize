#include "formal/SMTLibWriter.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"

#include <algorithm>
#include <map>
#include <set>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace formal {
namespace {

void validateSMTName(const std::string &Name) {
  if (Name.empty())
    throw std::invalid_argument("SMT name must not be empty");
  if (Name.size() > 1024)
    throw std::length_error("SMT name exceeds the 1024-byte limit");
  for (unsigned char Character : Name)
    if (Character < 0x21 || Character > 0x7e || Character == '|' ||
        Character == '\\')
      throw std::invalid_argument(
          "SMT names must use printable ASCII excluding '|' and '\\'");
}

std::string apIntDecimal(const llvm::APInt &Value) {
  llvm::SmallString<64> Buffer;
  Value.toString(Buffer, 10, false);
  return Buffer.str().str();
}

struct PrintBudget {
  explicit PrintBudget(SMTLibWriterOptions Options) : Options(Options) {
    if (Options.MaxExpandedNodes == 0 || Options.MaxOutputBytes == 0)
      throw std::invalid_argument("SMT-LIB writer limits must be positive");
  }

  void consumeCollectedNode() {
    if (++CollectedNodes > Options.MaxExpandedNodes)
      throw std::length_error("SMT-LIB collection-node limit exceeded");
  }

  void consumeExpandedNode() {
    if (++ExpandedNodes > Options.MaxExpandedNodes)
      throw std::length_error("SMT-LIB expanded-node limit exceeded");
  }

  void consumeExpandedNodes(std::size_t Count) {
    if (Count > Options.MaxExpandedNodes -
                    std::min(ExpandedNodes, Options.MaxExpandedNodes))
      throw std::length_error("SMT-LIB expanded-node limit exceeded");
    ExpandedNodes += Count;
  }

  void reserveMinimumOutput(std::size_t Bytes) {
    if (Bytes > Options.MaxOutputBytes -
                    std::min(MinimumOutputBytes, Options.MaxOutputBytes))
      throw std::length_error("SMT-LIB output-size limit exceeded");
    MinimumOutputBytes += Bytes;
  }

  SMTLibWriterOptions Options;
  std::size_t CollectedNodes = 0;
  std::size_t ExpandedNodes = 0;
  std::size_t MinimumOutputBytes = 0;
};

class BoundedOutput final {
public:
  explicit BoundedOutput(std::size_t Limit) : Limit(Limit) {
    Text.reserve(std::min<std::size_t>(Limit, 4096));
  }

  void append(llvm::StringRef Value) {
    if (Value.size() > Limit - std::min(Text.size(), Limit))
      throw std::length_error("SMT-LIB output-size limit exceeded");
    Text.append(Value.data(), Value.size());
  }

  void append(char Value) {
    if (Text.size() == Limit)
      throw std::length_error("SMT-LIB output-size limit exceeded");
    Text.push_back(Value);
  }

  void append(unsigned Value) { append(std::to_string(Value)); }

  std::size_t remaining() const { return Limit - Text.size(); }

  std::string take() && { return std::move(Text); }

private:
  std::size_t Limit;
  std::string Text;
};

void printQuoted(BoundedOutput &Output, const std::string &Name) {
  validateSMTName(Name);
  Output.append('|');
  Output.append(Name);
  Output.append('|');
}

void printSort(BoundedOutput &Output, Sort Value) {
  if (Value.isBool()) {
    Output.append("Bool");
    return;
  }
  if (Value.isBitVector()) {
    Output.append("(_ BitVec ");
    Output.append(Value.bitWidth());
    Output.append(')');
    return;
  }
  Output.append("(Array (_ BitVec ");
  Output.append(Value.indexWidth());
  Output.append(") (_ BitVec 8))");
}

void appendCommentText(BoundedOutput &Output, const std::string &Text) {
  for (unsigned char Character : Text) {
    if (Character == '\n' || Character == '\r') {
      Output.append(' ');
      continue;
    }
    if (Character < 0x20 || Character > 0x7e)
      throw std::invalid_argument("SMT-LIB metadata must be printable ASCII");
    Output.append(static_cast<char>(Character));
  }
}

using DeclarationMap = std::map<std::string_view, Sort, std::less<>>;

struct Declarations {
  DeclarationMap Symbols;
  DeclarationMap Arrays;
  std::set<const Expr *> VisitedExpressions;
  std::set<const UpdateNode *> VisitedUpdates;
};

void printDeclarations(BoundedOutput &Output, const Declarations &Names) {
  for (const auto &[Name, ValueSort] : Names.Symbols) {
    Output.append("(declare-fun |");
    Output.append(Name);
    Output.append("| () ");
    printSort(Output, ValueSort);
    Output.append(")\n");
  }
  for (const auto &[Name, ArraySort] : Names.Arrays) {
    Output.append("(declare-fun |");
    Output.append(Name);
    Output.append("| () ");
    printSort(Output, ArraySort);
    Output.append(")\n");
  }
}

void validateDeclarations(const Declarations &Names) {
  for (const auto &[Name, ArraySort] : Names.Arrays) {
    static_cast<void>(ArraySort);
    if (Names.Symbols.count(Name))
      throw std::invalid_argument("array and scalar use the same SMT name");
  }
}

void addDeclaration(DeclarationMap &DeclarationsMap, const std::string &Name,
                    Sort ValueSort, PrintBudget &Budget) {
  auto Position = DeclarationsMap.find(Name);
  if (Position != DeclarationsMap.end()) {
    if (Position->second != ValueSort)
      throw std::invalid_argument("SMT name is used with multiple sorts");
    return;
  }
  Budget.reserveMinimumOutput(Name.size() + 64);
  DeclarationsMap.emplace_hint(Position, Name, ValueSort);
}

void collectExpression(const ExprRef &Expression, Declarations &Result,
                       PrintBudget &Budget);

void collectUpdates(const UpdateList &Updates, Declarations &Result,
                    PrintBudget &Budget) {
  addDeclaration(Result.Arrays, Updates.root()->name(), Updates.root()->sort(),
                 Budget);
  for (UpdateNodeRef Node = Updates.head(); Node; Node = Node->next()) {
    if (Result.VisitedUpdates.contains(Node.get()))
      break;
    // Every serialized update needs at least "(store ", two spaces and ')'.
    // Charge that lower bound before the set insertion allocates a node.
    Budget.reserveMinimumOutput(10);
    Budget.consumeCollectedNode();
    Result.VisitedUpdates.insert(Node.get());
    collectExpression(Node->index(), Result, Budget);
    collectExpression(Node->value(), Result, Budget);
  }
}

void collectExpression(const ExprRef &Expression, Declarations &Result,
                       PrintBudget &Budget) {
  if (!Expression)
    throw std::invalid_argument("cannot print a null expression");
  if (Result.VisitedExpressions.contains(Expression.get()))
    return;
  // Every unique reachable expression contributes at least one output byte.
  // Charge it before allocating the visited-set node.
  Budget.reserveMinimumOutput(1);
  Budget.consumeCollectedNode();
  Result.VisitedExpressions.insert(Expression.get());
  if (Expression->kind() == ExprKind::Symbol)
    addDeclaration(Result.Symbols, Expression->symbolName(), Expression->sort(),
                   Budget);
  if (Expression->kind() == ExprKind::Read)
    collectUpdates(Expression->updates(), Result, Budget);
  for (const ExprRef &Operand : Expression->operands())
    collectExpression(Operand, Result, Budget);
}

const char *binaryOperator(ExprKind Kind) {
  switch (Kind) {
  case ExprKind::Add:
    return "bvadd";
  case ExprKind::Sub:
    return "bvsub";
  case ExprKind::Mul:
    return "bvmul";
  case ExprKind::UDiv:
    return "bvudiv";
  case ExprKind::SDiv:
    return "bvsdiv";
  case ExprKind::URem:
    return "bvurem";
  case ExprKind::SRem:
    return "bvsrem";
  case ExprKind::And:
    return "bvand";
  case ExprKind::Or:
    return "bvor";
  case ExprKind::Xor:
    return "bvxor";
  case ExprKind::Shl:
    return "bvshl";
  case ExprKind::LShr:
    return "bvlshr";
  case ExprKind::AShr:
    return "bvashr";
  default:
    return nullptr;
  }
}

const char *comparisonOperator(ExprKind Kind) {
  switch (Kind) {
  case ExprKind::Eq:
    return "=";
  case ExprKind::Ult:
    return "bvult";
  case ExprKind::Ule:
    return "bvule";
  case ExprKind::Slt:
    return "bvslt";
  case ExprKind::Sle:
    return "bvsle";
  default:
    return nullptr;
  }
}

void printExpression(const ExprRef &Expression, PrintBudget &Budget,
                     BoundedOutput &Output);

void printArray(const UpdateList &Updates, PrintBudget &Budget,
                BoundedOutput &Output) {
  if (Updates.size() > Output.remaining() / 10)
    throw std::length_error("SMT-LIB output-size limit exceeded");
  Budget.consumeExpandedNodes(Updates.size());
  std::vector<UpdateNodeRef> Nodes;
  Nodes.reserve(Updates.size());
  for (UpdateNodeRef Node = Updates.head(); Node; Node = Node->next()) {
    Nodes.push_back(Node);
    Output.append("(store ");
  }
  printQuoted(Output, Updates.root()->name());
  for (auto Position = Nodes.rbegin(); Position != Nodes.rend(); ++Position) {
    Output.append(' ');
    printExpression((*Position)->index(), Budget, Output);
    Output.append(' ');
    printExpression((*Position)->value(), Budget, Output);
    Output.append(')');
  }
}

void printExpression(const ExprRef &Expression, PrintBudget &Budget,
                     BoundedOutput &Output) {
  Budget.consumeExpandedNode();
  switch (Expression->kind()) {
  case ExprKind::Constant: {
    if (Expression->sort().isBool()) {
      Output.append(Expression->constantValue().isZero() ? "false" : "true");
      return;
    }
    Output.append("(_ bv");
    // APInt::toString allocates its complete result.  Reject values whose
    // decimal representation cannot fit before asking APInt to allocate it.
    const std::size_t MaximumDecimalDigits =
        (static_cast<std::size_t>(Expression->width()) * 30103U + 99999U) /
        100000U;
    if (MaximumDecimalDigits > Output.remaining())
      throw std::length_error("SMT-LIB output-size limit exceeded");
    Output.append(apIntDecimal(Expression->constantValue()));
    Output.append(' ');
    Output.append(Expression->width());
    Output.append(')');
    return;
  }
  case ExprKind::Symbol:
    printQuoted(Output, Expression->symbolName());
    return;
  case ExprKind::Read:
    Output.append("(select ");
    printArray(Expression->updates(), Budget, Output);
    Output.append(' ');
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(')');
    return;
  case ExprKind::Not:
    Output.append("(not ");
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(')');
    return;
  case ExprKind::Select:
    Output.append("(ite ");
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(' ');
    printExpression(Expression->operand(1), Budget, Output);
    Output.append(' ');
    printExpression(Expression->operand(2), Budget, Output);
    Output.append(')');
    return;
  case ExprKind::Concat:
    Output.append("(concat ");
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(' ');
    printExpression(Expression->operand(1), Budget, Output);
    Output.append(')');
    return;
  case ExprKind::Extract:
    Output.append("((_ extract ");
    Output.append(Expression->offset() + Expression->width() - 1);
    Output.append(' ');
    Output.append(Expression->offset());
    Output.append(") ");
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(')');
    return;
  case ExprKind::ZExt:
  case ExprKind::SExt:
    Output.append(Expression->kind() == ExprKind::ZExt ? "((_ zero_extend "
                                                       : "((_ sign_extend ");
    Output.append(Expression->width() - Expression->operand(0)->width());
    Output.append(") ");
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(')');
    return;
  case ExprKind::Trunc:
    Output.append("((_ extract ");
    Output.append(Expression->width() - 1);
    Output.append(" 0) ");
    printExpression(Expression->operand(0), Budget, Output);
    Output.append(')');
    return;
  default:
    break;
  }

  const char *Operator = binaryOperator(Expression->kind());
  if (!Operator)
    Operator = comparisonOperator(Expression->kind());
  if (!Operator)
    throw std::invalid_argument("unsupported Expr kind in SMT-LIB writer");
  Output.append('(');
  Output.append(Operator);
  Output.append(' ');
  printExpression(Expression->operand(0), Budget, Output);
  Output.append(' ');
  printExpression(Expression->operand(1), Budget, Output);
  Output.append(')');
}

} // namespace

std::string SMTLibWriter::write(const ExprRef &Expression,
                                std::string ResultName, bool IncludeCheckSat,
                                SMTLibWriterOptions Options) {
  if (!Expression)
    throw std::invalid_argument("cannot write a null expression");
  validateSMTName(ResultName);
  PrintBudget Budget(Options);
  Budget.reserveMinimumOutput(ResultName.size() + 96);

  Declarations Names;
  collectExpression(Expression, Names, Budget);
  if (Names.Symbols.count(ResultName) || Names.Arrays.count(ResultName))
    throw std::invalid_argument("result name conflicts with an input symbol");
  validateDeclarations(Names);

  BoundedOutput Output(Options.MaxOutputBytes);
  Output.append("(set-logic ");
  Output.append(Names.Arrays.empty() ? "QF_BV" : "QF_AUFBV");
  Output.append(")\n");
  printDeclarations(Output, Names);
  Output.append("(define-fun ");
  printQuoted(Output, ResultName);
  Output.append(" () ");
  printSort(Output, Expression->sort());
  Output.append(' ');
  printExpression(Expression, Budget, Output);
  Output.append(")\n");
  if (IncludeCheckSat)
    Output.append("(check-sat)\n");
  return std::move(Output).take();
}

std::string SMTLibWriter::writeQuery(const SMTLibQuery &Query,
                                     SMTLibWriterOptions Options) {
  PrintBudget Budget(Options);
  Declarations Names;
  for (std::size_t Index = 0; Index < Query.Constraints.size(); ++Index) {
    const ExprRef &Constraint = Query.Constraints[Index];
    if (!Constraint)
      throw std::invalid_argument("cannot write a null query constraint");
    collectExpression(Constraint, Names, Budget);
  }
  if (Query.Target) {
    if (Query.AssertTarget && !Query.Target->sort().isBool())
      throw std::invalid_argument("asserted query target must have Bool sort");
    collectExpression(Query.Target, Names, Budget);
  } else if (Query.AssertTarget || Query.NegateTarget) {
    throw std::invalid_argument("query target flags require a target");
  }
  for (const SMTLibArrayRequest &Request : Query.ArrayRequests)
    if (Request.Root)
      addDeclaration(Names.Arrays, Request.Root->name(),
                     Request.Root->sort(), Budget);
  validateDeclarations(Names);

  BoundedOutput Output(Options.MaxOutputBytes);
  Output.append("(set-logic ");
  Output.append(Names.Arrays.empty() ? "QF_BV" : "QF_AUFBV");
  Output.append(")\n");
  for (const auto &[Key, Value] : Query.Metadata) {
    Output.append("; ");
    appendCommentText(Output, Key);
    Output.append(": ");
    appendCommentText(Output, Value);
    Output.append('\n');
  }
  for (std::size_t Index = 0; Index < Query.ArrayRequests.size(); ++Index) {
    const SMTLibArrayRequest &Request = Query.ArrayRequests[Index];
    Output.append("; array-request-");
    Output.append(std::to_string(Index));
    Output.append(": name=");
    if (Request.Root) {
      appendCommentText(Output, Request.Root->name());
      Output.append(" index-width=");
      Output.append(Request.Root->sort().indexWidth());
    } else {
      Output.append("<null> index-width=<invalid>");
    }
    Output.append(" byte-count=");
    Output.append(std::to_string(Request.ByteCount));
    Output.append('\n');
  }
  printDeclarations(Output, Names);
  for (std::size_t Index = 0; Index < Query.Constraints.size(); ++Index) {
    const ExprRef &Constraint = Query.Constraints[Index];
    if (Constraint->sort().isBool()) {
      Output.append("(assert ");
      printExpression(Constraint, Budget, Output);
      Output.append(")\n");
      continue;
    }
    // There is no Boolean semantics for a malformed scalar constraint.  Keep
    // its exact term in the standalone diagnostic while making the file
    // deterministically non-satisfiable so it can never be mistaken for a
    // usable solver query.
    Output.append("; invalid-constraint-sort: ");
    Output.append(static_cast<unsigned>(Index));
    Output.append("\n(define-fun |__formal_invalid_constraint_");
    Output.append(static_cast<unsigned>(Index));
    Output.append("| () ");
    printSort(Output, Constraint->sort());
    Output.append(' ');
    printExpression(Constraint, Budget, Output);
    Output.append(")\n(assert false)\n");
  }
  if (Query.Target) {
    Output.append("; query-target: ");
    Output.append(Query.AssertTarget ? (Query.NegateTarget ? "assert-not"
                                                          : "assert")
                                    : "value-only");
    Output.append('\n');
    if (Query.AssertTarget) {
      Output.append("(assert ");
      if (Query.NegateTarget)
        Output.append("(not ");
      printExpression(Query.Target, Budget, Output);
      if (Query.NegateTarget)
        Output.append(')');
      Output.append(")\n");
    } else {
      std::string TargetName = "__formal_query_target";
      while (Names.Symbols.count(TargetName) || Names.Arrays.count(TargetName))
        TargetName.push_back('_');
      Output.append("(define-fun ");
      printQuoted(Output, TargetName);
      Output.append(" () ");
      printSort(Output, Query.Target->sort());
      Output.append(' ');
      printExpression(Query.Target, Budget, Output);
      Output.append(")\n");
    }
  } else {
    Output.append("; query-target: none\n");
  }
  Output.append("(check-sat)\n");
  if (Query.IncludeGetModel)
    Output.append("(get-model)\n");
  return std::move(Output).take();
}

} // namespace formal
