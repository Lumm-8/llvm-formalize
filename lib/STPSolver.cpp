#include "formal/Solver.h"
#include "formal/SMTLibWriter.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include <stp/c_interface.h>

#include <algorithm>
#include <atomic>
#include <climits>
#include <cstdlib>
#include <iomanip>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <system_error>
#include <thread>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include <cerrno>
#include <csignal>
#include <fcntl.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define FORMAL_ADDRESS_SANITIZER 1
#endif
#endif
#if defined(__SANITIZE_ADDRESS__) && !defined(FORMAL_ADDRESS_SANITIZER)
#define FORMAL_ADDRESS_SANITIZER 1
#endif

#ifndef FORMAL_STP_VERSION
#define FORMAL_STP_VERSION "unknown"
#endif

#ifndef FORMAL_STP_REVISION
#define FORMAL_STP_REVISION "unknown"
#endif

#ifndef FORMAL_STP_COMPILER
#define FORMAL_STP_COMPILER "unknown"
#endif

#ifndef FORMAL_STP_WORKER_EXECUTABLE
#define FORMAL_STP_WORKER_EXECUTABLE "FormalSTPWorker"
#endif

namespace formal {
namespace {

std::atomic<std::uint64_t> NextQueryId{1};
std::atomic<std::uint64_t> NextContextId{1};
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
std::atomic<int> WorkerTestMode{0};
std::atomic<bool> WorkerTestClockEnabled{false};
std::atomic<std::int64_t> WorkerTestClockNanoseconds{0};

std::chrono::steady_clock::time_point solverClockNow() {
  using Clock = std::chrono::steady_clock;
  if (!WorkerTestClockEnabled.load())
    return Clock::now();
  const auto Ticks = std::chrono::duration_cast<Clock::duration>(
      std::chrono::nanoseconds(WorkerTestClockNanoseconds.load()));
  return Clock::time_point(Ticks);
}

void advanceSolverTestClock(std::chrono::steady_clock::duration Amount) {
  const auto Nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(
                               Amount)
                               .count();
  std::int64_t Current = WorkerTestClockNanoseconds.load();
  while (true) {
    const std::int64_t Next =
        Nanoseconds > 0 && Current > INT64_MAX - Nanoseconds
            ? INT64_MAX
            : Current + Nanoseconds;
    if (WorkerTestClockNanoseconds.compare_exchange_weak(Current, Next))
      return;
  }
}
#else
std::chrono::steady_clock::time_point solverClockNow() {
  return std::chrono::steady_clock::now();
}
#endif

constexpr int WorkerRequestReadFailureExitCode = 123;
constexpr int WorkerInternalExceptionExitCode = 124;
constexpr int WorkerResponseWriteFailureExitCode = 125;
constexpr int WorkerExecFailureExitCode = 126;

enum class QueryKind { Check, MayBeTrue, MustBeTrue, GetValue, InitialValues };

const char *queryKindName(QueryKind Kind) {
  switch (Kind) {
  case QueryKind::Check:
    return "check";
  case QueryKind::MayBeTrue:
    return "may-be-true";
  case QueryKind::MustBeTrue:
    return "must-be-true";
  case QueryKind::GetValue:
    return "get-value";
  case QueryKind::InitialValues:
    return "get-initial-values";
  }
  return "unknown";
}

std::string apIntDecimal(const llvm::APInt &Value) {
  llvm::SmallString<64> Buffer;
  Value.toString(Buffer, 10, false);
  return Buffer.str().str();
}

std::string encodedName(const char *Prefix, const std::string &Name) {
  static constexpr char Hex[] = "0123456789abcdef";
  std::string Result(Prefix);
  Result.reserve(Result.size() + Name.size() * 2);
  for (unsigned char Character : Name) {
    Result.push_back(Hex[Character >> 4]);
    Result.push_back(Hex[Character & 0xf]);
  }
  return Result;
}

class VCHandle final {
public:
  VCHandle() : Handle(vc_createValidityChecker()) {
    if (!Handle)
      throw std::runtime_error("STP failed to create a validity checker");
  }
  ~VCHandle() {
    if (Handle)
      vc_Destroy(Handle);
  }
  VCHandle(const VCHandle &) = delete;
  VCHandle &operator=(const VCHandle &) = delete;
  ::VC get() const noexcept { return Handle; }

private:
  ::VC Handle = nullptr;
};

class QueryContext final {
public:
  explicit QueryContext(std::uint64_t ContextId) : ContextId(ContextId) {
    // STP 2.3.3 only registers type handles for EXPRDELETE cleanup while the
    // ordinary expression constructors still return caller-owned wrappers.
    // Disable the mixed ownership mode and manage every public C API handle
    // uniformly in this context.
    vc_setInterfaceFlags(Checker.get(), EXPRDELETE, 0);
    if (!vc_supportsMinisat(Checker.get()) ||
        !vc_useMinisat(Checker.get()) ||
        !vc_isUsingMinisat(Checker.get()))
      throw std::runtime_error("STP MiniSat backend is unavailable");
  }

  ~QueryContext() {
    for (auto Position = OwnedHandles.rbegin(); Position != OwnedHandles.rend();
         ++Position)
      vc_DeleteExpr(*Position);
  }

  QueryContext(const QueryContext &) = delete;
  QueryContext &operator=(const QueryContext &) = delete;

  ::Expr own(::Expr Handle) {
    if (!Handle)
      throw std::runtime_error("STP returned a null expression handle");
    if (OwnedHandleSet.insert(Handle).second)
      OwnedHandles.push_back(Handle);
    return Handle;
  }

  ::Expr translate(const ExprRef &Expression) {
    if (!Expression)
      throw std::invalid_argument("cannot translate a null expression");
    if (Expression->sort().isBitVector())
      checkedWidth(Expression->sort().bitWidth(), "bitvector expression");
    auto Existing = ExpressionCache.find(Expression.get());
    if (Existing != ExpressionCache.end()) {
      ++Statistics.ExpressionCacheHits;
      return Existing->second;
    }

    ::Expr Result = nullptr;
    switch (Expression->kind()) {
    case ExprKind::Constant:
      if (Expression->sort().isBool())
        Result = Expression->constantValue().isZero()
                     ? vc_falseExpr(Checker.get())
                     : vc_trueExpr(Checker.get());
      else {
        std::string Decimal = apIntDecimal(Expression->constantValue());
        Result = vc_bvConstExprFromDecStr(Checker.get(),
                                          checkedWidth(Expression->width(),
                                                       "constant"),
                                          Decimal.c_str());
      }
      break;
    case ExprKind::Symbol:
      Result = translateSymbol(Expression);
      break;
    case ExprKind::Read:
      Result = translateRead(Expression);
      break;
    case ExprKind::Add:
      Result = vc_bvPlusExpr(Checker.get(), Expression->width(),
                             translate(Expression->operand(0)),
                             translate(Expression->operand(1)));
      break;
    case ExprKind::Sub:
      Result = vc_bvMinusExpr(Checker.get(), Expression->width(),
                              translate(Expression->operand(0)),
                              translate(Expression->operand(1)));
      break;
    case ExprKind::Mul:
      Result = vc_bvMultExpr(Checker.get(), Expression->width(),
                             translate(Expression->operand(0)),
                             translate(Expression->operand(1)));
      break;
    case ExprKind::UDiv:
      Result = vc_bvDivExpr(Checker.get(), Expression->width(),
                            translate(Expression->operand(0)),
                            translate(Expression->operand(1)));
      break;
    case ExprKind::SDiv:
      Result = vc_sbvDivExpr(Checker.get(), Expression->width(),
                             translate(Expression->operand(0)),
                             translate(Expression->operand(1)));
      break;
    case ExprKind::URem:
      Result = vc_bvRemExpr(Checker.get(), Expression->width(),
                            translate(Expression->operand(0)),
                            translate(Expression->operand(1)));
      break;
    case ExprKind::SRem:
      Result = vc_sbvRemExpr(Checker.get(), Expression->width(),
                             translate(Expression->operand(0)),
                             translate(Expression->operand(1)));
      break;
    case ExprKind::And:
      Result = vc_bvAndExpr(Checker.get(), translate(Expression->operand(0)),
                            translate(Expression->operand(1)));
      break;
    case ExprKind::Or:
      Result = vc_bvOrExpr(Checker.get(), translate(Expression->operand(0)),
                           translate(Expression->operand(1)));
      break;
    case ExprKind::Xor:
      Result = vc_bvXorExpr(Checker.get(), translate(Expression->operand(0)),
                            translate(Expression->operand(1)));
      break;
    case ExprKind::Shl:
      Result = vc_bvLeftShiftExprExpr(
          Checker.get(), Expression->width(), translate(Expression->operand(0)),
          translate(Expression->operand(1)));
      break;
    case ExprKind::LShr:
      Result = vc_bvRightShiftExprExpr(
          Checker.get(), Expression->width(), translate(Expression->operand(0)),
          translate(Expression->operand(1)));
      break;
    case ExprKind::AShr:
      Result = vc_bvSignedRightShiftExprExpr(
          Checker.get(), Expression->width(), translate(Expression->operand(0)),
          translate(Expression->operand(1)));
      break;
    case ExprKind::Eq:
      Result = vc_eqExpr(Checker.get(), translate(Expression->operand(0)),
                         translate(Expression->operand(1)));
      break;
    case ExprKind::Ult:
      Result = vc_bvLtExpr(Checker.get(), translate(Expression->operand(0)),
                           translate(Expression->operand(1)));
      break;
    case ExprKind::Ule:
      Result = vc_bvLeExpr(Checker.get(), translate(Expression->operand(0)),
                           translate(Expression->operand(1)));
      break;
    case ExprKind::Slt:
      Result = vc_sbvLtExpr(Checker.get(), translate(Expression->operand(0)),
                            translate(Expression->operand(1)));
      break;
    case ExprKind::Sle:
      Result = vc_sbvLeExpr(Checker.get(), translate(Expression->operand(0)),
                            translate(Expression->operand(1)));
      break;
    case ExprKind::Not:
      Result = vc_notExpr(Checker.get(), translate(Expression->operand(0)));
      break;
    case ExprKind::Select:
      Result = vc_iteExpr(Checker.get(), translate(Expression->operand(0)),
                          translate(Expression->operand(1)),
                          translate(Expression->operand(2)));
      break;
    case ExprKind::Concat:
      Result = vc_bvConcatExpr(Checker.get(),
                               translate(Expression->operand(0)),
                               translate(Expression->operand(1)));
      break;
    case ExprKind::Extract:
      Result = vc_bvExtract(Checker.get(), translate(Expression->operand(0)),
                            checkedExtractHigh(Expression),
                            checkedWidth(Expression->offset(),
                                         "extract low bit"));
      break;
    case ExprKind::ZExt: {
      unsigned Added = Expression->width() - Expression->operand(0)->width();
      llvm::APInt Zero(Added, 0);
      std::string Decimal = apIntDecimal(Zero);
      ::Expr Prefix = own(vc_bvConstExprFromDecStr(
          Checker.get(), checkedWidth(Added, "zero-extension prefix"),
          Decimal.c_str()));
      Result = vc_bvConcatExpr(Checker.get(), Prefix,
                               translate(Expression->operand(0)));
      break;
    }
    case ExprKind::SExt:
      Result = vc_bvSignExtend(Checker.get(),
                               translate(Expression->operand(0)),
                               checkedWidth(Expression->width(),
                                            "sign-extension result"));
      break;
    case ExprKind::Trunc:
      Result = vc_bvExtract(Checker.get(), translate(Expression->operand(0)),
                            checkedWidth(Expression->width() - 1,
                                         "truncation high bit"),
                            0);
      break;
    }
    Result = own(Result);
    ExpressionCache.emplace(Expression.get(), Result);
    ++Statistics.ExpressionTranslations;
    return Result;
  }

  ::Expr translateArray(const ArrayRef &Root) {
    if (!Root)
      throw std::invalid_argument("cannot translate a null array");
    checkedWidth(Root->sort().indexWidth(), "array index");
    auto Existing = ArrayCache.find(Root.get());
    if (Existing != ArrayCache.end()) {
      ++Statistics.ArrayCacheHits;
      return Existing->second;
    }
    registerName(Root->name(), Root->sort());
    const std::string Key = sortKey(Root->name(), Root->sort());
    auto Named = NamedArrays.find(Key);
    ::Expr Result = nullptr;
    if (Named != NamedArrays.end()) {
      throw std::invalid_argument(
          "query contains distinct arrays with the same name and sort: " +
          Root->name());
    } else {
      ::Type Index = own(vc_bvType(
          Checker.get(), checkedWidth(Root->sort().indexWidth(), "array index")));
      ::Type Byte = own(vc_bvType(Checker.get(), 8));
      ::Type ArrayType = own(vc_arrayType(Checker.get(), Index, Byte));
      std::string Name = encodedName("formal_array_", Root->name());
      Result = vc_varExpr(Checker.get(), Name.c_str(), ArrayType);
      NamedArrays.emplace(Key, Result);
    }
    Result = own(Result);
    ArrayCache.emplace(Root.get(), Result);
    ArrayRoots.push_back(Root);
    ++Statistics.ArrayTranslations;
    return Result;
  }

  llvm::APInt modelValue(::Expr Term, Sort ValueSort) {
    ::Expr Value = own(vc_getCounterExample(Checker.get(), Term));
    if (ValueSort.isBool()) {
      int Boolean = vc_isBool(Value);
      if (Boolean != 0 && Boolean != 1)
        throw std::runtime_error("STP returned a non-Boolean model value");
      return llvm::APInt(1, static_cast<std::uint64_t>(Boolean));
    }
    char *Buffer = nullptr;
    unsigned long Length = 0;
    vc_printBVBitStringToBuffer(Value, &Buffer, &Length);
    if (!Buffer)
      throw std::runtime_error("STP returned no bitvector model buffer");
    if (Length > 0 && Buffer[Length - 1] == '\0')
      --Length;
    std::string Bits(Buffer, Length);
    std::free(Buffer);
    if (Bits.size() != ValueSort.bitWidth() ||
        Bits.find_first_not_of("01") != std::string::npos)
      throw std::runtime_error("STP returned an invalid bitvector model value");
    return llvm::APInt(ValueSort.bitWidth(), Bits, 2);
  }

  Model extractModel(const std::vector<ArrayModelRequest> &Requests,
                     std::vector<InitialArrayValue> &InitialValues,
                     std::size_t MaxArrayBytes) {
    Model Result;
    std::set<std::string> SeenSymbols;
    for (const ExprRef &Symbol : Symbols) {
      const std::string Key = sortKey(Symbol->symbolName(), Symbol->sort());
      if (!SeenSymbols.insert(Key).second)
        continue;
      Result.Scalars.push_back({Symbol->symbolName(), Symbol->sort(),
                                modelValue(translate(Symbol), Symbol->sort())});
    }

    std::set<std::pair<const Array *, std::string>> SeenElements;
    auto AddElement = [&](const ArrayRef &Root,
                          const llvm::APInt &Index) -> std::uint8_t {
      llvm::SmallString<64> IndexText;
      Index.toString(IndexText, 16, false);
      std::pair<const Array *, std::string> Key{Root.get(),
                                                IndexText.str().str()};
      for (const ArrayElementValue &Existing : Result.ArrayElements)
        if (Existing.Root.get() == Root.get() && Existing.Index == Index)
          return Existing.Value;
      ::Expr IndexTerm = constant(Index);
      ::Expr Read =
          own(vc_readExpr(Checker.get(), translateArray(Root), IndexTerm));
      llvm::APInt Byte = modelValue(Read, Sort::bitVector(8));
      std::uint8_t Value = static_cast<std::uint8_t>(Byte.getZExtValue());
      if (SeenElements.insert(Key).second)
        Result.ArrayElements.push_back({Root, Index, Value});
      return Value;
    };

    for (const ExprRef &Read : Reads) {
      llvm::APInt Index =
          modelValue(translate(Read->operand(0)), Read->operand(0)->sort());
      AddElement(Read->updates().root(), Index);
    }

    for (const ArrayModelRequest &Request : Requests) {
      if (!Request.Root)
        throw std::invalid_argument("array model request has a null root");
      if (Request.ByteCount > MaxArrayBytes)
        throw std::length_error("array model request exceeds byte limit");
      unsigned IndexWidth = Request.Root->sort().indexWidth();
      if (IndexWidth < std::numeric_limits<std::size_t>::digits &&
          Request.ByteCount > (std::size_t{1} << IndexWidth))
        throw std::invalid_argument("array model request wraps its index sort");
      InitialArrayValue Initial{Request.Root, {}};
      Initial.Bytes.reserve(Request.ByteCount);
      translateArray(Request.Root);
      for (std::size_t Index = 0; Index < Request.ByteCount; ++Index)
        Initial.Bytes.push_back(
            AddElement(Request.Root, llvm::APInt(IndexWidth, Index)));
      InitialValues.push_back(std::move(Initial));
    }
    return Result;
  }

  ::VC vc() const noexcept { return Checker.get(); }

  TranslationStatistics statistics() const {
    TranslationStatistics Result = Statistics;
    Result.ContextId = ContextId;
    return Result;
  }

private:
  static int checkedWidth(unsigned Width, const char *Description) {
    if (Width > static_cast<unsigned>(INT_MAX))
      throw std::length_error(std::string("STP ") + Description +
                              " exceeds INT_MAX");
    return static_cast<int>(Width);
  }

  static int checkedExtractHigh(const ExprRef &Expression) {
    const std::uint64_t High =
        static_cast<std::uint64_t>(Expression->offset()) +
        Expression->width() - 1;
    if (High > static_cast<std::uint64_t>(INT_MAX))
      throw std::length_error("STP extract high bit exceeds INT_MAX");
    return static_cast<int>(High);
  }

  static std::string sortKey(const std::string &Name, Sort ValueSort) {
    if (ValueSort.isBool())
      return Name + ":Bool";
    if (ValueSort.isBitVector())
      return Name + ":BV" + std::to_string(ValueSort.bitWidth());
    return Name + ":Array" + std::to_string(ValueSort.indexWidth());
  }

  void registerName(const std::string &Name, Sort ValueSort) {
    auto [Position, Inserted] = Names.emplace(Name, ValueSort);
    if (!Inserted && Position->second != ValueSort)
      throw std::invalid_argument("query name is used with incompatible sorts: " +
                                  Name);
  }

  ::Expr translateSymbol(const ExprRef &Expression) {
    registerName(Expression->symbolName(), Expression->sort());
    const std::string Key = sortKey(Expression->symbolName(), Expression->sort());
    auto Existing = NamedSymbols.find(Key);
    if (Existing != NamedSymbols.end()) {
      if (Existing->second.first != Expression.get())
        throw std::invalid_argument(
            "query contains distinct scalar symbols with the same name: " +
            Expression->symbolName());
      Symbols.push_back(Expression);
      return Existing->second.second;
    }
    ::Type Type = own(Expression->sort().isBool()
                          ? vc_boolType(Checker.get())
                          : vc_bvType(Checker.get(), checkedWidth(
                                                       Expression->width(),
                                                       "symbol")));
    std::string Name = encodedName(Expression->sort().isBool()
                                       ? "formal_bool_"
                                       : "formal_bv_",
                                   Expression->symbolName());
    ::Expr Result = vc_varExpr(Checker.get(), Name.c_str(), Type);
    NamedSymbols.emplace(Key, std::pair{Expression.get(), Result});
    Symbols.push_back(Expression);
    return Result;
  }

  ::Expr translateRead(const ExprRef &Expression) {
    ::Expr Array = translateArray(Expression->updates().root());
    std::vector<UpdateNodeRef> Updates;
    for (UpdateNodeRef Node = Expression->updates().head(); Node;
         Node = Node->next())
      Updates.push_back(Node);
    for (auto Position = Updates.rbegin(); Position != Updates.rend(); ++Position)
      Array = own(vc_writeExpr(Checker.get(), Array,
                               translate((*Position)->index()),
                               translate((*Position)->value())));
    Reads.push_back(Expression);
    return own(vc_readExpr(Checker.get(), Array,
                           translate(Expression->operand(0))));
  }

  ::Expr constant(const llvm::APInt &Value) {
    std::string Decimal = apIntDecimal(Value);
    return own(vc_bvConstExprFromDecStr(
        Checker.get(), checkedWidth(Value.getBitWidth(), "model index"),
        Decimal.c_str()));
  }

  VCHandle Checker;
  std::uint64_t ContextId;
  TranslationStatistics Statistics;
  std::unordered_map<const Expr *, ::Expr> ExpressionCache;
  std::unordered_map<const Array *, ::Expr> ArrayCache;
  std::map<std::string, std::pair<const Expr *, ::Expr>> NamedSymbols;
  std::map<std::string, ::Expr> NamedArrays;
  std::map<std::string, Sort> Names;
  std::vector<ExprRef> Symbols;
  std::vector<ExprRef> Reads;
  std::vector<ArrayRef> ArrayRoots;
  std::vector<::Expr> OwnedHandles;
  std::unordered_set<::Expr> OwnedHandleSet;
};

class ResultWriter final {
public:
  explicit ResultWriter(int FileDescriptor) : FileDescriptor(FileDescriptor) {}

  template <typename T> bool pod(const T &Value) {
    static_assert(std::is_trivially_copyable_v<T>);
    return bytes(&Value, sizeof(Value));
  }

  bool string(const std::string &Value) {
    std::uint64_t Size = Value.size();
    return pod(Size) && bytes(Value.data(), Value.size());
  }

  bool apInt(const llvm::APInt &Value, bool InjectFault = true) {
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    const int TestMode = InjectFault ? WorkerTestMode.load() : 0;
    std::uint32_t Width = TestMode == 6
                              ? std::numeric_limits<std::uint32_t>::max()
                              : Value.getBitWidth();
#else
    (void)InjectFault;
    std::uint32_t Width = Value.getBitWidth();
#endif
    if (!pod(Width))
      return false;
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    if (TestMode == 11)
      return string("not-a-number");
    if (TestMode == 12)
      return string("256");
    if (TestMode == 19)
      return string("999");
#endif
    return string(apIntDecimal(Value));
  }

private:
  bool bytes(const void *Data, std::size_t Size) {
    const auto *Position = static_cast<const unsigned char *>(Data);
    while (Size != 0) {
      ssize_t Written = ::write(FileDescriptor, Position, Size);
      if (Written < 0) {
        if (errno == EINTR)
          continue;
        return false;
      }
      Position += Written;
      Size -= static_cast<std::size_t>(Written);
    }
    return true;
  }

  int FileDescriptor;
};

class ResultReader final {
public:
  explicit ResultReader(int FileDescriptor) : FileDescriptor(FileDescriptor) {}

  template <typename T> bool pod(T &Value) {
    static_assert(std::is_trivially_copyable_v<T>);
    return bytes(&Value, sizeof(Value));
  }

  bool string(std::string &Value, std::size_t Limit = 64U << 20) {
    std::uint64_t Size = 0;
    if (!pod(Size) || Size > Limit)
      return false;
    Value.resize(static_cast<std::size_t>(Size));
    return bytes(Value.data(), Value.size());
  }

  bool apInt(llvm::APInt &Value, unsigned ExpectedWidth) {
    std::uint32_t Width = 0;
    std::string Decimal;
    if (ExpectedWidth == 0 || ExpectedWidth > static_cast<unsigned>(INT_MAX) ||
        !pod(Width) || Width != ExpectedWidth)
      return false;
    const std::uint64_t MaximumDigits =
        (static_cast<std::uint64_t>(ExpectedWidth) * 30103U + 99999U) /
        100000U;
    if (!string(Decimal, static_cast<std::size_t>(MaximumDigits)) ||
        Decimal.empty() ||
        Decimal.find_first_not_of("0123456789") != std::string::npos)
      return false;
    try {
      llvm::APInt Parsed(ExpectedWidth + 4U, 0);
      const llvm::APInt Maximum =
          llvm::APInt::getAllOnes(ExpectedWidth).zext(ExpectedWidth + 4U);
      for (char Character : Decimal) {
        Parsed *= 10U;
        Parsed += static_cast<unsigned>(Character - '0');
        if (Parsed.ugt(Maximum))
          return false;
      }
      Value = Parsed.trunc(ExpectedWidth);
      return true;
    } catch (...) {
      return false;
    }
  }

  bool atEnd() {
    unsigned char Byte = 0;
    while (true) {
      ssize_t Read = ::read(FileDescriptor, &Byte, 1);
      if (Read == 0)
        return true;
      if (Read > 0)
        return false;
      if (errno != EINTR)
        return false;
    }
  }

private:
  bool bytes(void *Data, std::size_t Size) {
    auto *Position = static_cast<unsigned char *>(Data);
    while (Size != 0) {
      ssize_t Read = ::read(FileDescriptor, Position, Size);
      if (Read == 0)
        return false;
      if (Read < 0) {
        if (errno == EINTR)
          continue;
        return false;
      }
      Position += Read;
      Size -= static_cast<std::size_t>(Read);
    }
    return true;
  }

  int FileDescriptor;
};

bool writeSort(ResultWriter &Writer, Sort Value) {
  std::uint8_t Kind = static_cast<std::uint8_t>(Value.kind());
  std::uint32_t Width = Value.isBool() ? 1
                        : Value.isArray() ? Value.indexWidth()
                                          : Value.bitWidth();
  return Writer.pod(Kind) && Writer.pod(Width);
}

bool readSort(ResultReader &Reader, Sort &Value) {
  std::uint8_t Kind = 0;
  std::uint32_t Width = 0;
  if (!Reader.pod(Kind) || !Reader.pod(Width) || Width == 0)
    return false;
  switch (static_cast<SortKind>(Kind)) {
  case SortKind::Bool:
    if (Width != 1)
      return false;
    Value = Sort::boolean();
    return true;
  case SortKind::BitVector:
    Value = Sort::bitVector(Width);
    return true;
  case SortKind::Array:
    Value = Sort::byteArray(Width);
    return true;
  }
  return false;
}

struct WireSchema {
  QueryKind Kind = QueryKind::Check;
  std::map<const Array *, std::uint64_t> ArrayIds;
  std::vector<ArrayRef> Arrays;
  std::map<std::string, Sort> Scalars;
  std::map<std::string, const Expr *> ScalarIdentities;
  std::vector<ArrayModelRequest> Requests;
  std::optional<Sort> TargetSort;
  std::size_t MaximumArrayElements = 0;
  bool ScalarSortConflict = false;
  bool ScalarIdentityConflict = false;
  bool ArrayIdentityConflict = false;
};

void addWireArray(const ArrayRef &Root, WireSchema &Schema) {
  if (!Root || Schema.ArrayIds.contains(Root.get()))
    return;
  for (const ArrayRef &Existing : Schema.Arrays)
    if (Existing.get() != Root.get() && Existing->name() == Root->name() &&
        Existing->sort() == Root->sort()) {
      Schema.ArrayIdentityConflict = true;
      break;
    }
  const std::uint64_t Id = Schema.Arrays.size();
  Schema.ArrayIds.emplace(Root.get(), Id);
  Schema.Arrays.push_back(Root);
}

void collectWireExpression(const ExprRef &Expression, WireSchema &Schema,
                           std::unordered_set<const Expr *> &SeenExpressions,
                           std::unordered_set<const UpdateNode *> &SeenUpdates) {
  if (!Expression || !SeenExpressions.insert(Expression.get()).second)
    return;
  if (Expression->kind() == ExprKind::Symbol) {
    auto [Position, Inserted] =
        Schema.Scalars.emplace(Expression->symbolName(), Expression->sort());
    if (!Inserted && Position->second != Expression->sort())
      Schema.ScalarSortConflict = true;
    auto [Identity, NewIdentity] =
        Schema.ScalarIdentities.emplace(Expression->symbolName(),
                                        Expression.get());
    if (!NewIdentity && Identity->second != Expression.get())
      Schema.ScalarIdentityConflict = true;
  }
  if (Expression->kind() == ExprKind::Read) {
    const UpdateList &Updates = Expression->updates();
    addWireArray(Updates.root(), Schema);
    if (Schema.MaximumArrayElements !=
        std::numeric_limits<std::size_t>::max())
      ++Schema.MaximumArrayElements;
    for (UpdateNodeRef Node = Updates.head(); Node; Node = Node->next()) {
      if (!SeenUpdates.insert(Node.get()).second)
        break;
      collectWireExpression(Node->index(), Schema, SeenExpressions,
                            SeenUpdates);
      collectWireExpression(Node->value(), Schema, SeenExpressions,
                            SeenUpdates);
    }
  }
  for (const ExprRef &Operand : Expression->operands())
    collectWireExpression(Operand, Schema, SeenExpressions, SeenUpdates);
}

WireSchema makeWireSchema(QueryKind Kind, const ConstraintSet &Constraints,
                          const ExprRef &Target,
                          const std::vector<ArrayModelRequest> &Requests) {
  WireSchema Schema;
  Schema.Kind = Kind;
  Schema.Requests = Requests;
  if (Target)
    Schema.TargetSort = Target->sort();
  std::unordered_set<const Expr *> SeenExpressions;
  std::unordered_set<const UpdateNode *> SeenUpdates;
  for (const ExprRef &Constraint : Constraints)
    collectWireExpression(Constraint, Schema, SeenExpressions, SeenUpdates);
  collectWireExpression(Target, Schema, SeenExpressions, SeenUpdates);
  for (const ArrayModelRequest &Request : Requests) {
    if (!Request.Root)
      continue;
    addWireArray(Request.Root, Schema);
    if (Request.ByteCount > std::numeric_limits<std::size_t>::max() -
                                Schema.MaximumArrayElements)
      Schema.MaximumArrayElements = std::numeric_limits<std::size_t>::max();
    else
      Schema.MaximumArrayElements += Request.ByteCount;
  }
  return Schema;
}

struct RequestGraph {
  std::vector<ExprRef> Expressions;
  std::unordered_map<const Expr *, std::uint64_t> Ids;
};

void collectRequestExpression(const ExprRef &Expression, RequestGraph &Graph) {
  if (!Expression || Graph.Ids.contains(Expression.get()))
    return;
  if (Expression->kind() == ExprKind::Read) {
    for (UpdateNodeRef Node = Expression->updates().head(); Node;
         Node = Node->next()) {
      collectRequestExpression(Node->index(), Graph);
      collectRequestExpression(Node->value(), Graph);
    }
  }
  for (const ExprRef &Operand : Expression->operands())
    collectRequestExpression(Operand, Graph);
  const std::uint64_t Id = Graph.Expressions.size();
  Graph.Ids.emplace(Expression.get(), Id);
  Graph.Expressions.push_back(Expression);
}

RequestGraph makeRequestGraph(const ConstraintSet &Constraints,
                              const ExprRef &Target) {
  RequestGraph Graph;
  for (const ExprRef &Constraint : Constraints)
    collectRequestExpression(Constraint, Graph);
  collectRequestExpression(Target, Graph);
  return Graph;
}

bool writeWorkerRequest(int FileDescriptor, QueryKind Kind,
                        const ConstraintSet &Constraints,
                        const ExprRef &Target,
                        const std::vector<ArrayModelRequest> &Requests,
                        const STPSolverOptions &Options,
                        std::uint64_t QueryId, std::uint64_t ContextId,
                        const WireSchema &Schema) {
  ResultWriter Writer(FileDescriptor);
  RequestGraph Graph = makeRequestGraph(Constraints, Target);
  std::uint32_t Magic = 0x46525131U;
  const std::uint32_t Version = 1;
  const std::uint8_t KindValue = static_cast<std::uint8_t>(Kind);
  const std::int64_t Timeout = Options.Timeout.count();
  const std::uint32_t MaxConflicts = Options.MaxConflicts;
  const std::uint64_t MaxArrayBytes = Options.MaxArrayBytes;
  const std::uint8_t ExportAllQueries = Options.ExportAllQueries;
  std::int32_t ReservedTestMode = 0;
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
  if (WorkerTestMode.load() == 29)
    Magic = 0;
  ReservedTestMode = WorkerTestMode.load();
#endif
  if (!Writer.pod(Magic) || !Writer.pod(Version) || !Writer.pod(KindValue) ||
      !Writer.pod(QueryId) || !Writer.pod(ContextId) || !Writer.pod(Timeout) ||
      !Writer.pod(MaxConflicts) || !Writer.pod(MaxArrayBytes) ||
      !Writer.pod(ExportAllQueries) ||
      !Writer.string(Options.FailureQueryDirectory.string()) ||
      !Writer.pod(ReservedTestMode))
    return false;

  const std::uint64_t ArrayCount = Schema.Arrays.size();
  if (!Writer.pod(ArrayCount))
    return false;
  for (const ArrayRef &Root : Schema.Arrays) {
    const std::uint32_t Width = Root->sort().indexWidth();
    if (!Writer.string(Root->name()) || !Writer.pod(Width))
      return false;
  }

  const std::uint64_t ExpressionCount = Graph.Expressions.size();
  if (!Writer.pod(ExpressionCount))
    return false;
  for (const ExprRef &Expression : Graph.Expressions) {
    const std::uint8_t ExpressionKind =
        static_cast<std::uint8_t>(Expression->kind());
    if (!Writer.pod(ExpressionKind) ||
        !writeSort(Writer, Expression->sort()))
      return false;
    if (Expression->kind() == ExprKind::Constant) {
      if (!Writer.apInt(Expression->constantValue(), false))
        return false;
      continue;
    }
    if (Expression->kind() == ExprKind::Symbol) {
      if (!Writer.string(Expression->symbolName()))
        return false;
      continue;
    }

    const std::uint32_t Offset = Expression->offset();
    const std::uint64_t OperandCount = Expression->operands().size();
    if (!Writer.pod(Offset) || !Writer.pod(OperandCount))
      return false;
    for (const ExprRef &Operand : Expression->operands()) {
      const std::uint64_t OperandId = Graph.Ids.at(Operand.get());
      if (!Writer.pod(OperandId))
        return false;
    }
    if (Expression->kind() != ExprKind::Read)
      continue;
    const UpdateList &Updates = Expression->updates();
    const std::uint64_t ArrayId = Schema.ArrayIds.at(Updates.root().get());
    const std::uint64_t UpdateCount = Updates.head() ? Updates.head()->size() : 0;
    if (!Writer.pod(ArrayId) || !Writer.pod(UpdateCount))
      return false;
    for (UpdateNodeRef Node = Updates.head(); Node; Node = Node->next()) {
      const std::uint64_t IndexId = Graph.Ids.at(Node->index().get());
      const std::uint64_t ValueId = Graph.Ids.at(Node->value().get());
      if (!Writer.pod(IndexId) || !Writer.pod(ValueId))
        return false;
    }
  }

  const std::uint64_t ConstraintCount = Constraints.size();
  if (!Writer.pod(ConstraintCount))
    return false;
  for (const ExprRef &Constraint : Constraints) {
    const std::uint64_t Id = Graph.Ids.at(Constraint.get());
    if (!Writer.pod(Id))
      return false;
  }
  const std::uint8_t HasTarget = Target != nullptr;
  if (!Writer.pod(HasTarget))
    return false;
  if (HasTarget) {
    const std::uint64_t TargetId = Graph.Ids.at(Target.get());
    if (!Writer.pod(TargetId))
      return false;
  }
  const std::uint64_t RequestCount = Requests.size();
  if (!Writer.pod(RequestCount))
    return false;
  for (const ArrayModelRequest &Request : Requests) {
    auto Position = Schema.ArrayIds.find(Request.Root.get());
    const std::uint64_t ByteCount = Request.ByteCount;
    if (Position == Schema.ArrayIds.end() || !Writer.pod(Position->second) ||
        !Writer.pod(ByteCount))
      return false;
  }
  return true;
}

struct SerializedExpression {
  ExprKind Kind = ExprKind::Constant;
  Sort ValueSort = Sort::boolean();
  llvm::APInt Constant = llvm::APInt(1, 0);
  std::string Name;
  unsigned Offset = 0;
  std::vector<std::uint64_t> Operands;
  std::uint64_t ArrayId = 0;
  std::vector<std::pair<std::uint64_t, std::uint64_t>> Updates;
};

struct WorkerRequest {
  std::unique_ptr<ExprContext> Context;
  QueryKind Kind = QueryKind::Check;
  ConstraintSet Constraints;
  ExprRef Target;
  std::vector<ArrayModelRequest> Arrays;
  STPSolverOptions Options;
  std::uint64_t QueryId = 0;
  std::uint64_t ContextId = 0;
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
  int TestMode = 0;
#endif
};

ExprRef rebuildExpression(ExprContext &Context,
                          const SerializedExpression &Record,
                          const std::vector<ExprRef> &Expressions,
                          const std::vector<ArrayRef> &Arrays) {
  auto Operand = [&](std::size_t Index) -> ExprRef {
    if (Index >= Record.Operands.size() ||
        Record.Operands[Index] >= Expressions.size())
      throw std::invalid_argument("worker request has an invalid operand ID");
    return Expressions[static_cast<std::size_t>(Record.Operands[Index])];
  };
  auto RequireOperands = [&](std::size_t Count) {
    if (Record.Operands.size() != Count)
      throw std::invalid_argument("worker request has an invalid operand count");
  };
  ExprRef Result;
  switch (Record.Kind) {
  case ExprKind::Constant:
    RequireOperands(0);
    Result = Record.ValueSort.isBool()
                 ? Context.boolean(Record.Constant.getBoolValue())
                 : Context.constant(Record.Constant);
    break;
  case ExprKind::Symbol:
    RequireOperands(0);
    Result = Record.ValueSort.isBool()
                 ? Context.booleanSymbol(Record.Name)
                 : Context.symbol(Record.ValueSort.bitWidth(), Record.Name);
    break;
  case ExprKind::Read: {
    RequireOperands(1);
    if (Record.ArrayId >= Arrays.size())
      throw std::invalid_argument("worker request has an invalid array ID");
    UpdateList Updates =
        Context.updates(Arrays[static_cast<std::size_t>(Record.ArrayId)]);
    for (auto Position = Record.Updates.rbegin();
         Position != Record.Updates.rend(); ++Position) {
      if (Position->first >= Expressions.size() ||
          Position->second >= Expressions.size())
        throw std::invalid_argument("worker request has an invalid update ID");
      Updates = Context.write(
          Updates, Expressions[static_cast<std::size_t>(Position->first)],
          Expressions[static_cast<std::size_t>(Position->second)]);
    }
    Result = Context.read(Updates, Operand(0));
    break;
  }
#define FORMAL_REBUILD_BINARY(Name, Method)                                    \
  case ExprKind::Name:                                                         \
    RequireOperands(2);                                                        \
    Result = Context.Method(Operand(0), Operand(1));                            \
    break
    FORMAL_REBUILD_BINARY(Add, add);
    FORMAL_REBUILD_BINARY(Sub, sub);
    FORMAL_REBUILD_BINARY(Mul, mul);
    FORMAL_REBUILD_BINARY(UDiv, udiv);
    FORMAL_REBUILD_BINARY(SDiv, sdiv);
    FORMAL_REBUILD_BINARY(URem, urem);
    FORMAL_REBUILD_BINARY(SRem, srem);
    FORMAL_REBUILD_BINARY(And, bitAnd);
    FORMAL_REBUILD_BINARY(Or, bitOr);
    FORMAL_REBUILD_BINARY(Xor, bitXor);
    FORMAL_REBUILD_BINARY(Shl, shl);
    FORMAL_REBUILD_BINARY(LShr, lshr);
    FORMAL_REBUILD_BINARY(AShr, ashr);
    FORMAL_REBUILD_BINARY(Eq, eq);
    FORMAL_REBUILD_BINARY(Ult, ult);
    FORMAL_REBUILD_BINARY(Ule, ule);
    FORMAL_REBUILD_BINARY(Slt, slt);
    FORMAL_REBUILD_BINARY(Sle, sle);
#undef FORMAL_REBUILD_BINARY
  case ExprKind::Not:
    RequireOperands(1);
    Result = Context.logicalNot(Operand(0));
    break;
  case ExprKind::Select:
    RequireOperands(3);
    Result = Context.select(Operand(0), Operand(1), Operand(2));
    break;
  case ExprKind::Concat:
    RequireOperands(2);
    Result = Context.concat(Operand(0), Operand(1));
    break;
  case ExprKind::Extract:
    RequireOperands(1);
    Result = Context.extract(Operand(0), Record.Offset,
                             Record.ValueSort.bitWidth());
    break;
  case ExprKind::ZExt:
    RequireOperands(1);
    Result = Context.zext(Operand(0), Record.ValueSort.bitWidth());
    break;
  case ExprKind::SExt:
    RequireOperands(1);
    Result = Context.sext(Operand(0), Record.ValueSort.bitWidth());
    break;
  case ExprKind::Trunc:
    RequireOperands(1);
    Result = Context.trunc(Operand(0), Record.ValueSort.bitWidth());
    break;
  }
  if (!Result || Result->sort() != Record.ValueSort)
    throw std::invalid_argument("worker request expression sort mismatch");
  return Result;
}

bool readWorkerRequest(int FileDescriptor, WorkerRequest &Request) {
  ResultReader Reader(FileDescriptor);
  std::uint32_t Magic = 0;
  std::uint32_t Version = 0;
  std::uint8_t Kind = 0;
  std::int64_t Timeout = 0;
  std::uint32_t MaxConflicts = 0;
  std::uint64_t MaxArrayBytes = 0;
  std::uint8_t ExportAllQueries = 0;
  std::string FailureQueryDirectory;
  std::int32_t ReservedTestMode = 0;
  if (!Reader.pod(Magic) || Magic != 0x46525131U || !Reader.pod(Version) ||
      Version != 1 || !Reader.pod(Kind) ||
      Kind > static_cast<std::uint8_t>(QueryKind::InitialValues) ||
      !Reader.pod(Request.QueryId) || !Reader.pod(Request.ContextId) ||
      !Reader.pod(Timeout) || Timeout <= 0 || !Reader.pod(MaxConflicts) ||
      !Reader.pod(MaxArrayBytes) ||
      MaxArrayBytes > std::numeric_limits<std::size_t>::max() ||
      !Reader.pod(ExportAllQueries) || ExportAllQueries > 1 ||
      !Reader.string(FailureQueryDirectory, 1U << 20) ||
      !Reader.pod(ReservedTestMode))
    return false;
#ifndef FORMAL_SOLVER_ENABLE_TEST_FAULTS
  if (ReservedTestMode != 0)
    return false;
#endif
  Request.Kind = static_cast<QueryKind>(Kind);
  Request.Options.Timeout = std::chrono::milliseconds(Timeout);
  Request.Options.MaxConflicts = MaxConflicts;
  Request.Options.MaxArrayBytes = static_cast<std::size_t>(MaxArrayBytes);
  Request.Options.ExportAllQueries = ExportAllQueries != 0;
  Request.Options.FailureQueryDirectory = std::move(FailureQueryDirectory);
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
  Request.TestMode = ReservedTestMode;
#endif

  std::uint64_t ArrayCount = 0;
  if (!Reader.pod(ArrayCount) || ArrayCount > UINT_MAX - 1U)
    return false;
  std::vector<std::pair<std::string, unsigned>> ArrayRecords;
  ArrayRecords.reserve(static_cast<std::size_t>(ArrayCount));
  unsigned MaximumWidth = 1;
  for (std::uint64_t Index = 0; Index < ArrayCount; ++Index) {
    std::string Name;
    std::uint32_t Width = 0;
    if (!Reader.string(Name, 1U << 20) || !Reader.pod(Width) || Width == 0)
      return false;
    MaximumWidth = std::max(MaximumWidth, Width);
    ArrayRecords.emplace_back(std::move(Name), Width);
  }

  std::uint64_t ExpressionCount = 0;
  if (!Reader.pod(ExpressionCount) || ExpressionCount > UINT_MAX - 1U)
    return false;
  std::vector<SerializedExpression> Records;
  Records.reserve(static_cast<std::size_t>(ExpressionCount));
  std::size_t TotalUpdates = 0;
  for (std::uint64_t Index = 0; Index < ExpressionCount; ++Index) {
    std::uint8_t KindValue = 0;
    SerializedExpression Record;
    if (!Reader.pod(KindValue) ||
        KindValue > static_cast<std::uint8_t>(ExprKind::Trunc) ||
        !readSort(Reader, Record.ValueSort) || Record.ValueSort.isArray())
      return false;
    Record.Kind = static_cast<ExprKind>(KindValue);
    MaximumWidth = std::max(
        MaximumWidth,
        Record.ValueSort.isBool() ? 1U : Record.ValueSort.bitWidth());
    if (Record.Kind == ExprKind::Constant) {
      const unsigned Width =
          Record.ValueSort.isBool() ? 1U : Record.ValueSort.bitWidth();
      if (!Reader.apInt(Record.Constant, Width))
        return false;
      Records.push_back(std::move(Record));
      continue;
    }
    if (Record.Kind == ExprKind::Symbol) {
      if (!Reader.string(Record.Name, 1U << 20))
        return false;
      Records.push_back(std::move(Record));
      continue;
    }
    std::uint32_t Offset = 0;
    std::uint64_t OperandCount = 0;
    if (!Reader.pod(Offset) || !Reader.pod(OperandCount) || OperandCount > 3)
      return false;
    Record.Offset = Offset;
    Record.Operands.resize(static_cast<std::size_t>(OperandCount));
    for (std::uint64_t &OperandId : Record.Operands)
      if (!Reader.pod(OperandId) || OperandId >= Index)
        return false;
    if (Record.Kind == ExprKind::Read) {
      std::uint64_t UpdateCount = 0;
      if (!Reader.pod(Record.ArrayId) || Record.ArrayId >= ArrayCount ||
          !Reader.pod(UpdateCount) ||
          TotalUpdates == std::numeric_limits<std::size_t>::max() ||
          UpdateCount > std::numeric_limits<std::size_t>::max() -
                            TotalUpdates - 1U)
        return false;
      TotalUpdates += static_cast<std::size_t>(UpdateCount);
      Record.Updates.resize(static_cast<std::size_t>(UpdateCount));
      for (auto &[IndexId, ValueId] : Record.Updates)
        if (!Reader.pod(IndexId) || IndexId >= Index ||
            !Reader.pod(ValueId) || ValueId >= Index)
          return false;
    }
    Records.push_back(std::move(Record));
  }

  ExprContextOptions ContextOptions;
  ContextOptions.MaxBitWidth = MaximumWidth;
  ContextOptions.MaxDepth = static_cast<unsigned>(ExpressionCount + 1U);
  ContextOptions.MaxNodes = static_cast<std::size_t>(ExpressionCount + 1U);
  ContextOptions.MaxUpdates = TotalUpdates + 1U;
  ContextOptions.MaxArrays = static_cast<std::size_t>(ArrayCount + 1U);
  Request.Context = std::make_unique<ExprContext>(ContextOptions);
  std::vector<ArrayRef> Arrays;
  Arrays.reserve(ArrayRecords.size());
  for (auto &[Name, Width] : ArrayRecords)
    Arrays.push_back(Request.Context->array(Width, std::move(Name)));
  std::vector<ExprRef> Expressions;
  Expressions.reserve(Records.size());
  for (const SerializedExpression &Record : Records)
    Expressions.push_back(
        rebuildExpression(*Request.Context, Record, Expressions, Arrays));

  std::uint64_t ConstraintCount = 0;
  if (!Reader.pod(ConstraintCount) || ConstraintCount > UINT_MAX - 1U)
    return false;
  Request.Constraints.reserve(static_cast<std::size_t>(ConstraintCount));
  for (std::uint64_t Index = 0; Index < ConstraintCount; ++Index) {
    std::uint64_t Id = 0;
    if (!Reader.pod(Id) || Id >= Expressions.size())
      return false;
    Request.Constraints.push_back(Expressions[static_cast<std::size_t>(Id)]);
  }
  std::uint8_t HasTarget = 0;
  if (!Reader.pod(HasTarget) || HasTarget > 1)
    return false;
  if (HasTarget) {
    std::uint64_t Id = 0;
    if (!Reader.pod(Id) || Id >= Expressions.size())
      return false;
    Request.Target = Expressions[static_cast<std::size_t>(Id)];
  }
  std::uint64_t RequestCount = 0;
  if (!Reader.pod(RequestCount) || RequestCount > UINT_MAX - 1U)
    return false;
  Request.Arrays.reserve(static_cast<std::size_t>(RequestCount));
  for (std::uint64_t Index = 0; Index < RequestCount; ++Index) {
    std::uint64_t ArrayId = 0;
    std::uint64_t ByteCount = 0;
    if (!Reader.pod(ArrayId) || ArrayId >= Arrays.size() ||
        !Reader.pod(ByteCount) ||
        ByteCount > std::numeric_limits<std::size_t>::max())
      return false;
    Request.Arrays.push_back(
        {Arrays[static_cast<std::size_t>(ArrayId)],
         static_cast<std::size_t>(ByteCount)});
  }
  return Reader.atEnd();
}

bool writeArray(ResultWriter &Writer, const WireSchema &Schema,
                const ArrayRef &Root) {
  if (!Root)
    return false;
  auto Position = Schema.ArrayIds.find(Root.get());
  return Position != Schema.ArrayIds.end() && Writer.pod(Position->second);
}

bool readArray(ResultReader &Reader, const WireSchema &Schema, ArrayRef &Root) {
  std::uint64_t Id = 0;
  if (!Reader.pod(Id) || Id >= Schema.Arrays.size())
    return false;
  Root = Schema.Arrays[static_cast<std::size_t>(Id)];
  return true;
}

bool writeSolverResult(int FileDescriptor, const WireSchema &Schema,
                       const SolverResult &Result) {
  ResultWriter Writer(FileDescriptor);
  const std::uint32_t Magic = 0x46535450U;
  std::uint32_t Version = 1U;
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
  if (WorkerTestMode.load() == 16)
    Version = std::numeric_limits<std::uint32_t>::max();
#endif
  std::int32_t Status = static_cast<std::int32_t>(Result.Status);
  std::int32_t Reason = static_cast<std::int32_t>(Result.Metadata.Reason);
  std::int64_t Elapsed = Result.Metadata.Elapsed.count();
  if (!Writer.pod(Magic) || !Writer.pod(Version) || !Writer.pod(Status) ||
      !Writer.pod(Reason) ||
      !Writer.pod(Result.Metadata.QueryId) || !Writer.pod(Elapsed) ||
      !Writer.string(Result.Metadata.BackendName) ||
      !Writer.string(Result.Metadata.BackendVersion) ||
      !Writer.string(Result.Metadata.SatBackend) ||
      !Writer.string(Result.Metadata.Detail) ||
      !Writer.string(Result.Metadata.SavedQuery.string()) ||
      !Writer.pod(Result.Metadata.Translation.ExpressionTranslations) ||
      !Writer.pod(Result.Metadata.Translation.ExpressionCacheHits) ||
      !Writer.pod(Result.Metadata.Translation.ArrayTranslations) ||
      !Writer.pod(Result.Metadata.Translation.ArrayCacheHits) ||
      !Writer.pod(Result.Metadata.Translation.ContextId))
    return false;

  std::uint8_t HasTruth = Result.Truth.has_value();
  std::uint8_t Truth = Result.Truth.value_or(false);
  std::uint8_t HasValue = Result.Value.has_value();
  std::uint8_t HasModel = Result.SatisfyingModel.has_value();
  if (!Writer.pod(HasTruth) || !Writer.pod(Truth) || !Writer.pod(HasValue) ||
      (HasValue && !Writer.apInt(*Result.Value)) || !Writer.pod(HasModel))
    return false;

  if (HasModel) {
    std::uint64_t ScalarCount = Result.SatisfyingModel->Scalars.size();
    std::uint64_t ElementCount = Result.SatisfyingModel->ArrayElements.size();
    if (!Writer.pod(ScalarCount))
      return false;
    for (const ScalarModelValue &Scalar : Result.SatisfyingModel->Scalars)
      if (!Writer.string(Scalar.Name) || !writeSort(Writer, Scalar.ValueSort) ||
          !Writer.apInt(Scalar.Value))
        return false;
    if (!Writer.pod(ElementCount))
      return false;
    for (const ArrayElementValue &Element :
         Result.SatisfyingModel->ArrayElements)
      if (!writeArray(Writer, Schema, Element.Root) ||
          !Writer.apInt(Element.Index) || !Writer.pod(Element.Value))
        return false;
  }

  std::uint64_t InitialCount = Result.InitialValues.size();
  if (!Writer.pod(InitialCount))
    return false;
  for (const InitialArrayValue &Initial : Result.InitialValues) {
    std::uint64_t ByteCount = Initial.Bytes.size();
    if (!writeArray(Writer, Schema, Initial.Root) || !Writer.pod(ByteCount))
      return false;
    for (std::uint8_t Byte : Initial.Bytes)
      if (!Writer.pod(Byte))
        return false;
  }
  return true;
}

bool readSolverResult(int FileDescriptor, const WireSchema &Schema,
                      SolverResult &Result) {
  ResultReader Reader(FileDescriptor);
  std::uint32_t Magic = 0;
  std::uint32_t Version = 0;
  std::int32_t Status = 0;
  std::int32_t Reason = 0;
  std::int64_t Elapsed = 0;
  std::string SavedQuery;
  if (!Reader.pod(Magic) || Magic != 0x46535450U || !Reader.pod(Version) ||
      Version != 1 || !Reader.pod(Status) ||
      Status < static_cast<int>(SolverStatus::Sat) ||
      Status > static_cast<int>(SolverStatus::InternalError) ||
      !Reader.pod(Reason) || Reason < static_cast<int>(SolverReason::None) ||
      Reason > static_cast<int>(SolverReason::WorkerFailure) ||
      !Reader.pod(Result.Metadata.QueryId) || !Reader.pod(Elapsed) ||
      !Reader.string(Result.Metadata.BackendName) ||
      !Reader.string(Result.Metadata.BackendVersion) ||
      !Reader.string(Result.Metadata.SatBackend) ||
      !Reader.string(Result.Metadata.Detail) || !Reader.string(SavedQuery) ||
      !Reader.pod(Result.Metadata.Translation.ExpressionTranslations) ||
      !Reader.pod(Result.Metadata.Translation.ExpressionCacheHits) ||
      !Reader.pod(Result.Metadata.Translation.ArrayTranslations) ||
      !Reader.pod(Result.Metadata.Translation.ArrayCacheHits) ||
      !Reader.pod(Result.Metadata.Translation.ContextId))
    return false;
  Result.Status = static_cast<SolverStatus>(Status);
  Result.Metadata.Reason = static_cast<SolverReason>(Reason);
  Result.Metadata.Elapsed = std::chrono::nanoseconds(Elapsed);
  Result.Metadata.SavedQuery = std::move(SavedQuery);

  std::uint8_t HasTruth = 0;
  std::uint8_t Truth = 0;
  std::uint8_t HasValue = 0;
  std::uint8_t HasModel = 0;
  if (!Reader.pod(HasTruth) || !Reader.pod(Truth) || HasTruth > 1 || Truth > 1 ||
      !Reader.pod(HasValue) || HasValue > 1)
    return false;
  if (HasTruth)
    Result.Truth = Truth != 0;
  if (HasValue) {
    llvm::APInt Value(1, 0);
    if (!Schema.TargetSort || Schema.TargetSort->isArray())
      return false;
    unsigned Width = Schema.TargetSort->isBool()
                         ? 1
                         : Schema.TargetSort->bitWidth();
    if (!Reader.apInt(Value, Width))
      return false;
    Result.Value = std::move(Value);
  }
  if (!Reader.pod(HasModel) || HasModel > 1)
    return false;
  if (HasModel) {
    Model ModelValue;
    std::uint64_t Count = 0;
    if (!Reader.pod(Count) || Count != Schema.Scalars.size())
      return false;
    ModelValue.Scalars.reserve(static_cast<std::size_t>(Count));
    std::set<std::string> SeenScalars;
    for (std::uint64_t Index = 0; Index < Count; ++Index) {
      ScalarModelValue Scalar{"", Sort::boolean(), llvm::APInt(1, 0)};
      if (!Reader.string(Scalar.Name, 1024) ||
          !SeenScalars.insert(Scalar.Name).second)
        return false;
      auto Expected = Schema.Scalars.find(Scalar.Name);
      if (Expected == Schema.Scalars.end() ||
          !readSort(Reader, Scalar.ValueSort) ||
          Scalar.ValueSort != Expected->second || Scalar.ValueSort.isArray())
        return false;
      unsigned Width = Scalar.ValueSort.isBool()
                           ? 1
                           : Scalar.ValueSort.bitWidth();
      if (!Reader.apInt(Scalar.Value, Width))
        return false;
      ModelValue.Scalars.push_back(std::move(Scalar));
    }
    if (!Reader.pod(Count) || Count > Schema.MaximumArrayElements)
      return false;
    ModelValue.ArrayElements.reserve(static_cast<std::size_t>(Count));
    std::set<std::pair<std::uint64_t, std::string>> SeenElements;
    for (std::uint64_t Index = 0; Index < Count; ++Index) {
      ArrayElementValue Element;
      if (!readArray(Reader, Schema, Element.Root) ||
          !Reader.apInt(Element.Index,
                        Element.Root->sort().indexWidth()) ||
          !Reader.pod(Element.Value))
        return false;
      llvm::SmallString<64> IndexText;
      Element.Index.toString(IndexText, 16, false);
      std::uint64_t ArrayId = Schema.ArrayIds.at(Element.Root.get());
      if (!SeenElements
               .insert({ArrayId, IndexText.str().str()})
               .second)
        return false;
      ModelValue.ArrayElements.push_back(std::move(Element));
    }
    Result.SatisfyingModel = std::move(ModelValue);
  }

  std::uint64_t Count = 0;
  if (!Reader.pod(Count) || Count > Schema.Requests.size())
    return false;
  Result.InitialValues.reserve(static_cast<std::size_t>(Count));
  for (std::uint64_t Index = 0; Index < Count; ++Index) {
    InitialArrayValue Initial;
    std::uint64_t ByteCount = 0;
    if (!readArray(Reader, Schema, Initial.Root) ||
        !Reader.pod(ByteCount) ||
        ByteCount > Schema.Requests[static_cast<std::size_t>(Index)].ByteCount)
      return false;
    Initial.Bytes.resize(static_cast<std::size_t>(ByteCount));
    for (std::uint8_t &Byte : Initial.Bytes)
      if (!Reader.pod(Byte))
        return false;
    Result.InitialValues.push_back(std::move(Initial));
  }
  return Reader.atEnd();
}

bool hasExpectedTruthPayload(const SolverResult &Result, QueryKind Kind) {
  if (Kind != QueryKind::MayBeTrue && Kind != QueryKind::MustBeTrue)
    return !Result.Truth;
  if (Result.Status != SolverStatus::Sat &&
      Result.Status != SolverStatus::Unsat)
    return !Result.Truth;
  if (!Result.Truth)
    return false;
  bool Expected = Kind == QueryKind::MayBeTrue
                      ? Result.Status == SolverStatus::Sat
                      : Result.Status == SolverStatus::Unsat;
  return *Result.Truth == Expected;
}

bool hasNoResultPayload(const SolverResult &Result) {
  return !Result.Truth && !Result.Value && !Result.SatisfyingModel &&
         Result.InitialValues.empty();
}

bool validateResultEnvelope(const SolverResult &Result,
                            const WireSchema &Schema) {
  if (Result.Metadata.Elapsed.count() < 0 ||
      !hasExpectedTruthPayload(Result, Schema.Kind))
    return false;

  switch (Result.Status) {
  case SolverStatus::Sat:
    if (Result.Metadata.Reason != SolverReason::Satisfiable ||
        !Result.SatisfyingModel)
      return false;
    if ((Schema.Kind == QueryKind::GetValue) != Result.Value.has_value())
      return false;
    if (Schema.Kind == QueryKind::InitialValues) {
      if (Result.InitialValues.size() != Schema.Requests.size())
        return false;
    } else if (!Result.InitialValues.empty()) {
      return false;
    }
    return true;
  case SolverStatus::Unsat:
    return Result.Metadata.Reason == SolverReason::Unsatisfiable &&
           !Result.Value && !Result.SatisfyingModel &&
           Result.InitialValues.empty();
  case SolverStatus::Unknown:
    return (Result.Metadata.Reason == SolverReason::BackendUnknown ||
            Result.Metadata.Reason == SolverReason::WorkerFailure) &&
           hasNoResultPayload(Result);
  case SolverStatus::Timeout:
    return Result.Metadata.Reason == SolverReason::Timeout &&
           hasNoResultPayload(Result);
  case SolverStatus::InternalError:
    switch (Result.Metadata.Reason) {
    case SolverReason::BackendFailure:
    case SolverReason::TranslationError:
    case SolverReason::InvalidQuery:
    case SolverReason::ModelValidationFailed:
    case SolverReason::ArtifactWriteFailed:
      return hasNoResultPayload(Result);
    case SolverReason::WorkerFailure:
      return false;
    case SolverReason::None:
    case SolverReason::Satisfiable:
    case SolverReason::Unsatisfiable:
    case SolverReason::BackendUnknown:
    case SolverReason::Timeout:
      return false;
    }
  }
  return false;
}

bool setWorkerLimit(int Resource, rlim_t Limit) {
  struct rlimit Value { Limit, Limit };
  return ::setrlimit(Resource, &Value) == 0;
}

std::optional<std::chrono::steady_clock::time_point>
makeDeadline(std::chrono::steady_clock::time_point Started,
             std::chrono::milliseconds Timeout) {
  using Clock = std::chrono::steady_clock;
  if (Timeout == STPSolverOptions::UnlimitedTimeout)
    return std::nullopt;
  const auto MaximumDelay = std::chrono::duration_cast<
      std::chrono::milliseconds>(Clock::time_point::max() - Started);
  if (Timeout >= MaximumDelay)
    return Clock::time_point::max();
  return Started + std::chrono::duration_cast<Clock::duration>(Timeout);
}

std::optional<std::string>
arrayRequestError(const ArrayModelRequest &Request, std::size_t MaxArrayBytes) {
  if (!Request.Root)
    return "array model request has a null root";
  if (Request.ByteCount > MaxArrayBytes)
    return "array model request exceeds byte limit";
  const unsigned IndexWidth = Request.Root->sort().indexWidth();
  if (IndexWidth < std::numeric_limits<std::size_t>::digits &&
      Request.ByteCount > (std::size_t{1} << IndexWidth))
    return "array model request wraps its index sort";
  return std::nullopt;
}

std::string stpCompilationProfileMismatch(const std::string &Profile) {
  if (Profile.empty())
    return "missing compilation profile";
  if (Profile.find("-DNDEBUG") != std::string::npos)
    return "assertions are disabled (-DNDEBUG is present)";
  const std::string CompilerOption =
      "CMAKE_CXX_COMPILER = " + std::string(FORMAL_STP_COMPILER);
  if (Profile.find(CompilerOption) == std::string::npos)
    return "missing locked option " + CompilerOption;
  for (const char *Required : {"ONLY_SIMPLE = OFF", "STATICCOMPILE = OFF",
                               "FORCE_CMS = OFF", "ENABLE_TESTING = ON",
                               "ENABLE_PYTHON_INTERFACE = OFF", "libminisat"})
    if (Profile.find(Required) == std::string::npos)
      return "missing locked option " + std::string(Required);
  if (Profile.find("USE_CRYPTOMINISAT") != std::string::npos)
    return "CryptoMiniSat is not the locked SAT backend";
  return {};
}

bool closeWorkerDescriptorsExcept(int FirstKeep, int SecondKeep) {
  if (FirstKeep < 3 || SecondKeep < 3 || FirstKeep == SecondKeep)
    return false;
  unsigned First = static_cast<unsigned>(std::min(FirstKeep, SecondKeep));
  unsigned Second = static_cast<unsigned>(std::max(FirstKeep, SecondKeep));
#ifdef SYS_close_range
  auto CloseRange = [](unsigned Begin, unsigned End) {
    return Begin > End ||
           ::syscall(SYS_close_range, Begin, End, 0U) == 0;
  };
  if (CloseRange(3U, First - 1U) &&
      CloseRange(First + 1U, Second - 1U) &&
      CloseRange(Second + 1U, std::numeric_limits<unsigned>::max()))
    return true;
  if (errno != ENOSYS && errno != EINVAL)
    return false;
#endif
  struct rlimit Limit {};
  if (::getrlimit(RLIMIT_NOFILE, &Limit) != 0 || Limit.rlim_cur == RLIM_INFINITY)
    return false;
  const rlim_t Maximum = std::min<rlim_t>(
      Limit.rlim_cur, static_cast<rlim_t>(std::numeric_limits<int>::max()));
  for (int Descriptor = 3; static_cast<rlim_t>(Descriptor) < Maximum;
       ++Descriptor)
    if (Descriptor != FirstKeep && Descriptor != SecondKeep)
      (void)::close(Descriptor);
  return true;
}

bool applyWorkerLimits(const STPSolverOptions &Options) {
  if (!setWorkerLimit(RLIMIT_CORE, 0) ||
      !setWorkerLimit(RLIMIT_FSIZE,
                      static_cast<rlim_t>(Options.MaxWorkerFileBytes)))
    return false;
#ifndef FORMAL_ADDRESS_SANITIZER
  if (!setWorkerLimit(RLIMIT_AS,
                      static_cast<rlim_t>(Options.MaxWorkerMemoryBytes)))
    return false;
#endif
  if (Options.Timeout == STPSolverOptions::UnlimitedTimeout)
    return true;
  const std::uint64_t Milliseconds =
      static_cast<std::uint64_t>(Options.Timeout.count());
  const std::uint64_t Seconds = Milliseconds / 1000U +
                                (Milliseconds % 1000U != 0) + 1U;
  return Seconds <= std::numeric_limits<rlim_t>::max() &&
         setWorkerLimit(RLIMIT_CPU, static_cast<rlim_t>(Seconds));
}

} // namespace

struct STPSolver::Impl {
  explicit Impl(STPSolverOptions Options) : Options(std::move(Options)) {
    const char *Tag = get_git_version_tag();
    const char *Revision = get_git_version_sha();
    const char *CompilationEnvironment = get_compilation_env();
    Version = std::string(Tag ? Tag : "unknown") + "+" +
              (Revision ? Revision : "unknown");
    CompilationProfile = CompilationEnvironment ? CompilationEnvironment : "";
    if (!Tag || std::string(Tag) != FORMAL_STP_VERSION || !Revision ||
        std::string(Revision) != FORMAL_STP_REVISION)
      throw std::runtime_error("linked STP version does not match the CMake probe");
    const std::string ProfileMismatch =
        stpCompilationProfileMismatch(CompilationProfile);
    if (!ProfileMismatch.empty())
      throw std::runtime_error("linked STP compilation profile mismatch: " +
                               ProfileMismatch);
    if (this->Options.MaxConflicts > static_cast<unsigned>(INT_MAX))
      throw std::invalid_argument("STP conflict limit exceeds INT_MAX");
    if (this->Options.Timeout.count() < 0)
      throw std::invalid_argument("STP timeout must not be negative");
    if (this->Options.AdditionalModelAudit &&
        this->Options.Timeout != STPSolverOptions::UnlimitedTimeout)
      throw std::invalid_argument(
          "STP additional model audit requires UnlimitedTimeout because "
          "arbitrary parent callbacks cannot be cancelled safely");
    if (this->Options.MaxWorkerResponseBytes == 0 ||
        this->Options.MaxWorkerMemoryBytes == 0 ||
        this->Options.MaxWorkerFileBytes <
            this->Options.MaxWorkerResponseBytes ||
        this->Options.MaxWorkerMemoryBytes >
            std::numeric_limits<rlim_t>::max() ||
        this->Options.MaxWorkerFileBytes >
            std::numeric_limits<rlim_t>::max())
      throw std::invalid_argument("STP worker resource limits are invalid");
  }

  SolverResult run(QueryKind Kind, const ConstraintSet &Constraints,
                   const ExprRef &Target,
                   const std::vector<ArrayModelRequest> &Arrays) {
    const std::uint64_t QueryId = NextQueryId.fetch_add(1);
    if (Options.Timeout.count() == 0) {
      const auto Started = solverClockNow();
      SolverResult Result = baseResult(QueryId);
      Result.Status = SolverStatus::Timeout;
      Result.Metadata.Reason = SolverReason::Timeout;
      Result.Metadata.Detail =
          "zero timeout expired before query validation or STP initialization";
      Result.Metadata.Elapsed = solverClockNow() - Started;
      saveQuery(Result, Kind, {}, nullptr, false, false);
      return Result;
    }

    const std::uint64_t ContextId = NextContextId.fetch_add(1);
    const auto Started = solverClockNow();
    using Clock = std::chrono::steady_clock;
    const std::optional<Clock::time_point> Deadline =
        makeDeadline(Started, Options.Timeout);
    auto DeadlineExpired = [&]() {
      return Deadline && solverClockNow() >= *Deadline;
    };
    auto TimeoutResult = [&](std::string Detail) {
      SolverResult Result = baseResult(QueryId);
      Result.Status = SolverStatus::Timeout;
      Result.Metadata.Reason = SolverReason::Timeout;
      Result.Metadata.Detail = std::move(Detail);
      Result.Metadata.Elapsed = solverClockNow() - Started;
      const bool AssertTarget = Kind == QueryKind::MayBeTrue ||
                                Kind == QueryKind::MustBeTrue;
      saveQuery(Result, Kind, Constraints, Target, AssertTarget,
                Kind == QueryKind::MustBeTrue, Arrays);
      return Result;
    };
    auto InvalidQueryResult = [&](std::string Detail) {
      SolverResult Result = baseResult(QueryId);
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::InvalidQuery;
      Result.Metadata.Detail = std::move(Detail);
      Result.Metadata.Elapsed = solverClockNow() - Started;
      const bool AssertTarget = Kind == QueryKind::MayBeTrue ||
                                Kind == QueryKind::MustBeTrue;
      saveQuery(Result, Kind, Constraints, Target, AssertTarget,
                Kind == QueryKind::MustBeTrue, Arrays);
      return Result;
    };
    for (std::size_t Index = 0; Index < Constraints.size(); ++Index)
      if (!Constraints[Index] || !Constraints[Index]->sort().isBool())
        return InvalidQueryResult("constraint " + std::to_string(Index) +
                                  " must have Bool sort");
    if ((Kind == QueryKind::MayBeTrue || Kind == QueryKind::MustBeTrue) &&
        (!Target || !Target->sort().isBool()))
      return InvalidQueryResult("truth-query target must have Bool sort");
    if (Kind == QueryKind::GetValue &&
        (!Target || Target->sort().isArray()))
      return InvalidQueryResult("getValue target must have scalar sort");
    for (const ArrayModelRequest &Request : Arrays)
      if (std::optional<std::string> Error =
              arrayRequestError(Request, Options.MaxArrayBytes))
        return InvalidQueryResult(*Error);

    WireSchema Schema = makeWireSchema(Kind, Constraints, Target, Arrays);
    if (Schema.ArrayIdentityConflict)
      return InvalidQueryResult(
          "query contains distinct arrays with the same name and sort");
    if (Schema.ScalarSortConflict)
      return InvalidQueryResult("query name is used with incompatible sorts");
    if (Schema.ScalarIdentityConflict)
      return InvalidQueryResult(
          "query contains distinct scalar symbols with the same name");
    if (DeadlineExpired())
      return TimeoutResult(
          "wall-time deadline expired during parent-side query preparation");
    char ResultPath[] = "/tmp/formal-stp-result-XXXXXX";
    int ResultFile = ::mkstemp(ResultPath);
    if (ResultFile < 0)
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "cannot create STP worker result file", Arrays);

    char RequestPath[] = "/tmp/formal-stp-request-XXXXXX";
    int RequestFile = ::mkstemp(RequestPath);
    bool RequestWritten = RequestFile >= 0;
    if (RequestWritten) {
      try {
        RequestWritten = writeWorkerRequest(
            RequestFile, Kind, Constraints, Target, Arrays, Options, QueryId,
            ContextId, Schema);
      } catch (...) {
        RequestWritten = false;
      }
    }
    struct stat RequestStat {};
    if (!RequestWritten || ::fstat(RequestFile, &RequestStat) != 0 ||
        RequestStat.st_size < 0 ||
        static_cast<std::uint64_t>(RequestStat.st_size) >
            Options.MaxWorkerFileBytes ||
        ::lseek(RequestFile, 0, SEEK_SET) < 0) {
      if (RequestFile >= 0)
        ::close(RequestFile);
      ::unlink(RequestPath);
      ::close(ResultFile);
      ::unlink(ResultPath);
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "cannot serialize STP worker request", Arrays);
    }
    if (DeadlineExpired()) {
      ::close(RequestFile);
      ::unlink(RequestPath);
      ::close(ResultFile);
      ::unlink(ResultPath);
      return TimeoutResult(
          "wall-time deadline expired during worker request serialization");
    }

    const std::string RequestDescriptor = std::to_string(RequestFile);
    const std::string ResultDescriptor = std::to_string(ResultFile);
    char *const WorkerArguments[] = {
        const_cast<char *>(FORMAL_STP_WORKER_EXECUTABLE),
        const_cast<char *>(RequestDescriptor.c_str()),
        const_cast<char *>(ResultDescriptor.c_str()), nullptr};

    pid_t Worker = ::fork();
    if (Worker < 0) {
      ::close(RequestFile);
      ::unlink(RequestPath);
      ::close(ResultFile);
      ::unlink(ResultPath);
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "cannot start STP worker process", Arrays);
    }
    if (Worker == 0) {
      // Ignored signal dispositions survive exec. Restore the resource-limit
      // signals and unblock them so an embedding host cannot turn an RLIMIT
      // termination into a recoverable write/CPU error and a reserved worker
      // exit code.
      sigset_t ResourceSignals;
      const bool SignalsReady = ::sigemptyset(&ResourceSignals) == 0 &&
                                ::sigaddset(&ResourceSignals, SIGXFSZ) == 0 &&
                                ::sigaddset(&ResourceSignals, SIGXCPU) == 0;
      if (!SignalsReady ||
          ::sigprocmask(SIG_UNBLOCK, &ResourceSignals, nullptr) != 0 ||
          ::signal(SIGXFSZ, SIG_DFL) == SIG_ERR ||
          ::signal(SIGXCPU, SIG_DFL) == SIG_ERR ||
          !closeWorkerDescriptorsExcept(RequestFile, ResultFile) ||
          !applyWorkerLimits(Options) || ::ftruncate(ResultFile, 0) != 0 ||
          ::lseek(ResultFile, 0, SEEK_SET) < 0 ||
          ::lseek(RequestFile, 0, SEEK_SET) < 0)
        ::_exit(WorkerInternalExceptionExitCode);
      ::execv(FORMAL_STP_WORKER_EXECUTABLE, WorkerArguments);
      ::_exit(WorkerExecFailureExitCode);
    }
    ::close(RequestFile);
    ::unlink(RequestPath);

    int WorkerStatus = 0;
    bool TimedOut = false;
    bool WorkerObservedAfterDeadline = false;
    while (true) {
      pid_t Waited = ::waitpid(Worker, &WorkerStatus, WNOHANG);
      if (Waited == Worker) {
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
        if ((WorkerTestMode.load() == 22 || WorkerTestMode.load() == 28) &&
            Deadline)
          advanceSolverTestClock(*Deadline - solverClockNow());
#endif
        WorkerObservedAfterDeadline = DeadlineExpired();
        break;
      }
      if (Waited < 0 && errno != EINTR) {
        ::kill(Worker, SIGKILL);
        ::waitpid(Worker, &WorkerStatus, 0);
        ::close(ResultFile);
        ::unlink(ResultPath);
        return workerFailure(Kind, Constraints, Target, QueryId, Started,
                             "cannot wait for STP worker process", Arrays);
      }
      if (Waited == 0 && DeadlineExpired()) {
        ::kill(Worker, SIGKILL);
        pid_t Reaped = -1;
        do {
          Reaped = ::waitpid(Worker, &WorkerStatus, 0);
        } while (Reaped < 0 && errno == EINTR);
        if (Reaped != Worker) {
          ::close(ResultFile);
          ::unlink(ResultPath);
          return workerFailure(Kind, Constraints, Target, QueryId, Started,
                               "cannot reap timed-out STP worker process",
                               Arrays);
        }
        // The worker may have exited normally between WNOHANG and SIGKILL.
        // A complete response must still be read so InternalError cannot be
        // hidden by this race. Only a worker actually terminated by the
        // deadline is classified immediately as TIMEOUT.
        if (WIFEXITED(WorkerStatus)) {
          WorkerObservedAfterDeadline = true;
        } else {
          TimedOut = true;
        }
        break;
      }
      std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }

    if (TimedOut) {
      ::close(ResultFile);
      ::unlink(ResultPath);
      return TimeoutResult(
          "STP worker was killed at the configured wall-time deadline");
    }

    if (!WIFEXITED(WorkerStatus)) {
      ::close(ResultFile);
      ::unlink(ResultPath);
      const std::string Detail =
          WIFSIGNALED(WorkerStatus)
              ? "STP worker process terminated by signal " +
                    std::to_string(WTERMSIG(WorkerStatus))
              : "STP worker process terminated without an exit status";
      return workerTermination(Kind, Constraints, Target, QueryId, Started,
                               Detail, Arrays);
    }
    const int WorkerExitCode = WEXITSTATUS(WorkerStatus);
    if (WorkerExitCode != 0) {
      ::close(ResultFile);
      ::unlink(ResultPath);
      std::string Detail = "STP worker process terminated with exit code " +
                           std::to_string(WorkerExitCode);
      switch (WorkerExitCode) {
      case WorkerRequestReadFailureExitCode:
        Detail += " while parsing the worker request";
        break;
      case WorkerInternalExceptionExitCode:
        Detail += " after an internal worker exception or setup failure";
        break;
      case WorkerResponseWriteFailureExitCode:
        Detail += " while writing the worker response";
        break;
      case WorkerExecFailureExitCode:
        Detail += " because the worker executable could not be started";
        break;
      default:
        return workerTermination(Kind, Constraints, Target, QueryId, Started,
                                 std::move(Detail), Arrays);
      }
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           std::move(Detail), Arrays);
    }
    if (::lseek(ResultFile, 0, SEEK_SET) < 0) {
      ::close(ResultFile);
      ::unlink(ResultPath);
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "cannot read STP worker result file", Arrays);
    }

    SolverResult Result;
    struct stat ResultStat {};
    bool Read = ::fstat(ResultFile, &ResultStat) == 0 &&
                ResultStat.st_size >= 0 &&
                static_cast<std::uint64_t>(ResultStat.st_size) <=
                    Options.MaxWorkerResponseBytes &&
                ::lseek(ResultFile, 0, SEEK_SET) >= 0;
    if (Read) {
      try {
        Read = readSolverResult(ResultFile, Schema, Result);
      } catch (...) {
        Read = false;
      }
    }
    ::close(ResultFile);
    ::unlink(ResultPath);
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    if (WorkerTestMode.load() == 24 && Deadline)
      advanceSolverTestClock(*Deadline - solverClockNow());
#endif
    if (!Read)
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "STP worker returned malformed data", Arrays);
    if (Result.Metadata.QueryId != QueryId ||
        Result.Metadata.Translation.ContextId != ContextId ||
        Result.Metadata.BackendName != "STP" ||
        Result.Metadata.BackendVersion != Version ||
        Result.Metadata.SatBackend != "MiniSat")
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "STP worker returned inconsistent metadata", Arrays);
    if (!validateResultEnvelope(Result, Schema))
      return workerFailure(Kind, Constraints, Target, QueryId, Started,
                           "STP worker returned an inconsistent result envelope",
                           Arrays);
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    if ((WorkerTestMode.load() == 26 || WorkerTestMode.load() == 27) && Deadline)
      advanceSolverTestClock(*Deadline - solverClockNow());
#endif

    if (Result.Status == SolverStatus::Sat) {
      std::string ValidationError;
      bool Valid = Result.SatisfyingModel.has_value() &&
                   validateModel(Constraints, *Result.SatisfyingModel,
                                 ValidationError);
      if (!Result.SatisfyingModel)
        ValidationError = "STP worker omitted the satisfying model";
      const bool AssertTarget = Kind == QueryKind::MayBeTrue ||
                                Kind == QueryKind::MustBeTrue;
      const bool NegateTarget = Kind == QueryKind::MustBeTrue;
      if (Valid && AssertTarget) {
        std::optional<llvm::APInt> Value =
            evaluate(Target, *Result.SatisfyingModel, ValidationError);
        Valid = Value && Value->getBitWidth() == 1 &&
                Value->getBoolValue() == !NegateTarget;
        if (Value && !Valid)
          ValidationError =
              "query target is false under the worker model";
      }
      if (Valid && Kind == QueryKind::GetValue) {
        std::optional<llvm::APInt> Value =
            evaluate(Target, *Result.SatisfyingModel, ValidationError);
        Valid = Value && Result.Value && *Value == *Result.Value;
        if (Value && !Valid)
          ValidationError =
              "worker target value disagrees with the Formal evaluator";
      }
      if (Valid && Kind == QueryKind::InitialValues) {
        for (std::size_t RequestIndex = 0;
             Valid && RequestIndex < Arrays.size(); ++RequestIndex) {
          const ArrayModelRequest &Request = Arrays[RequestIndex];
          const InitialArrayValue &Initial =
              Result.InitialValues[RequestIndex];
          if (Initial.Root.get() != Request.Root.get() ||
              Initial.Bytes.size() != Request.ByteCount) {
            Valid = false;
            ValidationError =
                "worker initial-array identity or byte count is inconsistent";
            break;
          }
          for (std::size_t ByteIndex = 0;
               ByteIndex < Initial.Bytes.size(); ++ByteIndex) {
            llvm::APInt Index(Request.Root->sort().indexWidth(), ByteIndex);
            const ArrayElementValue *Found = nullptr;
            for (const ArrayElementValue &Element :
                 Result.SatisfyingModel->ArrayElements) {
              if (Element.Root.get() != Request.Root.get() ||
                  Element.Index != Index)
                continue;
              Found = &Element;
              break;
            }
            if (!Found || Found->Value != Initial.Bytes[ByteIndex]) {
              Valid = false;
              ValidationError =
                  "worker initial byte disagrees with the returned array model";
              break;
            }
          }
        }
      }
      if (Valid && Options.AdditionalModelAudit) {
        try {
          std::optional<std::string> AuditFailure =
              Options.AdditionalModelAudit(Constraints,
                                           *Result.SatisfyingModel);
          if (AuditFailure) {
            Valid = false;
            ValidationError = *AuditFailure;
          }
        } catch (const std::exception &Error) {
          Valid = false;
          ValidationError =
              "additional model audit threw an exception: " +
              std::string(Error.what());
        } catch (...) {
          Valid = false;
          ValidationError =
              "additional model audit threw a non-standard exception";
        }
      }
      if (!Valid) {
        Result.Status = SolverStatus::InternalError;
        Result.Metadata.Reason = SolverReason::ModelValidationFailed;
        Result.Metadata.Detail = ValidationError;
        Result.Truth.reset();
        Result.Value.reset();
        Result.SatisfyingModel.reset();
        Result.InitialValues.clear();
        saveQuery(Result, Kind, Constraints, Target, AssertTarget,
                  NegateTarget, Arrays);
      }
    }
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    if (WorkerTestMode.load() == 25 && Deadline)
      advanceSolverTestClock(*Deadline - solverClockNow());
#endif
    const auto Completed = solverClockNow();
    if (Result.Status != SolverStatus::InternalError && Deadline &&
        Completed >= *Deadline)
      return TimeoutResult(
          WorkerObservedAfterDeadline
              ? "STP worker completed, but completion was observed at or "
                "after the total wall-time deadline"
              : "wall-time deadline expired during parent-side result "
                "validation");
    Result.Metadata.Elapsed = Completed - Started;
    return Result;
  }

  SolverResult runInProcess(QueryKind Kind, const ConstraintSet &Constraints,
                            const ExprRef &Target,
                            const std::vector<ArrayModelRequest> &Arrays,
                            std::uint64_t QueryId,
                            std::uint64_t ContextId) {
    SolverResult Result;
    Result = baseResult(QueryId);
    Result.Metadata.Translation.ContextId = ContextId;
    const auto Started = std::chrono::steady_clock::now();
    const std::optional<std::chrono::steady_clock::time_point> Deadline =
        makeDeadline(Started, Options.Timeout);
    bool AssertTarget = Kind == QueryKind::MayBeTrue ||
                        Kind == QueryKind::MustBeTrue;
    bool NegateTarget = Kind == QueryKind::MustBeTrue;
    if (Options.Timeout.count() == 0) {
      Result.Status = SolverStatus::Timeout;
      Result.Metadata.Reason = SolverReason::Timeout;
      Result.Metadata.Detail =
          "zero timeout expired before STP context initialization";
      Result.Metadata.Elapsed = std::chrono::steady_clock::now() - Started;
      return Result;
    }

    try {
      for (const ArrayModelRequest &Request : Arrays)
        if (std::optional<std::string> Error =
                arrayRequestError(Request, Options.MaxArrayBytes))
          throw std::invalid_argument(*Error);
      QueryContext Context(ContextId);
      ::Expr TranslatedTarget = nullptr;
      for (std::size_t Index = 0; Index < Constraints.size(); ++Index) {
        if (!Constraints[Index] || !Constraints[Index]->sort().isBool())
          throw std::invalid_argument("constraint " + std::to_string(Index) +
                                      " must have Bool sort");
        vc_assertFormula(Context.vc(), Context.translate(Constraints[Index]));
      }
      if (Target) {
        if (AssertTarget && !Target->sort().isBool())
          throw std::invalid_argument("truth-query target must have Bool sort");
        if (Kind == QueryKind::GetValue && Target->sort().isArray())
          throw std::invalid_argument("getValue target must have scalar sort");
        TranslatedTarget = Context.translate(Target);
        if (AssertTarget) {
          if (NegateTarget)
            TranslatedTarget =
                Context.own(vc_notExpr(Context.vc(), TranslatedTarget));
          vc_assertFormula(Context.vc(), TranslatedTarget);
        }
      }
      for (const ArrayModelRequest &Request : Arrays) {
        if (!Request.Root)
          throw std::invalid_argument("array model request has a null root");
        Context.translateArray(Request.Root);
      }

      if (Options.Timeout.count() == 0) {
        Result.Status = SolverStatus::Timeout;
        Result.Metadata.Reason = SolverReason::Timeout;
        Result.Metadata.Detail = "zero timeout expired before backend query";
      } else {
        ::Expr False = Context.own(vc_falseExpr(Context.vc()));
        int BackendResult = 0;
        try {
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
          switch (WorkerTestMode.load()) {
          case 1:
            BackendResult = 99;
            break;
          case 2:
            ::_exit(77);
          case 3:
            BackendResult = 2;
            break;
          case 4:
            throw std::runtime_error("injected STP backend exception");
          case 23:
            std::this_thread::sleep_for(std::chrono::seconds(5));
            BackendResult = 0;
            break;
          default:
            BackendResult = Options.MaxConflicts == 0
                                ? vc_query(Context.vc(), False)
                                : vc_query_with_timeout(
                                      Context.vc(), False,
                                      static_cast<int>(Options.MaxConflicts),
                                      -1);
            break;
          }
#else
          BackendResult = Options.MaxConflicts == 0
                              ? vc_query(Context.vc(), False)
                              : vc_query_with_timeout(
                                    Context.vc(), False,
                                    static_cast<int>(Options.MaxConflicts), -1);
#endif
        } catch (const std::exception &Error) {
          Result.Status = SolverStatus::InternalError;
          Result.Metadata.Reason = SolverReason::BackendFailure;
          Result.Metadata.Detail =
              "STP backend query threw an exception: " +
              std::string(Error.what());
          BackendResult = -1;
        } catch (...) {
          Result.Status = SolverStatus::InternalError;
          Result.Metadata.Reason = SolverReason::BackendFailure;
          Result.Metadata.Detail =
              "STP backend query threw a non-standard exception";
          BackendResult = -1;
        }
        if (BackendResult == -1) {
          // The exception path above has already populated a fail-closed
          // result and must not be reclassified as UNKNOWN.
        } else {
          switch (BackendResult) {
          case 0:
            Result.Status = SolverStatus::Sat;
            Result.Metadata.Reason = SolverReason::Satisfiable;
            break;
          case 1:
            Result.Status = SolverStatus::Unsat;
            Result.Metadata.Reason = SolverReason::Unsatisfiable;
            break;
          case 3:
            Result.Status = SolverStatus::Timeout;
            Result.Metadata.Reason = SolverReason::Timeout;
            Result.Metadata.Detail = "STP timeout/conflict limit reached";
            break;
          case 2:
            Result.Status = SolverStatus::InternalError;
            Result.Metadata.Reason = SolverReason::BackendFailure;
            Result.Metadata.Detail = "STP reported a backend error";
            break;
          default:
            Result.Status = SolverStatus::Unknown;
            Result.Metadata.Reason = SolverReason::BackendUnknown;
            Result.Metadata.Detail = "STP returned unknown code " +
                                     std::to_string(BackendResult);
            break;
          }
        }
      }

      if ((Result.Status == SolverStatus::Sat ||
           Result.Status == SolverStatus::Unsat) &&
          Deadline && std::chrono::steady_clock::now() > *Deadline) {
        Result.Status = SolverStatus::Timeout;
        Result.Metadata.Reason = SolverReason::Timeout;
        Result.Metadata.Detail =
            "STP completed after the configured wall-time deadline";
      }

      if (Result.Status == SolverStatus::Sat) {
        Model Assignment = Context.extractModel(
            Arrays, Result.InitialValues, Options.MaxArrayBytes);
        std::string ValidationError;
        if (!validateModel(Constraints, Assignment, ValidationError)) {
          Result.Status = SolverStatus::InternalError;
          Result.Metadata.Reason = SolverReason::ModelValidationFailed;
          Result.Metadata.Detail = ValidationError;
        }
        if (Result.Status == SolverStatus::Sat && AssertTarget) {
          std::optional<llvm::APInt> Value =
              evaluate(Target, Assignment, ValidationError);
          bool Expected = !NegateTarget;
          if (!Value || Value->getBitWidth() != 1 ||
              Value->getBoolValue() != Expected) {
            Result.Status = SolverStatus::InternalError;
            Result.Metadata.Reason = SolverReason::ModelValidationFailed;
            Result.Metadata.Detail = Value
                ? "query target is false under the returned model"
                : ValidationError;
          }
        }
        if (Result.Status == SolverStatus::Sat) {
          if (Kind == QueryKind::GetValue) {
            Result.Value =
                Context.modelValue(TranslatedTarget, Target->sort());
            std::optional<llvm::APInt> AuditedValue =
                evaluate(Target, Assignment, ValidationError);
            if (!AuditedValue) {
              Result.Status = SolverStatus::InternalError;
              Result.Metadata.Reason = SolverReason::ModelValidationFailed;
              Result.Metadata.Detail = ValidationError;
            } else if (*AuditedValue != *Result.Value) {
              Result.Status = SolverStatus::InternalError;
              Result.Metadata.Reason = SolverReason::ModelValidationFailed;
              Result.Metadata.Detail =
                  "STP target value disagrees with the Formal model evaluator";
            }
          }
          if (Result.Status == SolverStatus::Sat)
            Result.SatisfyingModel = std::move(Assignment);
        }
      }

      if (Kind == QueryKind::MayBeTrue)
        Result.Truth = Result.Status == SolverStatus::Sat
                           ? std::optional<bool>(true)
                           : Result.Status == SolverStatus::Unsat
                                 ? std::optional<bool>(false)
                                 : std::nullopt;
      if (Kind == QueryKind::MustBeTrue)
        Result.Truth = Result.Status == SolverStatus::Unsat
                           ? std::optional<bool>(true)
                           : Result.Status == SolverStatus::Sat
                                 ? std::optional<bool>(false)
                                 : std::nullopt;
      Result.Metadata.Translation = Context.statistics();
    } catch (const std::invalid_argument &Error) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::InvalidQuery;
      Result.Metadata.Detail = Error.what();
    } catch (const std::length_error &Error) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::TranslationError;
      Result.Metadata.Detail = Error.what();
    } catch (const std::exception &Error) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::TranslationError;
      Result.Metadata.Detail = Error.what();
    } catch (...) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::BackendFailure;
      Result.Metadata.Detail = "non-standard exception at STP boundary";
    }

    if (Result.Status != SolverStatus::Sat) {
      Result.Value.reset();
      Result.SatisfyingModel.reset();
      Result.InitialValues.clear();
      if (Result.Status != SolverStatus::Unsat)
        Result.Truth.reset();
    }

    Result.Metadata.Elapsed = std::chrono::steady_clock::now() - Started;
    if (Options.ExportAllQueries || Result.Status == SolverStatus::Unknown ||
        Result.Status == SolverStatus::Timeout ||
        Result.Status == SolverStatus::InternalError)
      saveQuery(Result, Kind, Constraints, Target, AssertTarget, NegateTarget,
                Arrays);
    return Result;
  }

  SolverResult baseResult(std::uint64_t QueryId) const {
    SolverResult Result;
    Result.Metadata.QueryId = QueryId;
    Result.Metadata.BackendName = "STP";
    Result.Metadata.BackendVersion = Version;
    Result.Metadata.SatBackend = "MiniSat";
    return Result;
  }

  SolverResult workerFailure(QueryKind Kind, const ConstraintSet &Constraints,
                             const ExprRef &Target, std::uint64_t QueryId,
                             std::chrono::steady_clock::time_point Started,
                             std::string Detail,
                             const std::vector<ArrayModelRequest> &Arrays) const {
    SolverResult Result = baseResult(QueryId);
    Result.Status = SolverStatus::InternalError;
    Result.Metadata.Reason = SolverReason::BackendFailure;
    Result.Metadata.Detail = std::move(Detail);
    Result.Metadata.Elapsed = solverClockNow() - Started;
    bool AssertTarget = Kind == QueryKind::MayBeTrue ||
                        Kind == QueryKind::MustBeTrue;
    saveQuery(Result, Kind, Constraints, Target, AssertTarget,
              Kind == QueryKind::MustBeTrue, Arrays);
    return Result;
  }

  SolverResult workerTermination(
      QueryKind Kind, const ConstraintSet &Constraints, const ExprRef &Target,
      std::uint64_t QueryId,
      std::chrono::steady_clock::time_point Started,
      std::string Detail,
      const std::vector<ArrayModelRequest> &Arrays) const {
    SolverResult Result = baseResult(QueryId);
    Result.Status = SolverStatus::Unknown;
    Result.Metadata.Reason = SolverReason::WorkerFailure;
    Result.Metadata.Detail = std::move(Detail);
    Result.Metadata.Elapsed = solverClockNow() - Started;
    const bool AssertTarget = Kind == QueryKind::MayBeTrue ||
                              Kind == QueryKind::MustBeTrue;
    saveQuery(Result, Kind, Constraints, Target, AssertTarget,
              Kind == QueryKind::MustBeTrue, Arrays);
    return Result;
  }

  void saveQuery(SolverResult &Result, QueryKind Kind,
                 const ConstraintSet &Constraints, const ExprRef &Target,
                 bool AssertTarget, bool NegateTarget,
                 const std::vector<ArrayModelRequest> &Arrays = {}) const {
    try {
      std::filesystem::create_directories(Options.FailureQueryDirectory);
      std::ostringstream FileStem;
      FileStem << "query-" << std::setw(8) << std::setfill('0')
               << Result.Metadata.QueryId << '-'
               << toReasonCode(Result.Metadata.Reason) << "-%%%%%%.tmp";
      SMTLibQuery Query;
      Query.Constraints = Constraints;
      Query.Target = Target;
      Query.AssertTarget = AssertTarget;
      Query.NegateTarget = NegateTarget;
      for (const ArrayModelRequest &Request : Arrays)
        Query.ArrayRequests.push_back({Request.Root, Request.ByteCount});
      Query.Metadata = {
          {"query-id", std::to_string(Result.Metadata.QueryId)},
          {"query-kind", queryKindName(Kind)},
          {"backend", Result.Metadata.BackendName},
          {"backend-version", Result.Metadata.BackendVersion},
          {"sat-backend", Result.Metadata.SatBackend},
          {"reason-code", toReasonCode(Result.Metadata.Reason)},
          {"timeout-ms", std::to_string(Options.Timeout.count())},
          {"max-conflicts", std::to_string(Options.MaxConflicts)},
          {"context-policy", "fresh-context-per-query"},
      };
      std::string Text = SMTLibWriter::writeQuery(Query);
      int FileDescriptor = -1;
      llvm::SmallString<256> UniquePath;
      std::filesystem::path Pattern =
          Options.FailureQueryDirectory / FileStem.str();
      std::error_code CreateError = llvm::sys::fs::createUniqueFile(
          Pattern.string(), FileDescriptor, UniquePath);
      if (CreateError)
        throw std::runtime_error("cannot create unique query artifact: " +
                                 CreateError.message());
      llvm::raw_fd_ostream Output(FileDescriptor, true);
      Output << Text;
      Output.flush();
      if (Output.has_error()) {
        llvm::sys::fs::remove(UniquePath);
        throw std::runtime_error("cannot write temporary query artifact");
      }
      Output.close();
      std::filesystem::path Temporary(UniquePath.str().str());
      std::filesystem::path Final = Temporary;
      Final.replace_extension(".smt2");
      std::filesystem::rename(Temporary, Final);
      Result.Metadata.SavedQuery = std::move(Final);
    } catch (const std::exception &Error) {
      std::string Original = Result.Metadata.Detail;
      SolverReason OriginalReason = Result.Metadata.Reason;
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::ArtifactWriteFailed;
      Result.Truth.reset();
      Result.Value.reset();
      Result.SatisfyingModel.reset();
      Result.InitialValues.clear();
      Result.Metadata.Detail = "query export failed: " + std::string(Error.what());
      if (!Original.empty())
        Result.Metadata.Detail += "; original error: " + Original;
      Result.Metadata.Detail += "; original reason: " +
                                std::string(toReasonCode(OriginalReason));
    }
  }

  STPSolverOptions Options;
  std::string Version;
  std::string CompilationProfile;
};

int detail::runSTPWorkerProcess(int RequestFileDescriptor,
                                int ResultFileDescriptor) {
  try {
    WorkerRequest Request;
    if (!readWorkerRequest(RequestFileDescriptor, Request))
      return WorkerRequestReadFailureExitCode;
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    WorkerTestMode.store(Request.TestMode);
    if (Request.TestMode == 30)
      throw std::runtime_error("injected internal worker exception");
#endif
    STPSolver::Impl Implementation(Request.Options);
    WireSchema Schema = makeWireSchema(Request.Kind, Request.Constraints,
                                       Request.Target, Request.Arrays);
    SolverResult Result = Implementation.runInProcess(
        Request.Kind, Request.Constraints, Request.Target, Request.Arrays,
        Request.QueryId, Request.ContextId);
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    if (Request.TestMode == 5) {
      Result.Status = SolverStatus::Unsat;
      Result.Metadata.Reason = SolverReason::Satisfiable;
    }
    if (Request.TestMode == 13) {
      Result.Status = SolverStatus::Unsat;
      Result.Metadata.Reason = SolverReason::Unsatisfiable;
    }
    if (Request.TestMode == 14)
      Result.Metadata.Reason = SolverReason::Unsatisfiable;
    if (Request.TestMode == 15) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::BackendFailure;
    }
    if (Request.TestMode >= 1000 &&
        ::fcntl(Request.TestMode - 1000, F_GETFD) >= 0) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::BackendFailure;
      Result.Metadata.Detail = "worker inherited an unrelated host descriptor";
      Result.Truth.reset();
      Result.Value.reset();
      Result.SatisfyingModel.reset();
      Result.InitialValues.clear();
    }
    if (Request.TestMode == 27) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::TranslationError;
      Result.Metadata.Detail = "injected worker translation failure";
      Result.Truth.reset();
      Result.Value.reset();
      Result.SatisfyingModel.reset();
      Result.InitialValues.clear();
    }
    if (Request.TestMode == 28) {
      Result.Status = SolverStatus::InternalError;
      Result.Metadata.Reason = SolverReason::TranslationError;
      Result.Metadata.Detail =
          "injected worker failure completed before late parent polling";
      Result.Truth.reset();
      Result.Value.reset();
      Result.SatisfyingModel.reset();
      Result.InitialValues.clear();
    }
    if (Request.TestMode == 17)
      Result.Truth = true;
    if (Request.TestMode == 18)
      Result.SatisfyingModel.reset();
    if ((Request.TestMode == 21 || Request.TestMode == 25 ||
         Request.TestMode == 26) &&
        Result.SatisfyingModel &&
        !Result.SatisfyingModel->Scalars.empty()) {
      llvm::APInt &Value = Result.SatisfyingModel->Scalars.front().Value;
      Value ^= llvm::APInt(Value.getBitWidth(), 1);
    }
    if (Request.TestMode == 32 && Result.SatisfyingModel &&
        !Result.SatisfyingModel->Scalars.empty()) {
      auto Missing = std::max_element(
          Result.SatisfyingModel->Scalars.begin(),
          Result.SatisfyingModel->Scalars.end(),
          [](const ScalarModelValue &Left, const ScalarModelValue &Right) {
            return Left.Name < Right.Name;
          });
      Result.SatisfyingModel->Scalars.erase(Missing);
    }
    if (Request.TestMode == 7 && !Result.InitialValues.empty() &&
        !Result.InitialValues.front().Bytes.empty())
      Result.InitialValues.front().Bytes.front() ^= 0xffU;
    if (Request.TestMode == 9 && !Result.InitialValues.empty())
      Result.InitialValues.pop_back();
    if (Request.TestMode == 10 && !Result.InitialValues.empty() &&
        !Schema.Arrays.empty())
      Result.InitialValues.front().Root = Schema.Arrays.front();
    if (Request.TestMode == 31)
      ::close(ResultFileDescriptor);
#endif
    bool Written = writeSolverResult(ResultFileDescriptor, Schema, Result);
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    if (Written && Request.TestMode == 20) {
      const unsigned char Padding[1024] = {};
      for (unsigned Index = 0; Written && Index != 16; ++Index)
        Written = ::write(ResultFileDescriptor, Padding, sizeof(Padding)) ==
                  static_cast<ssize_t>(sizeof(Padding));
    }
    if (!Written && Request.TestMode == 20) {
      // Some hosts report the RLIMIT_FSIZE violation only as EFBIG. Make the
      // resource-termination fault injection deterministic after the limit
      // has actually rejected the write.
      ::raise(SIGXFSZ);
      ::_exit(77);
    }
    if (Written && (Request.TestMode == 8 || Request.TestMode == 24)) {
      const std::uint8_t Trailing = 0xa5;
      Written = ::write(ResultFileDescriptor, &Trailing, 1) == 1;
    }
    WorkerTestMode.store(0);
#endif
    return Written ? 0 : WorkerResponseWriteFailureExitCode;
  } catch (...) {
#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
    WorkerTestMode.store(0);
#endif
    return WorkerInternalExceptionExitCode;
  }
}

STPSolver::STPSolver(STPSolverOptions Options)
    : Implementation(std::make_unique<Impl>(std::move(Options))) {}

STPSolver::~STPSolver() = default;

#ifdef FORMAL_SOLVER_ENABLE_TEST_FAULTS
void setSTPSolverWorkerTestMode(int Mode) {
  WorkerTestMode.store(Mode);
  const bool UsesDeterministicDeadline = Mode == 22 || Mode == 24 ||
                                         Mode == 25 || Mode == 26 ||
                                         Mode == 27 || Mode == 28;
  using Clock = std::chrono::steady_clock;
  const auto Base = std::chrono::duration_cast<std::chrono::nanoseconds>(
                        Clock::now().time_since_epoch())
                        .count();
  WorkerTestClockNanoseconds.store(Base);
  WorkerTestClockEnabled.store(UsesDeterministicDeadline);
}

void advanceSTPSolverWorkerTestClock(std::chrono::milliseconds Amount) {
  if (Amount.count() > 0)
    advanceSolverTestClock(Amount);
}

bool isPinnedSTPCompilationProfileForTest(const std::string &Profile) {
  return stpCompilationProfileMismatch(Profile).empty();
}
#endif

SolverResult STPSolver::check(const ConstraintSet &Constraints) {
  return Implementation->run(QueryKind::Check, Constraints, nullptr, {});
}

SolverResult STPSolver::mayBeTrue(const ConstraintSet &Constraints,
                                  const ExprRef &Expression) {
  return Implementation->run(QueryKind::MayBeTrue, Constraints, Expression,
                             {});
}

SolverResult STPSolver::mustBeTrue(const ConstraintSet &Constraints,
                                   const ExprRef &Expression) {
  return Implementation->run(QueryKind::MustBeTrue, Constraints, Expression,
                             {});
}

SolverResult STPSolver::getValue(const ConstraintSet &Constraints,
                                 const ExprRef &Expression) {
  return Implementation->run(QueryKind::GetValue, Constraints, Expression, {});
}

SolverResult STPSolver::getInitialValues(
    const ConstraintSet &Constraints,
    const std::vector<ArrayModelRequest> &Arrays) {
  return Implementation->run(QueryKind::InitialValues, Constraints, nullptr,
                             Arrays);
}

std::string STPSolver::backendVersion() const {
  return Implementation->Version;
}

std::string STPSolver::backendCompilationProfile() const {
  return Implementation->CompilationProfile;
}

std::string STPSolver::satBackend() const { return "MiniSat"; }

} // namespace formal
