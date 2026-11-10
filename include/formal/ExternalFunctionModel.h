#ifndef FORMAL_EXTERNAL_FUNCTION_MODEL_H
#define FORMAL_EXTERNAL_FUNCTION_MODEL_H

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include <array>
#include <cstddef>

namespace formal {

enum class ExternalModelKind { Heap, ByteMemory, Termination };
enum class ExternalArgumentKind { None, Pointer, SizeT, Int32 };
enum class ExternalReturnKind { Void, Pointer };

// The catalog is the shared preflight/executor contract. Ranges and effects
// describe the exact objects handled by the corresponding executor branch.
struct ExternalFunctionModel final {
  const char *Symbol;
  ExternalModelKind Kind;
  std::array<ExternalArgumentKind, 4> Arguments;
  std::size_t Arity;
  ExternalReturnKind Return;
  const char *ReadRange;
  const char *WriteRange;
  const char *AllocationEffect;
  const char *ReleaseEffect;
  const char *NormalPath;
  const char *FailurePath;
  const char *PointerShadowEffect;
  const char *InitializationEffect;
  bool SupportsSymbolicSize;
};

inline constexpr std::array<ExternalFunctionModel, 15> ExternalModels{{
    {"malloc", ExternalModelKind::Heap, {ExternalArgumentKind::SizeT}, 1,
     ExternalReturnKind::Pointer, "none", "none", "fresh heap object", "none",
     "fresh base pointer", "null when enabled", "empty", "uninitialized", false},
    {"calloc", ExternalModelKind::Heap,
     {ExternalArgumentKind::SizeT, ExternalArgumentKind::SizeT}, 2,
     ExternalReturnKind::Pointer, "none", "new object [0,count*size)",
     "fresh heap object", "none", "fresh base pointer", "null when enabled",
     "empty", "zero initialized", false},
    {"realloc", ExternalModelKind::Heap,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::SizeT}, 2,
     ExternalReturnKind::Pointer, "old object [0,min(old,new))",
     "new object [0,min(old,new))", "fresh heap object",
     "old heap object on success", "fresh base pointer",
     "null and old object live when enabled", "copy complete shadows",
     "copy known masks", false},
    {"free", ExternalModelKind::Heap, {ExternalArgumentKind::Pointer}, 1,
     ExternalReturnKind::Void, "none", "none", "none", "heap base or null",
     "void", "invalid/double free error", "retain dead shadow", "retain dead bytes", false},
    {"memcpy", ExternalModelKind::ByteMemory,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::Pointer,
      ExternalArgumentKind::SizeT}, 3,
     ExternalReturnKind::Pointer, "arg1 [0,arg2)", "arg0 [0,arg2)", "none", "none",
     "arg0", "overlap error", "copy complete shadows", "copy known masks", false},
    {"memmove", ExternalModelKind::ByteMemory,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::Pointer,
      ExternalArgumentKind::SizeT}, 3,
     ExternalReturnKind::Pointer, "arg1 [0,arg2)", "arg0 [0,arg2)", "none", "none",
     "arg0", "memory error", "copy complete shadows", "copy known masks", false},
    {"memset", ExternalModelKind::ByteMemory,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::Int32,
      ExternalArgumentKind::SizeT}, 3,
     ExternalReturnKind::Pointer, "none", "arg0 [0,arg2)", "none", "none",
     "arg0", "memory error", "clear overwritten shadows", "initialized", false},
    {"abort", ExternalModelKind::Termination, {}, 0,
     ExternalReturnKind::Void, "none", "none", "none", "none", "error only",
     "no normal return", "none", "none", false},
    {"__assert_fail", ExternalModelKind::Termination,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::Pointer,
      ExternalArgumentKind::Int32, ExternalArgumentKind::Pointer}, 4,
     ExternalReturnKind::Void, "argument strings", "none", "none", "none",
     "error only", "no normal return", "none", "none", false},
    {"_Znwm", ExternalModelKind::Heap, {ExternalArgumentKind::SizeT}, 1,
     ExternalReturnKind::Pointer, "none", "none", "fresh new object", "none",
     "fresh base pointer", "UNKNOWN exception/termination", "empty", "uninitialized", false},
    {"_Znam", ExternalModelKind::Heap, {ExternalArgumentKind::SizeT}, 1,
     ExternalReturnKind::Pointer, "none", "none", "fresh new object", "none",
     "fresh base pointer", "UNKNOWN exception/termination", "empty", "uninitialized", false},
    {"_ZdlPv", ExternalModelKind::Heap, {ExternalArgumentKind::Pointer}, 1,
     ExternalReturnKind::Void, "none", "none", "none", "new base or null",
     "void", "invalid/double delete error", "retain dead shadow", "retain dead bytes", false},
    {"_ZdaPv", ExternalModelKind::Heap, {ExternalArgumentKind::Pointer}, 1,
     ExternalReturnKind::Void, "none", "none", "none", "new base or null",
     "void", "invalid/double delete error", "retain dead shadow", "retain dead bytes", false},
    {"_ZdlPvm", ExternalModelKind::Heap,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::SizeT}, 2,
     ExternalReturnKind::Void, "none", "none", "none", "new base or null",
     "void", "invalid/double delete error", "retain dead shadow", "retain dead bytes", false},
    {"_ZdaPvm", ExternalModelKind::Heap,
     {ExternalArgumentKind::Pointer, ExternalArgumentKind::SizeT}, 2,
     ExternalReturnKind::Void, "none", "none", "none", "new base or null",
     "void", "invalid/double delete error", "retain dead shadow", "retain dead bytes", false},
}};

inline const ExternalFunctionModel *findExternalFunctionModel(
    llvm::StringRef Symbol) noexcept {
  for (const ExternalFunctionModel &Model : ExternalModels)
    if (Symbol == Model.Symbol)
      return &Model;
  return nullptr;
}

inline bool matchesExternalModelSignature(const ExternalFunctionModel &Model,
                                          const llvm::CallInst &Call,
                                          const llvm::Function &Callee,
                                          const llvm::DataLayout &Layout) {
  if (Callee.isVarArg() || Call.arg_size() != Model.Arity ||
      Callee.arg_size() != Model.Arity ||
      Call.getCallingConv() != llvm::CallingConv::C ||
      Callee.getCallingConv() != llvm::CallingConv::C ||
      Call.hasOperandBundles() || Call.isNoBuiltin())
    return false;
  auto MatchesReturn = [&](llvm::Type *Type) {
    return Model.Return == ExternalReturnKind::Void
               ? Type->isVoidTy()
               : Type->isPointerTy() &&
                     Type->getPointerAddressSpace() == 0;
  };
  if (!MatchesReturn(Call.getType()) ||
      !MatchesReturn(Callee.getReturnType()))
    return false;
  for (unsigned Index = 0; Index < Model.Arity; ++Index) {
    auto MatchesArgument = [&](llvm::Type *Type) {
      switch (Model.Arguments[Index]) {
      case ExternalArgumentKind::Pointer:
        return Type->isPointerTy() && Type->getPointerAddressSpace() == 0;
      case ExternalArgumentKind::SizeT:
        return Type->isIntegerTy(Layout.getPointerSizeInBits(0));
      case ExternalArgumentKind::Int32:
        return Type->isIntegerTy(32);
      case ExternalArgumentKind::None:
        return false;
      }
      return false;
    };
    if (!MatchesArgument(Call.getArgOperand(Index)->getType()) ||
        !MatchesArgument(Callee.getArg(Index)->getType()))
      return false;
  }
  return true;
}

} // namespace formal

#endif // FORMAL_EXTERNAL_FUNCTION_MODEL_H
