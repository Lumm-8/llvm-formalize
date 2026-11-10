#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstdio>

namespace {

struct Unpacked {
  std::uint8_t Tag;
  std::uint32_t Value;
};

#pragma pack(push, 1)
struct Packed {
  std::uint8_t Tag;
  std::uint32_t Value;
};
#pragma pack(pop)

struct Nested {
  Unpacked Head;
  Packed Items[2];
};

struct Tail {
  std::uint32_t Value;
  std::uint8_t Tag;
};

union Reinterpreted {
  std::uint32_t Word;
  std::uint8_t Bytes[4];
};

bool check(const char *Name, std::uint64_t Modeled, std::uint64_t Native) {
  if (Modeled == Native)
    return true;
  std::fprintf(stderr, "%s: DataLayout=%llu native=%llu\n", Name,
               static_cast<unsigned long long>(Modeled),
               static_cast<unsigned long long>(Native));
  return false;
}

} // namespace

int main() {
  static_assert(sizeof(void *) == 8);
  static_assert(std::endian::native == std::endian::little);
  llvm::LLVMContext Context;
  // Matches the target triple and DataLayout in tests/ir/m6/basic.ll.
  llvm::DataLayout Layout("e-p:64:64-i64:64-n8:16:32:64-S128");
  llvm::Type *I8 = llvm::Type::getInt8Ty(Context);
  llvm::Type *I32 = llvm::Type::getInt32Ty(Context);
  auto *U = llvm::StructType::get(Context, {I8, I32}, false);
  auto *P = llvm::StructType::get(Context, {I8, I32}, true);
  auto *N = llvm::StructType::get(
      Context, {U, llvm::ArrayType::get(P, 2)}, false);
  auto *T = llvm::StructType::get(Context, {I32, I8}, false);
  bool OK = true;
  OK &= check("unpacked.size", Layout.getTypeAllocSize(U), sizeof(Unpacked));
  OK &= check("unpacked.align", Layout.getABITypeAlign(U).value(), alignof(Unpacked));
  OK &= check("unpacked.tag", Layout.getStructLayout(U)->getElementOffset(0),
              offsetof(Unpacked, Tag));
  OK &= check("unpacked.value", Layout.getStructLayout(U)->getElementOffset(1),
              offsetof(Unpacked, Value));
  OK &= check("unpacked.store", Layout.getTypeStoreSize(U), sizeof(Unpacked));
  OK &= check("packed.size", Layout.getTypeAllocSize(P), sizeof(Packed));
  OK &= check("packed.align", Layout.getABITypeAlign(P).value(), alignof(Packed));
  OK &= check("packed.tag", Layout.getStructLayout(P)->getElementOffset(0),
              offsetof(Packed, Tag));
  OK &= check("packed.value", Layout.getStructLayout(P)->getElementOffset(1),
              offsetof(Packed, Value));
  OK &= check("packed.store", Layout.getTypeStoreSize(P), sizeof(Packed));
  OK &= check("nested.size", Layout.getTypeAllocSize(N), sizeof(Nested));
  OK &= check("nested.align", Layout.getABITypeAlign(N).value(), alignof(Nested));
  OK &= check("nested.head", Layout.getStructLayout(N)->getElementOffset(0),
              offsetof(Nested, Head));
  OK &= check("nested.items", Layout.getStructLayout(N)->getElementOffset(1),
              offsetof(Nested, Items));
  OK &= check("nested.stride", Layout.getTypeAllocSize(P),
              offsetof(Nested, Items[1]) - offsetof(Nested, Items[0]));
  OK &= check("nested.last.value",
              Layout.getStructLayout(N)->getElementOffset(1) +
                  Layout.getTypeAllocSize(P) +
                  Layout.getStructLayout(P)->getElementOffset(1),
              offsetof(Nested, Items[1].Value));
  OK &= check("nested.store", Layout.getTypeStoreSize(N), sizeof(Nested));
  OK &= check("tail.size", Layout.getTypeAllocSize(T), sizeof(Tail));
  OK &= check("tail.value", Layout.getStructLayout(T)->getElementOffset(0),
              offsetof(Tail, Value));
  OK &= check("tail.tag", Layout.getStructLayout(T)->getElementOffset(1),
              offsetof(Tail, Tag));
  OK &= check("tail.store", Layout.getTypeStoreSize(T), sizeof(Tail));
  // LLVM has no union type. M6 reinterprets one aligned four-byte object.
  OK &= check("union.size", Layout.getTypeAllocSize(I32), sizeof(Reinterpreted));
  OK &= check("union.align", Layout.getABITypeAlign(I32).value(),
              alignof(Reinterpreted));
  OK &= check("union.word", 0, offsetof(Reinterpreted, Word));
  OK &= check("union.bytes", 0, offsetof(Reinterpreted, Bytes));
  return OK ? 0 : 1;
}
