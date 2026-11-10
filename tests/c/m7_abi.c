// Generate IR with the pinned clang-20:
// clang-20 -O0 -S -emit-llvm -Xclang -disable-O0-optnone m7_abi.c -o m7_abi.ll
struct Triple {
  long a;
  long b;
  long c;
};

struct PointerTriple {
  long *target;
  long a;
  long b;
};

__attribute__((noinline)) struct Triple make_triple(void) {
  struct Triple value = {7, 8, 9};
  return value;
}

__attribute__((noinline)) long sum_triple(struct Triple value) {
  value.a = 17;
  return value.a;
}

__attribute__((noinline)) long mutate_pointer_triple(struct PointerTriple value) {
  *value.target = 31;
  value.target = 0;
  return 2;
}

long abi_entry(void) {
  struct Triple value = make_triple();
  long mutated = sum_triple(value);
  long target = 1;
  struct PointerTriple pointer_value = {0};
  pointer_value.target = &target;
  long pointer_result = mutate_pointer_triple(pointer_value);
  if (mutated != 17 || value.a != 7 || pointer_result != 2 ||
      target != 31 || pointer_value.target != &target)
    return 0;
  return 24;
}

#ifdef M7_ABI_NATIVE_MAIN
int main(void) { return abi_entry() == 24 ? 0 : 1; }
#endif
