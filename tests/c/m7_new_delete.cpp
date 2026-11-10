// clang-20 -std=c++17 -O0 -fno-exceptions -S -emit-llvm m7_new_delete.cpp
__attribute__((noinline)) int new_delete_entry() {
  int *value = new int(3);
  int result = *value;
  delete value;
  return result;
}
