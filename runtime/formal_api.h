#pragma once
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

__attribute__((noinline, used)) void
__formal_register_input(void *addr, size_t size, const char *name);
__attribute__((noinline, used)) void
__formal_register_output(void *addr, size_t size, const char *name);
__attribute__((noinline, used)) void __formal_assume(int condition);

#define FORMAL_INPUT(x) __formal_register_input(&(x), sizeof(x), #x)
#define FORMAL_OUTPUT(x) __formal_register_output(&(x), sizeof(x), #x)
#define FORMAL_INPUT_BUF(ptr, size, name)                                      \
  __formal_register_input((ptr), (size), (name))
#define FORMAL_OUTPUT_BUF(ptr, size, name)                                     \
  __formal_register_output((ptr), (size), (name))

#ifdef __cplusplus
}
#endif
