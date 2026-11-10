#include "formal_api.h"

extern "C" void __formal_register_input(void *, size_t, const char *) {}
extern "C" void __formal_register_output(void *, size_t, const char *) {}
extern "C" void __formal_assume(int) {}
