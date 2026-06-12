// JIT runner for TranslateToStp regression tests.
//
// Provides stub implementations of registerInput/registerOutput that
// read inputs from environment variables and print outputs in a
// machine-parseable format.
//
// Usage:
//   a=4 b=0 cnt=0 lli linked.ll
//   → prints:  c=30
//
// Each test has a .jit_expect file that lists input→output pairs.
#include <cstdio>
#include <cstdlib>

extern "C" {

// ---- stubs for the test harness API ----

void _Z13registerInputPKcPvi(const char *name, void *ptr, int size) {
    const char *val = getenv(name);
    int ival = val ? atoi(val) : 0;
    if (size == 4)
        *(int *)ptr = ival;
    else if (size == 1)
        *(char *)ptr = (char)ival;
    // struct fields: environment variable name matches registerInput name
    // e.g. "x" for p.x, "y" for p.y
}

void _Z14registerOutputPKcPvi(const char *name, void *ptr, int size) {
    // Print one line per output:  name=value
    if (size == 4)
        printf("%s=%d\n", name, *(int *)ptr);
    else if (size == 1)
        printf("%s=%d\n", name, (int)*(char *)ptr);
}

} // extern "C"

// The test function – defined in the linked test IR.
extern "C" void _Z3topv();

int main() {
    _Z3topv();
    return 0;
}
