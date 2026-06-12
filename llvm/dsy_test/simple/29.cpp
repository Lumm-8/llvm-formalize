// Test 29: mask and merge bits
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    // Take low 4 bits of a, high 4 bits of b (metaphorically using & and |)
    int lo = a & 15;
    int hi = b & ~15;
    c = lo | hi;

    registerOutput("c", &c, sizeof(c));
}
