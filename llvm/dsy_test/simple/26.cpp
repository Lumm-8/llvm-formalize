// Test 26: complex arithmetic with shift
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    c = (a + b) * 2;
    if (c > 100)
        c = c >> 1;
    else
        c = c << 1;

    registerOutput("c", &c, sizeof(c));
}
