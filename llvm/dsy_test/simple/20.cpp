// Test 20: mixed bitwise + arithmetic in conditional
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if ((a & 1) == 0)
        c = a * 2 + b;
    else
        c = a | b;

    registerOutput("c", &c, sizeof(c));
}
