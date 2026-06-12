// Test 9: bitwise AND / OR / XOR
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > 0)
        c = a & b;
    else
        c = a | b;

    registerOutput("c", &c, sizeof(c));
}
