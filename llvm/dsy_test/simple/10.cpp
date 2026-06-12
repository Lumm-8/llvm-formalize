// Test 10: unsigned comparison
#include "../top.h"
void top () {
    unsigned int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > 100u)
        c = a + b;
    else
        c = a * b;

    registerOutput("c", &c, sizeof(c));
}
