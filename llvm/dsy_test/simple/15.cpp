// Test 15: negation and absolute value
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    c = a - b;
    if (c < 0)
        c = -c;

    registerOutput("c", &c, sizeof(c));
}
