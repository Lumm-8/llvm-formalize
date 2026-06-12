// Test 16: multiply or add depending on sign
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > 0)
        c = a * 2 + b;
    else
        c = a + b * 2;

    registerOutput("c", &c, sizeof(c));
}
