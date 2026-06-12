// Test 22: XOR conditional pattern
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > 0) {
        if (b > 0)
            c = a ^ b;
        else
            c = a + b;
    } else {
        if (b > 0)
            c = a - b;
        else
            c = a & b;
    }

    registerOutput("c", &c, sizeof(c));
}
