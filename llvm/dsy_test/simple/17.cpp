// Test 17: max/min pattern
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > b)
        c = a;
    else
        c = b;

    registerOutput("c", &c, sizeof(c));
}
