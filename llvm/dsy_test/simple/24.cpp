// Test 24: boolean expressions with NOT
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (!(a > 10) && !(b < 0))
        c = 1;
    else if (a > 10 && b < 0)
        c = -1;
    else
        c = 0;

    registerOutput("c", &c, sizeof(c));
}
