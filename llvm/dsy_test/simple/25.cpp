// Test 25: multiple independent conditionals on same inputs
#include "../top.h"
void top () {
    int a, b, x, y;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    x = a > 0 ? 1 : 0;
    y = b > 0 ? 1 : 0;

    registerOutput("x", &x, sizeof(x));
    registerOutput("y", &y, sizeof(y));
}
