// Test 11: multi-output
#include "../top.h"
void top () {
    int a, b, x, y;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    x = a + b;
    y = a - b;

    if (x > 10)
        x = x + 1;
    if (y < 0)
        y = -y;

    registerOutput("x", &x, sizeof(x));
    registerOutput("y", &y, sizeof(y));
}
