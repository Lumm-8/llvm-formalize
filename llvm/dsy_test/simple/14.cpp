// Test 14: boolean logic with int output
#include "../top.h"
void top () {
    int a, b, c;
    bool flag;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > 0 && b > 0)
        c = 100;
    else if (a > 0 || b > 0)
        c = 50;
    else
        c = 0;

    registerOutput("c", &c, sizeof(c));
}
