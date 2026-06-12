// Test 18: ternary with three inputs
#include "../top.h"
void top () {
    int a, b, c, result;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    registerInput("c", &c, sizeof(c));

    if (a > b) {
        if (a > c)
            result = a;
        else
            result = c;
    } else {
        if (b > c)
            result = b;
        else
            result = c;
    }

    registerOutput("result", &result, sizeof(result));
}
