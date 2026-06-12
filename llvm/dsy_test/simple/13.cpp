// Test 13: arithmetic chain
#include "../top.h"
void top () {
    int a, b, c, result;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    registerInput("c", &c, sizeof(c));

    result = a * b;
    result = result + c;
    result = result - a;

    registerOutput("result", &result, sizeof(result));
}
