// Test 28: absolute difference and sign detection
#include "../top.h"
void top () {
    int a, b, diff, sign;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > b) {
        diff = a - b;
        sign = 1;
    } else if (a < b) {
        diff = b - a;
        sign = -1;
    } else {
        diff = 0;
        sign = 0;
    }

    registerOutput("diff", &diff, sizeof(diff));
    registerOutput("sign", &sign, sizeof(sign));
}
