// Test 19: signed division and remainder
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (b != 0) {
        if (a >= 0)
            c = a / b;
        else
            c = a % b;
    } else {
        c = 0;
    }

    registerOutput("c", &c, sizeof(c));
}
