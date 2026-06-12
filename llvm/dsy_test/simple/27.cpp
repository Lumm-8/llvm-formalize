// Test 27: conditional with constants on both branches
#include "../top.h"
void top () {
    int a, b, c;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > b)
        c = a * b + a;
    else
        c = a * b + b;

    registerOutput("c", &c, sizeof(c));
}
