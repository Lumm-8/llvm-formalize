#include "../top.h"

void top () {
    int a, b, c;
    bool flag;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    // c = a + b;

    flag = (a > 0) && (b > 0);

    if (flag) {
        c = a + b;
    }
    else {
        c = a - b;
    }

    registerOutput("c", &c, sizeof(c));
}
