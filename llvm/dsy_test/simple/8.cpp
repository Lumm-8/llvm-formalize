#include "../top.h"

void top () {
    int a, b, c, count;
    int cnt = 0;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    registerInput("cnt", &count, sizeof(count));

    #pragma clang loop unroll_count(2)
    while (a) {
        a >>= 1;
        cnt++;
    }

    c = 32 - cnt;
    registerOutput("c", &c, sizeof(c));
}
