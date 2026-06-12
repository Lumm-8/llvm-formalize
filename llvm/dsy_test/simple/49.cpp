// Test 49: struct + for + pointer combined (100+ lines)
#include "../top.h"

struct Box {
    int value;
    int *ptr;
    int count;
};

void top () {
    int a, b, r, i;
    Box bx;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    bx.value = a;
    bx.ptr = &b;
    bx.count = 0;

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            bx.value = bx.value + *(bx.ptr);
            bx.count = bx.count + 1;
        } else if (i == 1) {
            if (bx.value > 100) bx.value = bx.value / 2;
            bx.count = bx.count + 1;
        } else {
            bx.value = bx.value - a;
            bx.count = bx.count + 1;
        }
    }

    r = bx.value + bx.count;

    if (bx.ptr != 0) {
        r = r + *(bx.ptr);
    }

    if (r > 5000) r = 5000;
    if (r < -5000) r = -5000;

    registerOutput("r", &r, sizeof(r));
}
