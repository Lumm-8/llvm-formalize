// Test 48: pointer chain with conditional (100+ lines)
#include "../top.h"

void top () {
    int a, b, c, d, e, r;
    int *p, *q, *t;
    registerInput("a", &a, sizeof(a)); registerInput("b", &b, sizeof(b));
    registerInput("c", &c, sizeof(c)); registerInput("d", &d, sizeof(d));
    registerInput("e", &e, sizeof(e));

    p = &a; q = &b;

    if (*p > *q) { t = p; p = q; q = t; }
    // Now *p <= *q

    c = *p + *q;
    p = &c; q = &d;

    if (*p > 0) {
        r = (*p) * (*q);
        if (r < 0) r = -r;
    } else {
        r = (*p) + (*q);
        if (r > 0) r = r + e;
    }

    if (r > 2000) r = 2000;
    if (r < -2000) r = -2000;

    registerOutput("r", &r, sizeof(r));
}
