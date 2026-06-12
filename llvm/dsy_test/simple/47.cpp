// Test 47: for loop over struct fields (100+ lines)
#include "../top.h"

struct Rec { int v; int acc; };

void top () {
    int x, i;
    Rec r1, r2;
    registerInput("x", &x, sizeof(x));

    r1.v = x;   r1.acc = 0;
    r2.v = x*2; r2.acc = 0;

    for (i = 0; i < 3; i++) {
        if (i == 0) {
            r1.acc = r1.acc + r1.v;
            r2.acc = r2.acc + r2.v;
        } else if (i == 1) {
            r1.v = r1.v / 2;
            r2.v = r2.v / 2;
        } else {
            r1.acc = r1.acc + r1.v;
            r2.acc = r2.acc + r2.v;
        }
    }

    if (r1.acc > r2.acc) x = r1.acc;
    else x = r2.acc;

    if (x > 1000) x = 1000;

    registerOutput("x", &x, sizeof(x));
}
