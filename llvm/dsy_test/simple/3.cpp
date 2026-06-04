
#include "../top.h"

int x[2] = {10, 11};
void top () {
    int a, b, c;

    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

    if (a > 10) {
        c = x[0]; 
    }
    else {
         c = x[1];
    }

    registerOutput("c", &c, sizeof(c));
}
