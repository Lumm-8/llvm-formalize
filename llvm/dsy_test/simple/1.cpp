#include "../top.h"

void top () {
    int a, b, c;
    bool flag;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

   if (a > 0) {
        if (b > 0) {
            c = 1;
        }
        else {
            c = 2;
        }
   }
   else {
        if (b > 0) {
            c = 3;
        }
        else {
            c = 4;
        }
   }

    registerOutput("c", &c, sizeof(c));
}
