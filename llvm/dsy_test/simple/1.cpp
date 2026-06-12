#include "../top.h"

void top () {
    int a, b, c;
    bool flag;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

   if (a > 0) {
        if (b > 0) {
            c = 10;
        }
        else {
            c = 20;
        }
   }
   else {
        if (b > 0) {
            c = 30;
        }
        else {
            c = 40;
        }
   }

    registerOutput("c", &c, sizeof(c));
}
