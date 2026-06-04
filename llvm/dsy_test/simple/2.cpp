
#include "../top.h"

void top () {
    int a, b, c;
    bool flag;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));

   if (a > 10) {
       c = 10; 
   }
   else if (a < 2) {
        c = 0;
   }
   else {
        c = 5;
   }

    registerOutput("c", &c, sizeof(c));
}
