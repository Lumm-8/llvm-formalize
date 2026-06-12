#include "../top.h"
void top () {
    int x1,x2,x3,x4,x5,x6, r;
    registerInput("x1",&x1,sizeof(x1)); registerInput("x2",&x2,sizeof(x2));
    registerInput("x3",&x3,sizeof(x3)); registerInput("x4",&x4,sizeof(x4));
    registerInput("x5",&x5,sizeof(x5)); registerInput("x6",&x6,sizeof(x6));
    r = 0;
    if (x1 > 0) r = r + x1; else r = r - x1;
    if (x2 > 5) r = r * 2; else r = r + x2;
    if (x3 % 2 == 0) r = r + x3; else r = r - x3;
    if (x4 > x1) r = r + 10; else r = r - 5;
    if (x5 > x6) { r = r + x5; } else { r = r - x6; }
    r = r + (x1 ^ x2) + (x3 & x4);
    if (r > 5000) r = 5000; if (r < -5000) r = -5000;
    registerOutput("r",&r,sizeof(r));
}
