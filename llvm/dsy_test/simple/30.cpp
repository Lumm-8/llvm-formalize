#include "../top.h"
void top () {
    int a,b,c,d, r;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c)); registerInput("d",&d,sizeof(d));
    if (a > b) { if (c > d) r = a + c; else r = a + d; }
    else { if (c > d) r = b + c; else r = b + d; }
    if (r > 200) r = 200; if (r < -200) r = -200;
    registerOutput("r",&r,sizeof(r));
}
