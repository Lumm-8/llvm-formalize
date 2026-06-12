#include "../top.h"
void top () {
    int a,b,c,d, f1,f2,f3,f4, r;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c)); registerInput("d",&d,sizeof(d));
    f1 = (a > 0 && b > 0) ? 1 : 0;
    f2 = (c > 0 && d > 0) ? 2 : 0;
    f3 = (a > 0 || c > 0) ? 4 : 0;
    f4 = (b < 0 || d < 0) ? 8 : 0;
    r = f1 + f2 + f3 + f4;
    if (r & 1) r = r + a;
    if (r & 2) r = r - b;
    if (r & 4) r = r * c;
    if (r & 8) { if (d != 0) r = r / d; }
    registerOutput("r",&r,sizeof(r));
}
