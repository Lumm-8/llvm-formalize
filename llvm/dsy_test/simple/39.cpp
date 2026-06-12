#include "../top.h"
void top () {
    int a,b,c,d, hi1,hi2,lo1,lo2, r;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c)); registerInput("d",&d,sizeof(d));
    // Multi-path stores for hi/lo pairs
    if (a > b) { hi1 = a; lo1 = b; } else { hi1 = b; lo1 = a; }
    if (c > d) { hi2 = c; lo2 = d; } else { hi2 = d; lo2 = c; }
    if (hi1 > hi2) r = hi1 * 100 + hi2 * 10 + lo1 + lo2;
    else r = hi2 * 100 + hi1 * 10 + lo1 + lo2;
    registerOutput("r",&r,sizeof(r));
}
