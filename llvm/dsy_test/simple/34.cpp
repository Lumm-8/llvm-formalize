#include "../top.h"
void top () {
    int a,b,c,d, x,y, o1,o2;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c)); registerInput("d",&d,sizeof(d));
    // x = abs(a-b), y = abs(c-d) (multi-path stores)
    if (a > b) x = a - b; else x = b - a;
    if (c > d) y = c - d; else y = d - c;
    if (x > y) { o1 = x * 2; o2 = y + 10; }
    else { o1 = y * 2; o2 = x + 10; }
    registerOutput("o1",&o1,sizeof(o1)); registerOutput("o2",&o2,sizeof(o2));
}
