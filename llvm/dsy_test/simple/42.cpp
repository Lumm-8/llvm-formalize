#include "../top.h"
void top () {
    int a, b, c, d, result;
    int *p, *q;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c)); registerInput("d",&d,sizeof(d));
    p = &a; q = &b;
    c = *p + *q;
    p = &c; q = &d;
    result = *p * (*q);
    if (result > 0) { if (result > 500) result = 500; }
    else { result = result + a; }
    if (a > b) result = result + (a - b);
    else result = result + (b - a);
    registerOutput("result", &result, sizeof(result));
}
