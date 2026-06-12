#include "../top.h"
void top () {
    int a,b,c;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c));
    if (a > 10) c = a + 5; else if (b > 10) c = b - 3; else c = a + b;
    registerOutput("c", &c, sizeof(c));
}
