#include "../top.h"
void top () {
    int a,b,op, r;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("op",&op,sizeof(op));
    if (op == 0) r = a + b;
    else if (op == 1) r = a - b;
    else if (op == 2) r = a * b;
    else if (op == 3) { if (b != 0) r = a / b; else r = 0; }
    else if (op == 4) r = a & b;
    else if (op == 5) r = a | b;
    else if (op == 6) r = a ^ b;
    else if (op == 7) r = a << (b & 7);
    else if (op == 8) r = a >> (b & 7);
    else r = (a + b) * (a - b);
    if (r > 2000) r = 2000; if (r < -2000) r = -2000;
    registerOutput("r",&r,sizeof(r));
}
