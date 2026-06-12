#include "../top.h"
struct Data { int x; int y; int z; int w; };
void top () {
    int a, b, c;
    Data d;
    registerInput("a", &a, sizeof(a)); registerInput("b", &b, sizeof(b));
    registerInput("c", &c, sizeof(c));
    d.x = a + 1; d.y = b + 2; d.z = c + 3; d.w = a + b + c;
    if (d.x > d.y) { if (d.z > d.w) { if (a > 0) c = d.x*2; else c = d.y*2; }
                     else { if (b > 0) c = d.z*3; else c = d.w*3; } }
    else { if (d.z > d.w) { if (a > 0) c = d.x+d.y; else c = d.z+d.w; }
           else { if (b > 0) c = d.x-d.y; else c = d.z-d.w; } }
    if (c > 1000) c = 1000; if (c < -1000) c = -1000;
    registerOutput("c", &c, sizeof(c));
}
