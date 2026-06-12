#include "../top.h"
struct Point { int x; int y; };
void top () {
    int n, val;
    Point p;
    registerInput("n", &n, sizeof(n)); registerInput("val", &val, sizeof(val));
    p.x = 0; p.y = 0;
    { p.x = val + 1; p.y = val - 1; }
    { p.x = p.x * 2; p.y = p.y * 2; }
    { p.x = p.x + n; p.y = p.y - n; }
    if (p.x > 500) p.x = 500; if (p.x < -500) p.x = -500;
    registerOutput("p.x", &p.x, sizeof(p.x)); registerOutput("p.y", &p.y, sizeof(p.y));
}
