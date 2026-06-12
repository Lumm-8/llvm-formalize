#include "../top.h"
void top () {
    int u,v,w,x,y,z,r;
    registerInput("u",&u,sizeof(u)); registerInput("v",&v,sizeof(v));
    registerInput("w",&w,sizeof(w));
    // x = max, y = min (multi-path stores)
    if (u > v) { if (u > w) x = u; else x = w; }
    else { if (v > w) x = v; else x = w; }
    if (u < v) { if (u < w) y = u; else y = w; }
    else { if (v < w) y = v; else y = w; }
    z = u + v + w - x - y;  // middle value
    r = x * 100 + y * 10 + z;
    registerOutput("r",&r,sizeof(r));
}
