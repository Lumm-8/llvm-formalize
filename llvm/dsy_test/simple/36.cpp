#include "../top.h"
void top () {
    int idx, v0,v1,v2,v3, r;
    registerInput("idx",&idx,sizeof(idx)); registerInput("v0",&v0,sizeof(v0));
    registerInput("v1",&v1,sizeof(v1)); registerInput("v2",&v2,sizeof(v2));
    registerInput("v3",&v3,sizeof(v3));
    if (idx <= 0) r = v0;
    else if (idx == 1) r = v1;
    else if (idx == 2) r = v2;
    else if (idx == 3) r = v3;
    else if (idx == 4) r = v0 + v1;
    else if (idx == 5) r = v1 + v2;
    else if (idx == 6) r = v2 + v3;
    else if (idx == 7) r = v0 * v1;
    else if (idx == 8) r = v1 * v2;
    else r = v0 + v1 + v2 + v3;
    if (r > 1000) r = 1000; if (r < -1000) r = -1000;
    registerOutput("r",&r,sizeof(r));
}
