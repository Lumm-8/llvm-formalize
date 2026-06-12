#include "../top.h"
void top () {
    int v1,v2,v3,v4, tmp;
    int *p1, *p2;
    registerInput("v1",&v1,sizeof(v1)); registerInput("v2",&v2,sizeof(v2));
    registerInput("v3",&v3,sizeof(v3)); registerInput("v4",&v4,sizeof(v4));
    p1 = &v1; p2 = &v2;
    if (*p1 < *p2) { tmp = *p1; *p1 = *p2; *p2 = tmp; }
    p1 = &v3; p2 = &v4;
    if (*p1 < *p2) { tmp = *p1; *p1 = *p2; *p2 = tmp; }
    if (v1 > v3) tmp = v1 + v2; else tmp = v3 + v4;
    if (tmp > 500) tmp = 500; if (tmp < -500) tmp = -500;
    registerOutput("tmp", &tmp, sizeof(tmp));
}
