#include "../top.h"
void top () {
    int p,q,r,s, t1,t2, o;
    registerInput("p",&p,sizeof(p)); registerInput("q",&q,sizeof(q));
    registerInput("r",&r,sizeof(r)); registerInput("s",&s,sizeof(s));
    t1 = p + q; t2 = p - q;
    if (t1 > t2) o = t1 * r; else o = t2 * s;
    if (p > 0) o = o + s; else o = o - r;
    if (o > 1000) o = 1000; if (o < -1000) o = -1000;
    registerOutput("o",&o,sizeof(o));
}
