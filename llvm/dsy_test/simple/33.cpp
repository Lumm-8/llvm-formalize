#include "../top.h"
void top () {
    int s0,s1,s2,st,o;
    registerInput("s0",&s0,sizeof(s0)); registerInput("s1",&s1,sizeof(s1));
    registerInput("s2",&s2,sizeof(s2)); registerInput("st",&st,sizeof(st));
    if (st == 0) { o = s0 + s1; if (o > 100) o = o / 2; }
    else if (st == 1) { o = s1 - s2; if (o < 0) o = -o; }
    else if (st == 2) { o = s0 * s2; if (o > 500) o = 500; }
    else if (st == 3) { if (s2 != 0) o = s0 / s2; else o = 0; }
    else if (st == 4) { o = (s0 & s1) | s2; }
    else if (st == 5) { o = (s0 ^ s1) & s2; }
    else if (st == 6) { o = (s0 << (s1 & 7)) + s2; }
    else if (st == 7) { o = (s0 >> (s1 & 7)) - s2; }
    else if (st == 8) { o = s0 + s1 + s2; }
    else { o = s0 * s1 * s2; }
    if (st % 2 == 0 && o > 0) o = o * 2;
    if (st % 3 == 0 && o < 0) o = o + 100;
    registerOutput("o",&o,sizeof(o));
}
