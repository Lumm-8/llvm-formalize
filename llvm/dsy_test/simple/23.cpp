// Test 23: clamp value to range [lo, hi]
#include "../top.h"
void top () {
    int val, lo, hi, result;
    registerInput("val", &val, sizeof(val));
    registerInput("lo", &lo, sizeof(lo));
    registerInput("hi", &hi, sizeof(hi));

    if (val < lo)
        result = lo;
    else if (val > hi)
        result = hi;
    else
        result = val;

    registerOutput("result", &result, sizeof(result));
}
