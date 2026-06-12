// Test 45: for loop with direct accumulation (simplified)
#include "../top.h"
void top () {
    int sum, a, b;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    sum = 0;
    // Bounded loop - unrolled by loop-unroll
    if (1) { sum = sum + a; }
    if (1) { sum = sum + b; }
    if (1) { sum = sum * 2; }
    if (1) { sum = sum - (a + b); }
    // Simulates: for(i=0;i<4;i++) { sum = ... }
    if (sum > 10000) sum = 10000;
    if (sum < -10000) sum = -10000;
    registerOutput("sum", &sum, sizeof(sum));
}
