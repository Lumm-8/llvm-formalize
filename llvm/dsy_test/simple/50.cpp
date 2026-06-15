// Test 50: Complex data pipeline with function-like blocks (all inlined)
// Simulates: read → transform → filter → aggregate pipeline
// Original functions inlined: transform(), filter(), aggregate(), clamp()
#include "../top.h"

struct Record {
    int id;
    int value;
    int flag;
};

// --- inlined transform(Record *r, int factor) ---
// --- inlined filter(Record *r, int threshold) ---
// --- inlined aggregate(Record *a, Record *b, Record *out) ---
// --- inlined clamp(int val, int lo, int hi) ---

void top () {
    int factor, threshold, lo, hi;
    int result;
    Record r1, r2, r3;

    registerInput("factor",  &factor,  sizeof(factor));
    registerInput("threshold",&threshold, sizeof(threshold));
    registerInput("lo",  &lo,  sizeof(lo));
    registerInput("hi",  &hi,  sizeof(hi));

    // --- setup records ---
    r1.id = 1; r1.value = factor * 3;      r1.flag = 0;
    r2.id = 2; r2.value = threshold * 2;   r2.flag = 0;
    r3.id = 3; r3.value = factor + threshold; r3.flag = 0;

    // --- inlined transform(r1, factor) ---
    {
        Record *r = &r1;
        int f = factor;
        r->value = r->value * f;
        if (r->value > 1000) r->value = 1000;
        r->flag = (r->value > 0) ? 1 : -1;
    }

    // --- inlined transform(r2, threshold) ---
    {
        Record *r = &r2;
        int f = threshold;
        r->value = r->value + f * 10;
        if (r->value < -500) r->value = -500;
        r->flag = (r->value > 0) ? 1 : -1;
    }

    // --- inlined filter(r1, threshold) ---
    {
        Record *r = &r1;
        int t = threshold;
        if (r->value < t) {
            r->value = 0;
            r->flag = 0;
        }
    }

    // --- inlined filter(r2, threshold) ---
    {
        Record *r = &r2;
        int t = threshold;
        if (r->value < t) {
            r->value = 0;
            r->flag = 0;
        }
    }

    // --- inlined aggregate(r1, r2, r3) ---
    {
        r3.value = r1.value + r2.value;
        r3.flag  = r1.flag  + r2.flag;
    }

    // --- inlined clamp(r3.value, lo, hi) ---
    {
        int val = r3.value;
        int lo_val = lo;
        int hi_val = hi;
        if (val < lo_val) val = lo_val;
        if (val > hi_val) val = hi_val;
        if (lo_val > hi_val) {
            int tmp = lo_val; lo_val = hi_val; hi_val = tmp;
        }
        result = val;
    }

    // Post-processing: adjust by flag
    if (r3.flag > 0) {
        result = result + r3.flag * 10;
    } else if (r3.flag < 0) {
        result = result - r3.flag * 5;
    }

    if (result > 5000) result = 5000;
    if (result < -5000) result = -5000;

    registerOutput("result", &result, sizeof(result));
}
