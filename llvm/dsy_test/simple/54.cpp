// Test 54: Multi-struct data processing pipeline (all functions inlined)
// Simulates: validate(), transform(), merge(), normalize(), finalize()
// Each "function" is expanded inline with clear block comments
#include "../top.h"

struct InputRec {
    int raw;
    int scaled;
    int *peer;
};

struct OutputRec {
    int sum;
    int product;
    int flag;
};

// --- inlined validate(InputRec *r, int min_val, int max_val) ---
// --- inlined transform(InputRec *r, int factor) ---
// --- inlined merge(InputRec *a, InputRec *b, OutputRec *out) ---
// --- inlined normalize(OutputRec *out, int norm) ---
// --- inlined finalize(OutputRec *out, int bias) ---

void top () {
    int min_val, max_val, factor, norm, bias;
    InputRec  ir1, ir2;
    OutputRec orec;
    int result;

    registerInput("min_val", &min_val, sizeof(min_val));
    registerInput("max_val", &max_val, sizeof(max_val));
    registerInput("factor",  &factor,  sizeof(factor));
    registerInput("norm",    &norm,    sizeof(norm));
    registerInput("bias",    &bias,    sizeof(bias));

    // --- initialize ---
    ir1.raw = factor * min_val;
    ir1.scaled = 0;
    ir1.peer = &(ir2.raw);

    ir2.raw = norm * max_val;
    ir2.scaled = 0;
    ir2.peer = &(ir1.raw);

    orec.sum = 0;
    orec.product = 1;
    orec.flag = 0;

    // --- inlined validate(ir1, min_val, max_val) ---
    {
        InputRec *r = &ir1;
        int lo = min_val;
        int hi = max_val;
        if (r->raw < lo) {
            r->raw = lo;
            r->scaled = -1;
        } else if (r->raw > hi) {
            r->raw = hi;
            r->scaled = 1;
        } else {
            r->scaled = 0;
        }
        // Also check peer
        if (r->peer != 0 && *(r->peer) < lo) {
            *(r->peer) = lo;
        }
    }

    // --- inlined validate(ir2, min_val, max_val) ---
    {
        InputRec *r = &ir2;
        int lo = min_val;
        int hi = max_val;
        if (r->raw < lo) {
            r->raw = lo;
            r->scaled = -1;
        } else if (r->raw > hi) {
            r->raw = hi;
            r->scaled = 1;
        } else {
            r->scaled = 0;
        }
    }

    // --- inlined transform(ir1, factor) ---
    {
        InputRec *r = &ir1;
        int f = factor;
        r->scaled = r->raw * f;
        if (r->scaled > 1000) r->scaled = 1000;
        // Update peer's knowledge of this value
        if (r->peer != 0) {
            // peer points to ir2.raw; use scaled as new raw for next stage
            *(r->peer) = *(r->peer) + r->scaled / 10;
        }
    }

    // --- inlined transform(ir2, factor) ---
    {
        InputRec *r = &ir2;
        int f = factor;
        r->scaled = r->raw * f;
        if (r->scaled < -1000) r->scaled = -1000;
        if (r->peer != 0) {
            *(r->peer) = *(r->peer) + r->scaled / 10;
        }
    }

    // --- inlined merge(ir1, ir2, orec) ---
    {
        orec.sum     = ir1.scaled + ir2.scaled;
        orec.product = ir1.scaled * ir2.scaled;
        if (ir1.scaled > ir2.scaled)
            orec.flag = 1;
        else if (ir1.scaled < ir2.scaled)
            orec.flag = -1;
        else
            orec.flag = 0;
    }

    // --- inlined normalize(orec, norm) ---
    {
        OutputRec *o = &orec;
        int n = norm;
        if (n != 0) {
            if (o->sum > 100)     o->sum     = o->sum     / n;
            if (o->product > 100) o->product = o->product / n;
        }
        if (o->sum < 0)     o->sum     = -o->sum;
        if (o->product < 0) o->product = -o->product;
    }

    // --- inlined finalize(orec, bias) ---
    {
        OutputRec *o = &orec;
        int b = bias;
        if (o->flag > 0) {
            result = o->sum + b;
        } else if (o->flag < 0) {
            result = o->product + b;
        } else {
            result = o->sum + o->product + b;
        }
    }

    // --- cross-check ir1.scaled vs ir2.scaled ---
    {
        int *p1 = &(ir1.scaled);
        int *p2 = &(ir2.scaled);
        if (*p1 > *p2) {
            result = result + (*p1 - *p2);
        } else {
            result = result + (*p2 - *p1);
        }
    }

    if (result > 10000) result = 10000;
    if (result < -10000) result = -10000;

    registerOutput("result", &result, sizeof(result));
}
