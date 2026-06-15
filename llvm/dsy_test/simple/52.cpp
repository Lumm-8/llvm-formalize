// Test 52: Array-like processing with struct + pointer arithmetic simulation
// Simulates: std::vector-like push_back, std::sort comparison, std::accumulate
// All functions inlined into top()
#include "../top.h"

struct Element {
    int value;
    int index;
    int *next;
};

// --- inlined push_back(Element *arr, int idx, int val) ---
// --- inlined compare_and_swap(Element *a, Element *b) ---
// --- inlined accumulate(Element *arr, int base) ---
// --- inlined find_median(Element *e1, Element *e2, Element *e3) ---

void top () {
    int base, v1, v2, v3;
    Element e1, e2, e3;
    int result;

    registerInput("base", &base, sizeof(base));
    registerInput("v1",   &v1,   sizeof(v1));
    registerInput("v2",   &v2,   sizeof(v2));
    registerInput("v3",   &v3,   sizeof(v3));

    // --- setup elements ---
    e1.index = 0; e1.value = base + v1; e1.next = &v2;
    e2.index = 1; e2.value = base + v2; e2.next = &v3;
    e3.index = 2; e3.value = base + v3; e3.next = &v1;

    // --- inlined push_back for e1 (adjust value by index) ---
    {
        Element *el = &e1;
        el->value = el->value + el->index * 10;
        if (el->value > 2000) el->value = 2000;
        if (el->next != 0) {
            *(el->next) = *(el->next) + el->index;
        }
    }

    // --- inlined push_back for e2 ---
    {
        Element *el = &e2;
        el->value = el->value + el->index * 10;
        if (el->value < -1000) el->value = -1000;
        if (el->next != 0) {
            *(el->next) = *(el->next) + el->index;
        }
    }

    // --- inlined push_back for e3 ---
    {
        Element *el = &e3;
        el->value = el->value + el->index * 10;
        if (el->value > 3000) el->value = 3000;
        if (el->next != 0) {
            *(el->next) = *(el->next) + el->index;
        }
    }

    // --- inlined compare_and_swap(e1, e2) ---
    {
        Element *a = &e1;
        Element *b = &e2;
        if (a->value > b->value) {
            // swap values
            int tmp_val = a->value;
            a->value = b->value;
            b->value = tmp_val;
            // swap indices
            int tmp_idx = a->index;
            a->index = b->index;
            b->index = tmp_idx;
        }
    }

    // --- inlined compare_and_swap(e2, e3) ---
    {
        Element *a = &e2;
        Element *b = &e3;
        if (a->value > b->value) {
            int tmp_val = a->value;
            a->value = b->value;
            b->value = tmp_val;
            int tmp_idx = a->index;
            a->index = b->index;
            b->index = tmp_idx;
        }
    }

    // --- inlined compare_and_swap(e1, e2) again (bubble sort pass 2) ---
    {
        Element *a = &e1;
        Element *b = &e2;
        if (a->value > b->value) {
            int tmp_val = a->value;
            a->value = b->value;
            b->value = tmp_val;
            int tmp_idx = a->index;
            a->index = b->index;
            b->index = tmp_idx;
        }
    }

    // --- inlined find_median(e1, e2, e3) ---
    {
        // After two bubble passes, e2 should be the median
        result = e2.value;
    }

    // --- inlined accumulate(base adjustment) ---
    {
        int sum = e1.value + e2.value + e3.value;
        if (sum > 0) {
            result = result + sum / 10;
        } else {
            result = result - sum / 20;
        }
    }

    if (result > 10000) result = 10000;
    if (result < -10000) result = -10000;

    registerOutput("result", &result, sizeof(result));
}
