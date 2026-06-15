// Test 51: Binary tree node swap + balance simulation (all functions inlined)
// Simulates: std::swap, std::max, std::min, tree rotation helpers
// Original functions: swapNodes(), findMax(), findMin(), rotateLeft(), balance()
#include "../top.h"

struct TreeNode {
    int key;
    int *left_val;
    int *right_val;
    int balance_factor;
};

// --- inlined swapValues(int *a, int *b) ---
// --- inlined findMax(int a, int b, int c) ---
// --- inlined findMin(int a, int b, int c) ---
// --- inlined clampBalance(int bf) ---

void top () {
    int v1, v2, v3, v4;
    TreeNode node;
    int result;

    registerInput("v1", &v1, sizeof(v1));
    registerInput("v2", &v2, sizeof(v2));
    registerInput("v3", &v3, sizeof(v3));
    registerInput("v4", &v4, sizeof(v4));

    // --- setup node ---
    node.key = v1;
    node.left_val  = &v2;
    node.right_val = &v3;
    node.balance_factor = 0;

    // --- inlined swapValues(left_val, right_val) if left > right ---
    {
        int *a = node.left_val;
        int *b = node.right_val;
        if (*a > *b) {
            int tmp = *a;
            *a = *b;
            *b = tmp;
        }
    }

    // --- inlined findMax(node.key, *node.left_val, *node.right_val) ---
    {
        int a = node.key;
        int b = *node.left_val;
        int c = *node.right_val;
        int max_val;
        if (a > b) {
            if (a > c) max_val = a;
            else       max_val = c;
        } else {
            if (b > c) max_val = b;
            else       max_val = c;
        }
        node.key = max_val;
    }

    // --- inlined findMin from the three values, store in v4 ---
    {
        int a = node.key;
        int b = *node.left_val;
        int c = *node.right_val;
        int min_val;
        if (a < b) {
            if (a < c) min_val = a;
            else       min_val = c;
        } else {
            if (b < c) min_val = b;
            else       min_val = c;
        }
        v4 = min_val;
    }

    // --- inlined rotateLeft simulation ---
    {
        // In a left rotation, the right child becomes the new root.
        // Simulate by swapping key and right_val's value, then adjusting.
        int *r = node.right_val;
        int old_key = node.key;
        node.key = *r;
        *r = old_key;
    }

    // --- inlined clampBalance ---
    {
        int bf = (node.key - *node.left_val) - (*node.right_val - v4);
        if (bf > 1)  bf = 1;
        if (bf < -1) bf = -1;
        node.balance_factor = bf;
    }

    // --- compute result from balance ---
    {
        int *l = node.left_val;
        int *r = node.right_val;
        if (node.balance_factor > 0) {
            result = node.key + *l;
        } else if (node.balance_factor < 0) {
            result = node.key + *r;
        } else {
            result = node.key + (*l + *r) / 2;
        }
    }

    if (result > 5000)  result = 5000;
    if (result < -5000) result = -5000;

    registerOutput("result", &result, sizeof(result));
}
