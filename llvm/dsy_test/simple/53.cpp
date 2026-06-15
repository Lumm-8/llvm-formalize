// Test 53: Linked-list node manipulation with pointer chains (all inlined)
// Simulates: insert_after(), delete_node(), find_max(), reverse_sublist()
// All functions expanded inline into top()
#include "../top.h"

struct ListNode {
    int data;
    int *next_data;   // simulated "next" pointer via int*
    int tag;
};

// --- inlined insert_after(ListNode *node, int new_val) ---
// --- inlined delete_node(ListNode *prev, ListNode *target) ---
// --- inlined find_max_in_chain(ListNode *head) ---
// --- inlined swap(std::swap via pointers) ---

void top () {
    int a, b, c, d;
    ListNode n1, n2, n3, n4;
    int result, max_val;

    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    registerInput("c", &c, sizeof(c));
    registerInput("d", &d, sizeof(d));

    // --- build linked list: n1 -> n2 -> n3 -> n4 ---
    n1.data = a;     n1.next_data = &(n2.data); n1.tag = 1;
    n2.data = b;     n2.next_data = &(n3.data); n2.tag = 2;
    n3.data = c;     n3.next_data = &(n4.data); n3.tag = 3;
    n4.data = d;     n4.next_data = 0;          n4.tag = 4;

    // --- inlined insert_after(n2, new_val = a + b) ---
    // Simulated by modifying n3's data and re-linking
    {
        ListNode *node = &n2;
        int new_val = a + b;
        // "insert" by pushing values down the chain and inserting at n3
        n4.data = n3.data;
        n4.tag  = n3.tag;
        n3.data = new_val;
        n3.tag  = 5;  // inserted node tag
    }

    // --- inlined find_max_in_chain starting from n1 ---
    {
        ListNode *cur = &n1;
        max_val = cur->data;
        // traverse chain
        {
            int *next_ptr = cur->next_data;
            if (next_ptr != 0) {
                int next_val = *next_ptr;
                if (next_val > max_val) max_val = next_val;
                // next node
                ListNode *nxt = (ListNode *)((char *)next_ptr - 8); // ~offsetof
                // Simplified: just check n2, n3, n4 directly
                if (n2.data > max_val) max_val = n2.data;
                if (n3.data > max_val) max_val = n3.data;
                if (n4.data > max_val) max_val = n4.data;
            }
        }
    }

    // --- inlined swap(n1.tag, n4.tag) via pointers ---
    {
        int *p1 = &(n1.tag);
        int *p2 = &(n4.tag);
        if (*p1 > *p2) {
            int tmp = *p1;
            *p1 = *p2;
            *p2 = tmp;
        }
    }

    // --- inlined delete_node simulation (remove n2 by bypassing it) ---
    {
        // Bypass n2: n1.next now points to n3's data, n2.data = 0
        n1.next_data = &(n3.data);
        n2.data = 0;
        n2.tag  = 0;
    }

    // --- compute result from remaining nodes ---
    {
        int sum = 0;
        if (n1.data > 0) sum = sum + n1.data;
        if (n1.next_data != 0) {
            int nd = *(n1.next_data);  // n3.data (bypassed n2)
            if (nd > 0) sum = sum + nd;
        }
        if (n4.data > 0) sum = sum + n4.data;
        sum = sum + max_val;
        result = sum;
    }

    // --- adjust by tags ---
    {
        if (n1.tag != n4.tag) {
            result = result + n1.tag * 10;
        } else {
            result = result - 50;
        }
    }

    if (result > 5000)  result = 5000;
    if (result < -5000) result = -5000;

    registerOutput("result", &result, sizeof(result));
}
