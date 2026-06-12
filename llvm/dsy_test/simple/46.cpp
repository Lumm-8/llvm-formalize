// Test 46: struct comparison with multiple fields
#include "../top.h"
struct Item { int id; int val; };
void top () {
    int a, b, c, r;
    Item it1, it2, it3;
    registerInput("a", &a, sizeof(a)); registerInput("b", &b, sizeof(b));
    registerInput("c", &c, sizeof(c));
    it1.id = 1; it1.val = a;
    it2.id = 2; it2.val = b;
    it3.id = 3; it3.val = c;
    if (it1.val > it2.val) r = it1.val; else r = it2.val;
    if (it3.val > r) r = it3.val;
    r = r + it1.val - it2.val + it3.val;
    if (r > 5000) r = 5000;
    registerOutput("r", &r, sizeof(r));
}
