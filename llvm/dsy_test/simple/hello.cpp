#include "../top.h"
void top () {
    int a;
    registerInput("a", &a, sizeof(a));
    registerOutput("a", &a, sizeof(a));
}
