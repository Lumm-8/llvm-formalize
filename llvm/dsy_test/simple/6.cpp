// clang++-13 -S -emit-llvm -o 4.ll 4.cpp -O1 -fno-discard-value-names
// LD_LIBRARY_PATH=/home/dengshy/github/stp/deps/install/lib:/home/dengshy/github/stp/build/lib:$LD_LIBRARY_PATH ../../../build/bin/opt -passes=translateToStp 4.ll -disable-output -debug-pass-manager
#include "../top.h"

void top () {
    int a, b, c, d;
    bool flag;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    registerInput("d", &d, sizeof(d));
    
   if (a > 10) {
       c = a + b; 
   }
   else if (a < 2) {
        c = a - b;
   }
   else {
        c = a * b;
   }

   if (b > 1) {
        c = c + d;
   }

    registerOutput("c", &c, sizeof(c));
}
