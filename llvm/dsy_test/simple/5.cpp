// clang++-13 -S -emit-llvm -o 4.ll 4.cpp -O1 -fno-discard-value-names
// LD_LIBRARY_PATH=/home/dengshy/github/stp/deps/install/lib:/home/dengshy/github/stp/build/lib:$LD_LIBRARY_PATH ../../../build/bin/opt -passes=translateToStp 4.ll -disable-output -debug-pass-manager
#include "../top.h"

struct Point {
     int x;
     int y;
};

void top () {
    int a, b, c;
    bool flag;

    Point p;
    registerInput("a", &a, sizeof(a));
    registerInput("b", &b, sizeof(b));
    
    p.x = 2;
    p.y = 3;

   if (a > 10) {
       c = p.x; 
   }
   else if (a < 2) {
        c = p.y;
   }
   else {
        c = 5;
   }

   if (b > 1) {
        c = 4;
   }

    registerOutput("c", &c, sizeof(c));
}
