#include "../top.h"
struct Node { int val; int next; };
void top () {
    int a,b,c,d, r;
    Node n1,n2;
    registerInput("a",&a,sizeof(a)); registerInput("b",&b,sizeof(b));
    registerInput("c",&c,sizeof(c)); registerInput("d",&d,sizeof(d));
    n1.val=a; n2.val=b; n1.next=c; n2.next=d;
    if (a>b) r=n1.val+n1.next; else r=n2.val+n2.next;
    if (r>0) { n1.val=r; n2.val=r*2; if(n1.val>n2.val) r=n1.val; else r=n2.val; }
    else r=-r;
    if(r>1000) r=1000;
    registerOutput("r",&r,sizeof(r));
}
