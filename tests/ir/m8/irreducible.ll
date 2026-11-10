; RUN: %scalar-executor %s --entry entry --loop-bound 2 | %FileCheck %s
; CHECK: M3 status UNKNOWN
; CHECK: reason=UNKNOWN-IRREDUCIBLE-CYCLE

define void @entry() {
start:
  br i1 true, label %left, label %right
left:
  br label %right
right:
  br label %left
}
