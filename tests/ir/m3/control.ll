; RUN: %scalar-executor %s --entry diamond | %FileCheck %s --check-prefix=DIAMOND
; RUN: %scalar-executor %s --entry only_true | %FileCheck %s --check-prefix=TRUE
; RUN: %scalar-executor %s --entry only_false | %FileCheck %s --check-prefix=FALSE
; RUN: %scalar-executor %s --entry switches | %FileCheck %s --check-prefix=SWITCH
; RUN: %scalar-executor %s --entry phis | %FileCheck %s --check-prefix=PHI
; RUN: %scalar-executor %s --entry parallel_phis | %FileCheck %s --check-prefix=PARALLEL
; RUN: %scalar-executor %s --entry return_void | %FileCheck %s --check-prefix=VOID
; RUN: %scalar-executor %s --entry stable > %t.one
; RUN: %scalar-executor %s --entry stable > %t.two
; RUN: diff %t.one %t.two

define i8 @diamond(i8 %x) {
entry:
  %c = icmp ult i8 %x, 10
  br i1 %c, label %left, label %right
left:
  %a = add i8 %x, 1
  br label %join
right:
  %b = sub i8 %x, 1
  br label %join
join:
  %r = phi i8 [ %a, %left ], [ %b, %right ]
  ret i8 %r
}
; DIAMOND: M3 status M3_EXECUTOR_COMPLETE
; DIAMOND: M3 paths 2
; DIAMOND: M3 stats states=2 forks=1 pruned=0

define i8 @only_true(i8 %x) {
entry:
  %c = icmp eq i8 %x, %x
  br i1 %c, label %yes, label %no
yes:
  ret i8 1
no:
  ret i8 0
}
; TRUE: M3 paths 1
; TRUE: status=COMPLETED
; TRUE: return=bv8:1
; TRUE: M3 stats states=1 forks=0 pruned=1

define i8 @only_false(i8 %x) {
entry:
  %c = icmp ne i8 %x, %x
  br i1 %c, label %yes, label %no
yes:
  ret i8 1
no:
  ret i8 0
}
; FALSE: M3 paths 1
; FALSE: return=bv8:0
; FALSE: M3 stats states=1 forks=0 pruned=1

define i8 @switches(i8 %x) {
entry:
  switch i8 %x, label %default [
    i8 1, label %same
    i8 2, label %same
    i8 3, label %three
  ]
same:
  ret i8 9
three:
  ret i8 7
default:
  ret i8 0
}
; SWITCH: M3 status M3_EXECUTOR_COMPLETE
; SWITCH: M3 paths 4
; SWITCH: M3 stats states=4 forks=3 pruned=0 instructions=5 queries=4

define i8 @phis(i1 %c, i8 %x, i8 %y) {
entry:
  br i1 %c, label %left, label %right
left:
  br label %join
right:
  br label %join
join:
  %r = phi i8 [ %x, %left ], [ %y, %right ]
  ret i8 %r
}
; PHI: M3 status M3_EXECUTOR_COMPLETE
; PHI: M3 paths 2

define i8 @parallel_phis(i1 %c, i8 %x, i8 %y) {
entry:
  br i1 %c, label %left, label %right
left:
  br label %join
right:
  br label %join
join:
  %a = phi i8 [ %x, %left ], [ %y, %right ]
  %b = phi i8 [ %y, %left ], [ %x, %right ]
  %r = xor i8 %a, %b
  ret i8 %r
}
; PARALLEL: M3 status M3_EXECUTOR_COMPLETE
; PARALLEL: M3 paths 2

define void @return_void() {
entry:
  ret void
}
; VOID: M3 status M3_EXECUTOR_COMPLETE
; VOID: return=void

define i8 @stable(i8 %x) {
entry:
  %c = icmp sgt i8 %x, 0
  br i1 %c, label %positive, label %other
positive:
  ret i8 1
other:
  ret i8 2
}
