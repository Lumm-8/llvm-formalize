; RUN: %scalar-executor %s --entry body --loop-bound 0 | %FileCheck %s --check-prefix=M3
; RUN: %not %scalar-executor %s --m5 --entry formal_top --loop-bound 0 --output-dir %t.m5 | %FileCheck %s --check-prefix=M5
; M3: M3 status UNKNOWN
; M3: reason=UNKNOWN-IRREDUCIBLE-CYCLE
; M5: M5 status UNKNOWN
; M5: reason=UNKNOWN-IRREDUCIBLE-CYCLE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
declare void @__formal_register_input(ptr, i64, ptr)

define void @body(i1 %choice) {
entry:
  br label %outer
outer:
  br label %a
a:
  br i1 %choice, label %b, label %c
b:
  br label %c
c:
  %from_a = phi i1 [ true, %a ], [ false, %b ]
  br i1 %from_a, label %b, label %maybe_outer
maybe_outer:
  br i1 false, label %outer_latch, label %done
outer_latch:
  br label %outer
done:
  ret void
}

define void @formal_top() {
entry:
  %input = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  %x = load i8, ptr %input, align 1
  %choice = icmp eq i8 %x, 0
  call void @body(i1 %choice)
  ret void
}
