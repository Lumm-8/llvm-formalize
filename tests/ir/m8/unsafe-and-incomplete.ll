; RUN: %not %scalar-executor %s --m5 --entry formal_top --output-dir %t.unsafe --loop-bound 2 | %FileCheck %s
; CHECK: M5 status UNSAFE
; CHECK-DAG: reason=REACHED-UNREACHABLE
; CHECK-DAG: reason=UNKNOWN-LOOP-BOUND

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
declare void @__formal_register_input(ptr, i64, ptr)

define void @formal_top() {
entry:
  %input = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  %value = load i8, ptr %input, align 1
  %bad = icmp eq i8 %value, 0
  br i1 %bad, label %error, label %header
error:
  unreachable
header:
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, 3
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header
done:
  ret void
}
