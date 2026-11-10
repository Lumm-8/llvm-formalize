; RUN: %not %scalar-executor %s --m5 --entry formal_top --output-dir %t.reasons --loop-bound 1 --max-states 2 | %FileCheck %s --check-prefix=MIXED
; MIXED: M5 status UNKNOWN
; MIXED-DAG: reason=UNKNOWN-LOOP-BOUND
; MIXED-DAG: reason=UNKNOWN-STATE-LIMIT

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
declare void @__formal_register_input(ptr, i64, ptr)

define void @formal_top() {
entry:
  %input = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  %value = load i8, ptr %input, align 1
  %choose = icmp eq i8 %value, 0
  br i1 %choose, label %loop_header, label %normal
loop_header:
  %i = phi i32 [ 0, %entry ], [ %next, %loop_latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %loop_latch, label %normal
loop_latch:
  %next = add i32 %i, 1
  br label %loop_header
normal:
  %is_one = icmp eq i8 %value, 1
  br i1 %is_one, label %done_one, label %done_other
done_one:
  ret void
done_other:
  ret void
}
