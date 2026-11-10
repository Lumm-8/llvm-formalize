; RUN: %scalar-executor %s --m5 --entry loop_top --output-dir %t.relation --loop-bound 3 | %FileCheck %s
; CHECK: M5 status SAFE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @__formal_assume(i32)

define void @loop_top() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  %value = load i8, ptr %x, align 1
  %domain = icmp ult i8 %value, 4
  %domain32 = zext i1 %domain to i32
  call void @__formal_assume(i32 %domain32)
  br label %header
header:
  %i = phi i8 [ 0, %entry ], [ %next, %body ]
  %sum = phi i8 [ 0, %entry ], [ %next_sum, %body ]
  %more = icmp ult i8 %i, %value
  br i1 %more, label %body, label %done
body:
  %next = add i8 %i, 1
  %next_sum = add i8 %sum, %next
  br label %header
done:
  store i8 %sum, ptr %y, align 1
  ret void
}
