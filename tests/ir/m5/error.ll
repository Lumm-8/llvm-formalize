; RUN: %not %scalar-executor %s --m5 --entry error --output-dir %t.error | %FileCheck %s --check-prefix=ERROR

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
@name_x = private constant [2 x i8] c"x\00"
declare void @__formal_register_input(ptr, i64, ptr)

define void @error() {
entry:
  %slot = alloca [1 x i8], align 1
  %first = getelementptr [1 x i8], ptr %slot, i64 0, i64 0
  call void @__formal_register_input(ptr %first, i64 1, ptr @name_x)
  %v = load i8, ptr %first, align 1
  %bad = icmp eq i8 %v, 1
  br i1 %bad, label %unsafe, label %safe
unsafe:
  %past = getelementptr [1 x i8], ptr %slot, i64 0, i64 1
  store i8 7, ptr %past, align 1
  ret void
safe:
  ret void
}
; ERROR: M5 status UNSAFE
; ERROR: reason=OUT_OF_BOUNDS
