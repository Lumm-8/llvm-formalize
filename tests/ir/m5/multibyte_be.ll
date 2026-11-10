; RUN: %scalar-executor %s --m5 --entry multibyte --output-dir %t.multi | %FileCheck %s --check-prefix=MULTI

target datalayout = "E-p:64:64-i64:64-n8:16:32:64-S128"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @multibyte() {
entry:
  %x = alloca i16, align 2
  %y = alloca i16, align 2
  call void @__formal_register_input(ptr %x, i64 2, ptr @name_x)
  call void @__formal_register_output(ptr %y, i64 2, ptr @name_y)
  %value = load i16, ptr %x, align 2
  %next = add i16 %value, 1
  store i16 %next, ptr %y, align 2
  ret void
}
; MULTI: M5 status SAFE
