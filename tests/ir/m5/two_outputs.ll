; RUN: %scalar-executor %s --m5 --entry two_outputs --output-dir %t.tuple | %FileCheck %s --check-prefix=TUPLE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
@name_z = private constant [2 x i8] c"z\00"

declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @__formal_assume(i32)

define void @two_outputs() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  %z = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  call void @__formal_register_output(ptr %z, i64 1, ptr @name_z)
  %value = load i8, ptr %x, align 1
  %in-domain = icmp ult i8 %value, 4
  %condition = zext i1 %in-domain to i32
  call void @__formal_assume(i32 %condition)
  %plus-one = add i8 %value, 1
  %xor-three = xor i8 %value, 3
  store i8 %plus-one, ptr %y, align 1
  store i8 %xor-three, ptr %z, align 1
  ret void
}
; TUPLE: M5 status SAFE
