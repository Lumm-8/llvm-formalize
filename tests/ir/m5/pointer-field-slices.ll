; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m5 --entry stack_field_input --output-dir %t.stack | %FileCheck %s --check-prefix=STACK
; RUN: %scalar-executor %s --m5 --entry global_field_input --output-dir %t.global | %FileCheck %s --check-prefix=GLOBAL
; RUN: %not %scalar-executor %s --m5 --entry pointer_byte_input --output-dir %t.pointer | %FileCheck %s --check-prefix=POINTER

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
%WithPointer = type { i8, ptr }
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
@global_target = global i8 3
@global_record = global %WithPointer { i8 0, ptr @global_target }
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @stack_field_input() {
entry:
  %records = alloca [2 x %WithPointer], align 8
  %target = alloca i8, align 1
  %output = alloca i8, align 1
  store i8 7, ptr %target, align 1
  %second = getelementptr [2 x %WithPointer], ptr %records, i64 0, i64 1
  store %WithPointer { i8 0, ptr null }, ptr %second, align 8
  %pointer_field = getelementptr %WithPointer, ptr %second, i64 0, i32 1
  store ptr %target, ptr %pointer_field, align 8
  %tag = getelementptr %WithPointer, ptr %second, i64 0, i32 0
  call void @__formal_register_input(ptr %tag, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %output, i64 1, ptr @name_y)
  %record = load %WithPointer, ptr %second, align 8
  %input = extractvalue %WithPointer %record, 0
  %pointer = extractvalue %WithPointer %record, 1
  %base = load i8, ptr %pointer, align 1
  %sum = add i8 %input, %base
  store i8 %sum, ptr %output, align 1
  ret void
}
; STACK: M5 status SAFE

define void @global_field_input() {
entry:
  %output = alloca i8, align 1
  %tag = getelementptr %WithPointer, ptr @global_record, i64 0, i32 0
  call void @__formal_register_input(ptr %tag, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %output, i64 1, ptr @name_y)
  %record = load %WithPointer, ptr @global_record, align 8
  %input = extractvalue %WithPointer %record, 0
  %pointer = extractvalue %WithPointer %record, 1
  %base = load i8, ptr %pointer, align 1
  %sum = add i8 %input, %base
  store i8 %sum, ptr %output, align 1
  ret void
}
; GLOBAL: M5 status SAFE

define void @pointer_byte_input() {
entry:
  %record = alloca %WithPointer, align 8
  %pointer_byte = getelementptr i8, ptr %record, i64 9
  call void @__formal_register_input(ptr %pointer_byte, i64 1, ptr @name_x)
  ret void
}
; POINTER: M5 status UNKNOWN
; POINTER: reason=UNKNOWN-REGISTRATION-UNSUPPORTED
