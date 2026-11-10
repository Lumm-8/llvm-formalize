; RUN: %scalar-executor %s --m5 --entry scalar --output-dir %t.scalar | %FileCheck %s --check-prefix=SCALAR
; RUN: %scalar-executor %s --m5 --entry inout --output-dir %t.inout | %FileCheck %s --check-prefix=INOUT
; RUN: %not %scalar-executor %s --m5 --entry before_input --output-dir %t.before | %FileCheck %s --check-prefix=BEFORE
; RUN: %not %scalar-executor %s --m5 --entry symbolic_size --output-dir %t.size | %FileCheck %s --check-prefix=SIZE
; RUN: %not %scalar-executor %s --m5 --entry duplicate --output-dir %t.duplicate | %FileCheck %s --check-prefix=DUP
; RUN: %not %scalar-executor %s --m5 --entry overlap --output-dir %t.overlap | %FileCheck %s --check-prefix=OVERLAP
; RUN: %scalar-executor %s --m5 --entry byte_array --output-dir %t.array | %FileCheck %s --check-prefix=ARRAY
; RUN: %not %scalar-executor %s --m5 --entry output_alias --output-dir %t.alias | %FileCheck %s --check-prefix=ALIAS
; RUN: %not %scalar-executor %s --m5 --entry initialized_before_input --output-dir %t.prior | %FileCheck %s --check-prefix=PRIOR
; RUN: %not %scalar-executor %s --m5 --entry memcpy_before_input --output-dir %t.copy | %FileCheck %s --check-prefix=COPY
; RUN: %not %scalar-executor %s --m5 --entry pointer_byte_slice --output-dir %t.pointer-byte | %FileCheck %s --check-prefix=POINTER-BYTE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
@name_z = private constant [2 x i8] c"z\00"
@initialized = global i8 9

declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @__formal_assume(i32)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define void @scalar() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  %v = load i8, ptr %x, align 1
  %a = add i8 %v, 1
  store i8 %a, ptr %y, align 1
  ret void
}
; SCALAR: M5 status SAFE

define void @inout() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %x, i64 1, ptr @name_x)
  %v = load i8, ptr %x, align 1
  %a = xor i8 %v, 255
  store i8 %a, ptr %x, align 1
  ret void
}
; INOUT: M5 status SAFE

define void @before_input() {
entry:
  %x = alloca i8, align 1
  %v = load i8, ptr %x, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  ret void
}
; BEFORE: M5 status UNKNOWN
; BEFORE: reason=UNINITIALIZED_READ

define void @symbolic_size() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  %v = load i8, ptr %x, align 1
  %size = zext i8 %v to i64
  call void @__formal_register_output(ptr %x, i64 %size, ptr @name_y)
  ret void
}
; SIZE: M5 status UNKNOWN
; SIZE: reason=UNKNOWN-SYMBOLIC-REGISTRATION-SIZE

define void @duplicate() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_input(ptr %y, i64 1, ptr @name_x)
  ret void
}
; DUP: M5 status UNKNOWN
; DUP: reason=UNKNOWN-REGISTRATION-NAME

define void @overlap() {
entry:
  %x = alloca [2 x i8], align 1
  %a = getelementptr [2 x i8], ptr %x, i64 0, i64 0
  %b = getelementptr [2 x i8], ptr %x, i64 0, i64 1
  call void @__formal_register_input(ptr %a, i64 2, ptr @name_x)
  call void @__formal_register_input(ptr %b, i64 1, ptr @name_z)
  ret void
}
; OVERLAP: M5 status UNKNOWN
; OVERLAP: reason=UNKNOWN-REGISTRATION-OVERLAP

define void @byte_array() {
entry:
  %input = alloca [2 x i8], align 1
  %output = alloca [2 x i8], align 1
  %in0 = getelementptr [2 x i8], ptr %input, i64 0, i64 0
  %in1 = getelementptr [2 x i8], ptr %input, i64 0, i64 1
  %out0 = getelementptr [2 x i8], ptr %output, i64 0, i64 0
  %out1 = getelementptr [2 x i8], ptr %output, i64 0, i64 1
  call void @__formal_register_input(ptr %in0, i64 1, ptr @name_x)
  call void @__formal_register_input(ptr %in1, i64 1, ptr @name_z)
  call void @__formal_register_output(ptr %out0, i64 2, ptr @name_y)
  %a = load i8, ptr %in0, align 1
  %b = load i8, ptr %in1, align 1
  store i8 %b, ptr %out0, align 1
  store i8 %a, ptr %out1, align 1
  ret void
}
; ARRAY: M5 status SAFE

define void @output_alias() {
entry:
  %output = alloca [2 x i8], align 1
  %out0 = getelementptr [2 x i8], ptr %output, i64 0, i64 0
  %out1 = getelementptr [2 x i8], ptr %output, i64 0, i64 1
  store i8 1, ptr %out0, align 1
  store i8 2, ptr %out1, align 1
  call void @__formal_register_output(ptr %out0, i64 2, ptr @name_y)
  call void @__formal_register_output(ptr %out1, i64 1, ptr @name_z)
  ret void
}
; ALIAS: M5 status UNKNOWN
; ALIAS: reason=UNKNOWN-REGISTRATION-OVERLAP

define void @initialized_before_input() {
entry:
  %old = load i8, ptr @initialized, align 1
  call void @__formal_register_input(ptr @initialized, i64 1, ptr @name_x)
  ret void
}
; PRIOR: M5 status UNKNOWN
; PRIOR: reason=UNKNOWN-USE-BEFORE-INPUT

define void @memcpy_before_input() {
entry:
  %copy = alloca i8, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr %copy, ptr @initialized, i64 1, i1 false)
  call void @__formal_register_input(ptr @initialized, i64 1, ptr @name_x)
  ret void
}
; COPY: M5 status UNKNOWN
; COPY: reason=UNKNOWN-USE-BEFORE-INPUT

define void @pointer_byte_slice() {
entry:
  %pointer = alloca ptr, align 8
  %byte = getelementptr i8, ptr %pointer, i64 1
  call void @__formal_register_input(ptr %byte, i64 1, ptr @name_x)
  ret void
}
; POINTER-BYTE: M5 status UNKNOWN
; POINTER-BYTE: reason=UNKNOWN-REGISTRATION-UNSUPPORTED
