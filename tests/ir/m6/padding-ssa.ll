; RUN: %opt -passes=verify -disable-output %s
; RUN: %not %scalar-executor %s --m5 --entry copy_all_bytes --output-dir %t.all | %FileCheck %s --check-prefix=ALL
; RUN: %scalar-executor %s --m5 --entry copy_field_bytes --output-dir %t.fields | %FileCheck %s --check-prefix=FIELDS
; RUN: %scalar-executor %s --m4 --entry drop_initialized_padding | %FileCheck %s --check-prefix=DROP
; RUN: %scalar-executor %s --m4 --entry drop_padding_shadow | %FileCheck %s --check-prefix=SHADOW
; RUN: %scalar-executor %s --m4 --entry bytewise_copy_keeps_padding | %FileCheck %s --check-prefix=BYTEWISE
; RUN: %scalar-executor %s --m5 --entry register_padding_after_load --output-dir %t.latepad | %FileCheck %s --check-prefix=LATEPAD
; RUN: %not %scalar-executor %s --m5 --entry memcpy_overlap_error --output-dir %t.overlap | %FileCheck %s --check-prefix=OVERLAP

target datalayout = "e-p:64:64-i64:64-i128:128-n8:16:32:64-S128"
%U = type { i8, i32 }
%Wide = type { i8, i128 }
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
@name_z = private constant [2 x i8] c"z\00"
@name_w = private constant [2 x i8] c"w\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define void @copy_all_bytes() {
  %source = alloca %U, align 4
  %target = alloca %U, align 4
  call void @__formal_register_input(ptr %source, i64 8, ptr @name_x)
  call void @__formal_register_output(ptr %target, i64 8, ptr @name_y)
  %value = load %U, ptr %source, align 4
  store %U %value, ptr %target, align 4
  ret void
}
; ALL: M5 status UNKNOWN
; ALL: reason=UNINITIALIZED_READ

define void @copy_field_bytes() {
  %source = alloca %U, align 4
  %target = alloca %U, align 4
  %source_tag = getelementptr %U, ptr %source, i64 0, i32 0
  %source_value = getelementptr %U, ptr %source, i64 0, i32 1
  %target_tag = getelementptr %U, ptr %target, i64 0, i32 0
  %target_value = getelementptr %U, ptr %target, i64 0, i32 1
  call void @__formal_register_input(ptr %source_tag, i64 1, ptr @name_x)
  call void @__formal_register_input(ptr %source_value, i64 4, ptr @name_z)
  call void @__formal_register_output(ptr %target_tag, i64 1, ptr @name_y)
  call void @__formal_register_output(ptr %target_value, i64 4, ptr @name_w)
  %value = load %U, ptr %source, align 4
  store %U %value, ptr %target, align 4
  ret void
}
; FIELDS: M5 status SAFE

define i8 @drop_initialized_padding() {
  %source = alloca %U, align 4
  %target = alloca %U, align 4
  call void @llvm.memset.p0.i64(ptr %source, i8 90, i64 8, i1 false)
  call void @llvm.memset.p0.i64(ptr %target, i8 90, i64 8, i1 false)
  %value = load %U, ptr %source, align 4
  store %U %value, ptr %target, align 4
  %padding = getelementptr i8, ptr %target, i64 1
  %byte = load i8, ptr %padding, align 1
  ret i8 %byte
}
; DROP: M4 status UNKNOWN
; DROP: reason=UNINITIALIZED_READ

define void @drop_padding_shadow() {
  %source = alloca %Wide, align 16
  %target = alloca %Wide, align 16
  %object = alloca i8, align 1
  store %Wide { i8 1, i128 2 }, ptr %source, align 16
  %source_padding = getelementptr i8, ptr %source, i64 1
  store ptr %object, ptr %source_padding, align 1
  %value = load %Wide, ptr %source, align 16
  store %Wide %value, ptr %target, align 16
  %target_padding = getelementptr i8, ptr %target, i64 1
  %p = load ptr, ptr %target_padding, align 1
  ret void
}
; SHADOW: M4 status UNKNOWN
; SHADOW: reason=UNINITIALIZED_READ

define i8 @bytewise_copy_keeps_padding() {
  %source = alloca %U, align 4
  %target = alloca %U, align 4
  call void @llvm.memset.p0.i64(ptr %source, i8 90, i64 8, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr %target, ptr %source, i64 8, i1 false)
  %padding = getelementptr i8, ptr %target, i64 1
  %byte = load i8, ptr %padding, align 1
  ret i8 %byte
}
; BYTEWISE: return=bv8:90

define void @register_padding_after_load() {
  %source = alloca %U, align 4
  %output = alloca i8, align 1
  store %U { i8 1, i32 2 }, ptr %source, align 4
  %aggregate = load %U, ptr %source, align 4
  %padding = getelementptr i8, ptr %source, i64 1
  call void @__formal_register_input(ptr %padding, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %output, i64 1, ptr @name_y)
  store i8 7, ptr %output, align 1
  ret void
}
; LATEPAD: M5 status SAFE

define void @memcpy_overlap_error() {
  %slot = alloca [5 x i8], align 1
  %dest = getelementptr i8, ptr %slot, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %slot, i64 4, i1 false)
  ret void
}
; OVERLAP: M5 status UNSAFE
; OVERLAP: reason=OVERLAPPING_MEMCPY
