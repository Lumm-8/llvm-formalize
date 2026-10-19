; RUN: %scalar-executor %s --m4 --entry null_gep_eq | %FileCheck %s --check-prefix=NULL-EQ
; RUN: %scalar-executor %s --m4 --entry null_gep_ne | %FileCheck %s --check-prefix=NULL-NE
; RUN: %scalar-executor %s --m4 --entry null_gep_same | %FileCheck %s --check-prefix=NULL-SAME
; RUN: %scalar-executor %s --m4 --entry aggregate_fields | %FileCheck %s --check-prefix=AGG-FIELDS
; RUN: %scalar-executor %s --m4 --entry aggregate_pointer | %FileCheck %s --check-prefix=AGG-POINTER
; RUN: %scalar-executor %s --m4 --entry zero_aggregate_pointer | %FileCheck %s --check-prefix=ZERO-AGG-POINTER
; RUN: %scalar-executor %s --m4 --entry data_array | %FileCheck %s --check-prefix=DATA-ARRAY
; RUN: %scalar-executor %s --m4 --entry memcpy_pointer | %FileCheck %s --check-prefix=MEMCPY-POINTER
; RUN: %scalar-executor %s --m4 --entry memcpy_partial | %FileCheck %s --check-prefix=MEMCPY-PARTIAL
; RUN: %scalar-executor %s --m4 --entry memcpy_overlap | %FileCheck %s --check-prefix=MEMCPY-OVERLAP
; RUN: %scalar-executor %s --m4 --entry memcpy_zero | %FileCheck %s --check-prefix=MEMCPY-ZERO

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@target = global i8 19
@record = global { i8, i32, [2 x i16], ptr } {
  i8 3, i32 287454020, [2 x i16] [i16 5, i16 6], ptr @target
}
@zero_record = global { i8, ptr } zeroinitializer
@text = constant [4 x i8] c"ABCD"

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define i1 @null_gep_eq() {
entry:
  %p = getelementptr i8, ptr null, i64 1
  %q = icmp eq ptr %p, null
  ret i1 %q
}
; NULL-EQ: M4_EXECUTOR_COMPLETE
; NULL-EQ: return=bv1:0

define i1 @null_gep_ne() {
entry:
  %p = getelementptr i8, ptr null, i64 1
  %q = icmp ne ptr %p, null
  ret i1 %q
}
; NULL-NE: M4_EXECUTOR_COMPLETE
; NULL-NE: return=bv1:1

define i1 @null_gep_same() {
entry:
  %p = getelementptr i8, ptr null, i64 1
  %q = getelementptr i8, ptr null, i64 1
  %same = icmp eq ptr %p, %q
  ret i1 %same
}
; NULL-SAME: return=bv1:1

define i32 @aggregate_fields() {
entry:
  %field = getelementptr { i8, i32, [2 x i16], ptr }, ptr @record, i64 0, i32 1
  %value = load i32, ptr %field, align 4
  ret i32 %value
}
; AGG-FIELDS: return=bv32:287454020

define i8 @aggregate_pointer() {
entry:
  %field = getelementptr { i8, i32, [2 x i16], ptr }, ptr @record, i64 0, i32 3
  %pointer = load ptr, ptr %field, align 8
  %value = load i8, ptr %pointer, align 1
  ret i8 %value
}
; AGG-POINTER: return=bv8:19

define i1 @zero_aggregate_pointer() {
entry:
  %field = getelementptr { i8, ptr }, ptr @zero_record, i64 0, i32 1
  %pointer = load ptr, ptr %field, align 8
  %isnull = icmp eq ptr %pointer, null
  ret i1 %isnull
}
; ZERO-AGG-POINTER: return=bv1:1

define i8 @data_array() {
entry:
  %letter = getelementptr [4 x i8], ptr @text, i64 0, i64 1
  %value = load i8, ptr %letter, align 1
  ret i8 %value
}
; DATA-ARRAY: return=bv8:66

define i8 @memcpy_pointer() {
entry:
  %src = alloca ptr, align 8
  %dst = alloca ptr, align 8
  %target = alloca i8, align 1
  store i8 47, ptr %target, align 1
  store ptr %target, ptr %src, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dst, ptr align 8 %src, i64 8, i1 false)
  %pointer = load ptr, ptr %dst, align 8
  %value = load i8, ptr %pointer, align 1
  ret i8 %value
}
; MEMCPY-POINTER: return=bv8:47

define i8 @memcpy_partial() {
entry:
  %src = alloca ptr, align 8
  %dst = alloca ptr, align 8
  %target = alloca i8, align 1
  store ptr %target, ptr %src, align 8
  store ptr null, ptr %dst, align 8
  %srcbyte = getelementptr i8, ptr %src, i64 1
  %dstbyte = getelementptr i8, ptr %dst, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr %dstbyte, ptr %srcbyte, i64 7, i1 false)
  %pointer = load ptr, ptr %dst, align 8
  %value = load i8, ptr %pointer, align 1
  ret i8 %value
}
; MEMCPY-PARTIAL: status=INCOMPLETE reason=POINTER_PROVENANCE_LOST

define void @memcpy_overlap() {
entry:
  %slot = alloca [8 x i8], align 1
  %other = getelementptr i8, ptr %slot, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr %other, ptr %slot, i64 4, i1 false)
  ret void
}
; MEMCPY-OVERLAP: status=INCOMPLETE reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION

define void @memcpy_zero() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr null, ptr null, i64 0, i1 false)
  ret void
}
; MEMCPY-ZERO: M4_EXECUTOR_COMPLETE
