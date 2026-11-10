; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry nested_array | %FileCheck %s --check-prefix=NESTED
; RUN: %scalar-executor %s --m4 --entry struct_copy | %FileCheck %s --check-prefix=COPY
; RUN: %scalar-executor %s --m4 --entry undef_insert | %FileCheck %s --check-prefix=UNDEF
; RUN: %scalar-executor %s --m4 --entry flexible_inside | %FileCheck %s --check-prefix=FLEX-IN
; RUN: %scalar-executor %s --m4 --entry flexible_outside | %FileCheck %s --check-prefix=FLEX-OUT
; RUN: %scalar-executor %s --m4 --entry pointer_struct_copy | %FileCheck %s --check-prefix=PTR-COPY
; RUN: %scalar-executor %s --m4 --entry pointer_partial_overwrite | %FileCheck %s --check-prefix=PTR-PARTIAL
; RUN: %scalar-executor %s --m4 --entry partial_global_good | %FileCheck %s --check-prefix=GLOBAL-GOOD
; RUN: %scalar-executor %s --m4 --entry partial_global_unknown | %FileCheck %s --check-prefix=GLOBAL-UNKNOWN
; RUN: %scalar-executor %s --m4 --entry pointer_overlap_move | %FileCheck %s --check-prefix=PTR-MOVE
; RUN: %scalar-executor %s --m4 --entry nested_global | %FileCheck %s --check-prefix=NESTED-GLOBAL
; RUN: %scalar-executor %s --m4 --entry zero_global_padding | %FileCheck %s --check-prefix=ZERO-PAD
; RUN: %scalar-executor %s --m4 --entry pointer_memset | %FileCheck %s --check-prefix=PTR-SET
; RUN: %scalar-executor %s --m4 --entry pointer_partial_copy | %FileCheck %s --check-prefix=PTR-PARTIAL-COPY
; RUN: %scalar-executor %s --m4 --entry pointer_global_aggregate | %FileCheck %s --check-prefix=PTR-GLOBAL

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
%P = type <{ i8, i32 }>
%U = type { i8, i32 }
%N = type { %U, [2 x %P] }
%F = type { i32, [0 x i8] }
%WithPointer = type { ptr, i8 }
@partial = global %U { i8 undef, i32 9 }
@nested_constant = constant %N { %U { i8 3, i32 4 }, [2 x %P] [%P <{ i8 5, i32 6 }>, %P <{ i8 7, i32 8 }>] }
@zero_aggregate = global %U zeroinitializer
@global_target = global i8 42
@pointer_constant = global %WithPointer { ptr @global_target, i8 1 }

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)

define i32 @nested_array() {
  %slot = alloca %N, align 4
  store %N { %U { i8 1, i32 2 }, [2 x %P] [%P <{ i8 3, i32 4 }>, %P <{ i8 5, i32 6 }>] }, ptr %slot, align 4
  %field = getelementptr %N, ptr %slot, i64 0, i32 1, i64 1, i32 1
  %value = load i32, ptr %field, align 1
  ret i32 %value
}
; NESTED: return=bv32:6

define i32 @struct_copy() {
  %src = alloca %U, align 4
  %dst = alloca %U, align 4
  store %U { i8 7, i32 23 }, ptr %src, align 4
  %value = load %U, ptr %src, align 4
  store %U %value, ptr %dst, align 4
  %field = getelementptr %U, ptr %dst, i64 0, i32 1
  %result = load i32, ptr %field, align 4
  ret i32 %result
}
; COPY: return=bv32:23

define i32 @undef_insert() {
  %a = insertvalue %U undef, i8 9, 0
  %b = insertvalue %U %a, i32 77, 1
  %value = extractvalue %U %b, 1
  ret i32 %value
}
; UNDEF: return=bv32:77

define i8 @flexible_inside() {
  %slot = alloca [8 x i8], align 4
  %last = getelementptr i8, ptr %slot, i64 7
  store i8 42, ptr %last, align 1
  %trailing = getelementptr %F, ptr %slot, i64 0, i32 1, i64 3
  %value = load i8, ptr %trailing, align 1
  ret i8 %value
}
; FLEX-IN: return=bv8:42

define i8 @flexible_outside() {
  %slot = alloca %F, align 4
  %trailing = getelementptr %F, ptr %slot, i64 0, i32 1, i64 3
  %value = load i8, ptr %trailing, align 1
  ret i8 %value
}
; FLEX-OUT: M4 status ERROR
; FLEX-OUT: reason=OUT_OF_BOUNDS

define i8 @pointer_struct_copy() {
  %target = alloca i8, align 1
  %src = alloca %WithPointer, align 8
  %dst = alloca %WithPointer, align 8
  store i8 31, ptr %target, align 1
  %a = insertvalue %WithPointer undef, ptr %target, 0
  %b = insertvalue %WithPointer %a, i8 5, 1
  store %WithPointer %b, ptr %src, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false)
  %value = load %WithPointer, ptr %dst, align 8
  %p = extractvalue %WithPointer %value, 0
  %result = load i8, ptr %p, align 1
  ret i8 %result
}
; PTR-COPY: return=bv8:31

define i8 @pointer_partial_overwrite() {
  %target = alloca i8, align 1
  %slot = alloca %WithPointer, align 8
  store i8 31, ptr %target, align 1
  %a = insertvalue %WithPointer undef, ptr %target, 0
  store %WithPointer %a, ptr %slot, align 8
  store i8 0, ptr %slot, align 1
  %value = load %WithPointer, ptr %slot, align 8
  %p = extractvalue %WithPointer %value, 0
  %result = load i8, ptr %p, align 1
  ret i8 %result
}
; PTR-PARTIAL: M4 status UNKNOWN
; PTR-PARTIAL: reason=POINTER_PROVENANCE_LOST

define i32 @partial_global_good() {
  %field = getelementptr %U, ptr @partial, i64 0, i32 1
  %value = load i32, ptr %field, align 4
  ret i32 %value
}
; GLOBAL-GOOD: return=bv32:9

define i8 @partial_global_unknown() {
  %field = getelementptr %U, ptr @partial, i64 0, i32 0
  %value = load i8, ptr %field, align 1
  ret i8 %value
}
; GLOBAL-UNKNOWN: M4 status UNKNOWN
; GLOBAL-UNKNOWN: reason=UNINITIALIZED_READ

define i8 @pointer_overlap_move() {
  %target = alloca i8, align 1
  %slot = alloca [9 x i8], align 8
  store i8 41, ptr %target, align 1
  store ptr %target, ptr %slot, align 1
  %dest = getelementptr i8, ptr %slot, i64 1
  call void @llvm.memmove.p0.p0.i64(ptr %dest, ptr %slot, i64 8, i1 false)
  %p = load ptr, ptr %dest, align 1
  %value = load i8, ptr %p, align 1
  ret i8 %value
}
; PTR-MOVE: return=bv8:41

define i32 @nested_global() {
  %field = getelementptr %N, ptr @nested_constant, i64 0, i32 1, i64 1, i32 1
  %value = load i32, ptr %field, align 1
  ret i32 %value
}
; NESTED-GLOBAL: return=bv32:8

define i8 @zero_global_padding() {
  %padding = getelementptr i8, ptr @zero_aggregate, i64 1
  %value = load i8, ptr %padding, align 1
  ret i8 %value
}
; ZERO-PAD: return=bv8:0

define void @pointer_memset() {
  %target = alloca i8, align 1
  %slot = alloca ptr, align 8
  store ptr %target, ptr %slot, align 8
  call void @llvm.memset.p0.i64(ptr %slot, i8 0, i64 1, i1 false)
  %value = load ptr, ptr %slot, align 8
  ret void
}
; PTR-SET: M4 status UNKNOWN
; PTR-SET: reason=POINTER_PROVENANCE_LOST

define void @pointer_partial_copy() {
  %target = alloca i8, align 1
  %slot = alloca ptr, align 8
  %bytes = alloca i32, align 4
  store ptr %target, ptr %slot, align 8
  store i32 0, ptr %bytes, align 4
  call void @llvm.memcpy.p0.p0.i64(ptr %slot, ptr %bytes, i64 4, i1 false)
  %value = load ptr, ptr %slot, align 8
  ret void
}
; PTR-PARTIAL-COPY: M4 status UNKNOWN
; PTR-PARTIAL-COPY: reason=POINTER_PROVENANCE_LOST

define i8 @pointer_global_aggregate() {
  %aggregate = load %WithPointer, ptr @pointer_constant, align 8
  %p = extractvalue %WithPointer %aggregate, 0
  %value = load i8, ptr %p, align 1
  ret i8 %value
}
; PTR-GLOBAL: return=bv8:42
