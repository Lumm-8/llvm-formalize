; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry packed_field | %FileCheck %s --check-prefix=PACKED
; RUN: %scalar-executor %s --m4 --entry aggregate_roundtrip | %FileCheck %s --check-prefix=AGG
; RUN: %scalar-executor %s --m4 --entry insert_field | %FileCheck %s --check-prefix=INSERT
; RUN: %scalar-executor %s --m4 --entry union_bytes | %FileCheck %s --check-prefix=UNION
; RUN: %scalar-executor %s --m4 --entry move_forward | %FileCheck %s --check-prefix=FORWARD
; RUN: %scalar-executor %s --m4 --entry move_backward | %FileCheck %s --check-prefix=BACKWARD
; RUN: %scalar-executor %s --m4 --entry set_bytes | %FileCheck %s --check-prefix=SET
; RUN: %scalar-executor %s --m4 --entry copy_padding | %FileCheck %s --check-prefix=PADDING
; RUN: %scalar-executor %s --m4 --entry overlap_copy | %FileCheck %s --check-prefix=OVERLAP
; RUN: %scalar-executor %s --m4 --entry noalias_overlap_move | %FileCheck %s --check-prefix=NOALIAS

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
%P = type <{ i8, i32 }>
%U = type { i8, i32 }

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)

define i32 @packed_field() {
  %slot = alloca %P, align 1
  store %P <{ i8 7, i32 287454020 }>, ptr %slot, align 1
  %field = getelementptr %P, ptr %slot, i64 0, i32 1
  %value = load i32, ptr %field, align 1
  ret i32 %value
}
; PACKED: return=bv32:287454020

define i32 @aggregate_roundtrip() {
  %slot = alloca %U, align 4
  store %U { i8 7, i32 287454020 }, ptr %slot, align 4
  %value = load %U, ptr %slot, align 4
  %field = extractvalue %U %value, 1
  ret i32 %field
}
; AGG: return=bv32:287454020

define i32 @insert_field() {
  %value = insertvalue %U zeroinitializer, i32 99, 1
  %field = extractvalue %U %value, 1
  ret i32 %field
}
; INSERT: return=bv32:99

define i8 @union_bytes() {
  %slot = alloca [4 x i8], align 4
  store i32 287454020, ptr %slot, align 4
  %value = load i8, ptr %slot, align 1
  ret i8 %value
}
; UNION: return=bv8:68

define i32 @move_forward() {
  %slot = alloca [5 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %slot, i8 0, i64 5, i1 false)
  store i32 67305985, ptr %slot, align 1
  %dest = getelementptr i8, ptr %slot, i64 1
  call void @llvm.memmove.p0.p0.i64(ptr %dest, ptr %slot, i64 4, i1 false)
  %value = load i32, ptr %dest, align 1
  ret i32 %value
}
; FORWARD: return=bv32:67305985

define i32 @move_backward() {
  %slot = alloca [5 x i8], align 1
  call void @llvm.memset.p0.i64(ptr %slot, i8 0, i64 5, i1 false)
  %source = getelementptr i8, ptr %slot, i64 1
  store i32 67305985, ptr %source, align 1
  call void @llvm.memmove.p0.p0.i64(ptr %slot, ptr %source, i64 4, i1 false)
  %value = load i32, ptr %slot, align 1
  ret i32 %value
}
; BACKWARD: return=bv32:67305985

define i32 @set_bytes() {
  %slot = alloca i32, align 4
  call void @llvm.memset.p0.i64(ptr %slot, i8 -85, i64 4, i1 false)
  %value = load i32, ptr %slot, align 4
  ret i32 %value
}
; SET: return=bv32:2880154539

define i8 @copy_padding() {
  %source = alloca %U, align 4
  %dest = alloca %U, align 4
  store %U { i8 1, i32 2 }, ptr %source, align 4
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %source, i64 8, i1 false)
  %padding = getelementptr i8, ptr %dest, i64 1
  %value = load i8, ptr %padding, align 1
  ret i8 %value
}
; PADDING: M4 status UNKNOWN
; PADDING: reason=UNINITIALIZED_READ

define void @overlap_copy() {
  %slot = alloca [5 x i8], align 1
  %dest = getelementptr i8, ptr %slot, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %slot, i64 4, i1 false)
  ret void
}
; OVERLAP: M4 status ERROR
; OVERLAP: reason=OVERLAPPING_MEMCPY

define void @noalias_overlap_move() {
  %slot = alloca [5 x i8], align 1
  %dest = getelementptr i8, ptr %slot, i64 1
  call void @llvm.memmove.p0.p0.i64(ptr noalias %dest, ptr %slot, i64 4, i1 false)
  ret void
}
; NOALIAS: M4 status UNKNOWN
