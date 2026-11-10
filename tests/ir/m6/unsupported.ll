; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry inline_copy | %FileCheck %s --check-prefix=INLINE-COPY
; RUN: %scalar-executor %s --m4 --entry inline_set | %FileCheck %s --check-prefix=INLINE-SET
; RUN: %scalar-executor %s --m4 --entry volatile_move | %FileCheck %s --check-prefix=VOLATILE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare void @llvm.memcpy.inline.p0.p0.i64(ptr, ptr, i64 immarg, i1)
declare void @llvm.memset.inline.p0.i64(ptr, i8, i64 immarg, i1)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)

define void @inline_copy() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  store i8 1, ptr %src, align 1
  call void @llvm.memcpy.inline.p0.p0.i64(ptr %dst, ptr %src, i64 1, i1 false)
  ret void
}
; INLINE-COPY: M4 status UNKNOWN

define void @inline_set() {
  %dst = alloca i8, align 1
  call void @llvm.memset.inline.p0.i64(ptr %dst, i8 1, i64 1, i1 false)
  ret void
}
; INLINE-SET: M4 status UNKNOWN

define void @volatile_move() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  store i8 1, ptr %src, align 1
  call void @llvm.memmove.p0.p0.i64(ptr %dst, ptr %src, i64 1, i1 true)
  ret void
}
; VOLATILE: M4 status UNKNOWN
