; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry libc_memcpy | %FileCheck %s --check-prefix=COPY
; RUN: %scalar-executor %s --m4 --entry libc_memmove | %FileCheck %s --check-prefix=MOVE
; RUN: %scalar-executor %s --m4 --entry libc_memset | %FileCheck %s --check-prefix=SET
; RUN: %scalar-executor %s --m4 --entry libc_overlap | %FileCheck %s --check-prefix=OVERLAP
; RUN: %scalar-executor %s --m4 --entry unknown_libc | %FileCheck %s --check-prefix=UNKNOWN

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare ptr @memcpy(ptr, ptr, i64)
declare ptr @memmove(ptr, ptr, i64)
declare ptr @memset(ptr, i32, i64)
declare i32 @memcmp(ptr, ptr, i64)

define i32 @libc_memcpy() {
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  store i32 123456, ptr %a, align 4
  %r = call ptr @memcpy(ptr %b, ptr %a, i64 4)
  %v = load i32, ptr %r, align 4
  ret i32 %v
}
; COPY: M4 status M4_EXECUTOR_COMPLETE
; COPY: return=bv32:123456

define i32 @libc_memmove() {
  %a = alloca [5 x i8], align 1
  store i32 67305985, ptr %a, align 1
  %dest = getelementptr i8, ptr %a, i64 1
  %r = call ptr @memmove(ptr %dest, ptr %a, i64 4)
  %v = load i32, ptr %r, align 1
  ret i32 %v
}
; MOVE: M4 status M4_EXECUTOR_COMPLETE
; MOVE: return=bv32:67305985

define i32 @libc_memset() {
  %a = alloca i32, align 4
  %r = call ptr @memset(ptr %a, i32 171, i64 4)
  %v = load i32, ptr %r, align 4
  ret i32 %v
}
; SET: M4 status M4_EXECUTOR_COMPLETE
; SET: return=bv32:2880154539

define void @libc_overlap() {
  %a = alloca [5 x i8], align 1
  %dest = getelementptr i8, ptr %a, i64 1
  %r = call ptr @memcpy(ptr %dest, ptr %a, i64 4)
  ret void
}
; OVERLAP: M4 status ERROR
; OVERLAP: reason=OVERLAPPING_MEMCPY

define i32 @unknown_libc() {
  %a = alloca i32, align 4
  %v = call i32 @memcmp(ptr %a, ptr %a, i64 4)
  ret i32 %v
}
; UNKNOWN: M4 status UNKNOWN
; UNKNOWN: reason=UNKNOWN-UNSUPPORTED-CALL
