; RUN: %scalar-executor %s --m4 --entry raw_pointer_bytes | %FileCheck %s --check-prefix=RAW
; RUN: %scalar-executor %s --m4 --entry merged_pointer_bytes | %FileCheck %s --check-prefix=MERGED
; RUN: %scalar-executor %s --m4 --entry partially_overwritten_pointer_bytes | %FileCheck %s --check-prefix=PARTIAL
; RUN: %scalar-executor %s --m4 --entry overwritten_pointer_bytes | %FileCheck %s --check-prefix=OVERWRITE
; RUN: %scalar-executor %s --m4 --entry overwritten_single_byte | %FileCheck %s --check-prefix=BYTE
; RUN: %scalar-executor %s --m4 --entry copied_pointer_bytes | %FileCheck %s --check-prefix=COPY
; RUN: %scalar-executor %s --m4 --entry pointer_initializer_bytes | %FileCheck %s --check-prefix=INITIALIZER
; RUN: %scalar-executor %s --m4 --entry null_pointer_bytes | %FileCheck %s --check-prefix=NULL

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@target = global i8 7
@merge_a = unnamed_addr constant i8 7
@merge_b = unnamed_addr constant i8 7
@pointer_initializer = constant ptr @target
@null_pointer_initializer = constant ptr null

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define i1 @raw_pointer_bytes() {
entry:
  %slot = alloca ptr, align 8
  store ptr @target, ptr %slot, align 8
  %bits = load i64, ptr %slot, align 8
  %fixed = icmp eq i64 %bits, 65536
  ret i1 %fixed
}
; RAW: M4 status UNKNOWN
; RAW: reason=POINTER_PROVENANCE_LOST

define i1 @merged_pointer_bytes() {
entry:
  %a = alloca ptr, align 8
  %b = alloca ptr, align 8
  store ptr @merge_a, ptr %a, align 8
  store ptr @merge_b, ptr %b, align 8
  %abits = load i64, ptr %a, align 8
  %bbits = load i64, ptr %b, align 8
  %same = icmp eq i64 %abits, %bbits
  ret i1 %same
}
; MERGED: M4 status UNKNOWN
; MERGED: reason=POINTER_PROVENANCE_LOST

define i64 @partially_overwritten_pointer_bytes() {
entry:
  %slot = alloca ptr, align 8
  store ptr @target, ptr %slot, align 8
  store i8 0, ptr %slot, align 1
  %bits = load i64, ptr %slot, align 8
  ret i64 %bits
}
; PARTIAL: M4 status UNKNOWN
; PARTIAL: reason=POINTER_PROVENANCE_LOST

define i64 @overwritten_pointer_bytes() {
entry:
  %slot = alloca ptr, align 8
  store ptr @target, ptr %slot, align 8
  store i64 5, ptr %slot, align 8
  %bits = load i64, ptr %slot, align 8
  ret i64 %bits
}
; OVERWRITE: M4 status M4_EXECUTOR_COMPLETE
; OVERWRITE: return=bv64:5

define i8 @overwritten_single_byte() {
entry:
  %slot = alloca ptr, align 8
  store ptr @target, ptr %slot, align 8
  store i8 5, ptr %slot, align 1
  %byte = load i8, ptr %slot, align 1
  ret i8 %byte
}
; BYTE: M4 status M4_EXECUTOR_COMPLETE
; BYTE: return=bv8:5

define i64 @copied_pointer_bytes() {
entry:
  %source = alloca ptr, align 8
  %dest = alloca ptr, align 8
  store ptr @target, ptr %source, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %dest, ptr align 8 %source, i64 8, i1 false)
  %bits = load i64, ptr %dest, align 8
  ret i64 %bits
}
; COPY: M4 status UNKNOWN
; COPY: reason=POINTER_PROVENANCE_LOST

define i64 @pointer_initializer_bytes() {
entry:
  %bits = load i64, ptr @pointer_initializer, align 8
  ret i64 %bits
}
; INITIALIZER: M4 status UNKNOWN
; INITIALIZER: reason=POINTER_PROVENANCE_LOST

define i64 @null_pointer_bytes() {
entry:
  %bits = load i64, ptr @null_pointer_initializer, align 8
  ret i64 %bits
}
; NULL: M4 status M4_EXECUTOR_COMPLETE
; NULL: return=bv64:0
