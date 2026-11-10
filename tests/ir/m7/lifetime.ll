; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry lifetime_reuse | %FileCheck %s --check-prefix=REUSE
; RUN: %scalar-executor %s --m4 --entry lifetime_end_access | %FileCheck %s --check-prefix=DEAD
; RUN: %scalar-executor %s --m4 --entry lifetime_start_undef | %FileCheck %s --check-prefix=UNINIT
; RUN: %scalar-executor %s --m4 --entry partial_lifetime | %FileCheck %s --check-prefix=PARTIAL

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture)

define i32 @lifetime_reuse() {
  %p = alloca i32, align 4
  call void @llvm.lifetime.start.p0(i64 4, ptr %p)
  store i32 1, ptr %p, align 4
  call void @llvm.lifetime.end.p0(i64 4, ptr %p)
  call void @llvm.lifetime.start.p0(i64 4, ptr %p)
  store i32 3, ptr %p, align 4
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; REUSE: M4 status M4_EXECUTOR_COMPLETE
; REUSE: return=bv32:3

define i32 @lifetime_end_access() {
  %p = alloca i32, align 4
  store i32 1, ptr %p, align 4
  call void @llvm.lifetime.end.p0(i64 4, ptr %p)
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; DEAD: M4 status ERROR
; DEAD: reason=DEAD_OBJECT_ACCESS

define i32 @lifetime_start_undef() {
  %p = alloca i32, align 4
  store i32 1, ptr %p, align 4
  call void @llvm.lifetime.start.p0(i64 4, ptr %p)
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; UNINIT: M4 status UNKNOWN
; UNINIT: reason=UNINITIALIZED_READ

define void @partial_lifetime() {
  %p = alloca i32, align 4
  call void @llvm.lifetime.end.p0(i64 2, ptr %p)
  ret void
}
; PARTIAL: M4 status UNKNOWN
; PARTIAL: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION
