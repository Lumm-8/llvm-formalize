; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry lifetime_reuse | %FileCheck %s --check-prefix=REUSE
; RUN: %scalar-executor %s --m4 --entry lifetime_end_access | %FileCheck %s --check-prefix=DEAD
; RUN: %scalar-executor %s --m4 --entry lifetime_start_undef | %FileCheck %s --check-prefix=UNINIT
; RUN: %scalar-executor %s --m4 --entry partial_lifetime | %FileCheck %s --check-prefix=PARTIAL
; RUN: %scalar-executor %s --m4 --entry access_before_start | %FileCheck %s --check-prefix=BEFORE
; RUN: %scalar-executor %s --m4 --entry repeated_end | %FileCheck %s --check-prefix=REPEATEND
; RUN: %scalar-executor %s --m4 --entry zero_size_start | %FileCheck %s --check-prefix=ZEROSTART
; RUN: %scalar-executor %s --m4 --entry zero_size_end | %FileCheck %s --check-prefix=ZEROEND
; RUN: %scalar-executor %s --m4 --entry mismatched_start_before_access | %FileCheck %s --check-prefix=MISMATCH
; RUN: %scalar-executor %s --m4 --entry repeated_start_poison | %FileCheck %s --check-prefix=RESTART

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
  call void @llvm.lifetime.start.p0(i64 4, ptr %p)
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; UNINIT: M4 status UNKNOWN
; UNINIT: reason=UNINITIALIZED_READ

define void @partial_lifetime() {
  %p = alloca i32, align 4
  %interior = getelementptr i8, ptr %p, i64 1
  call void @llvm.lifetime.end.p0(i64 2, ptr %interior)
  ret void
}
; PARTIAL: M4 status UNKNOWN
; PARTIAL: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION

define void @access_before_start() {
  %p = alloca i8, align 1
  store i8 3, ptr %p, align 1
  call void @llvm.lifetime.start.p0(i64 1, ptr %p)
  ret void
}
; BEFORE: M4 status ERROR
; BEFORE: reason=DEAD_OBJECT_ACCESS

define void @repeated_end() {
  %p = alloca i8, align 1
  call void @llvm.lifetime.start.p0(i64 1, ptr %p)
  call void @llvm.lifetime.end.p0(i64 1, ptr %p)
  call void @llvm.lifetime.end.p0(i64 1, ptr %p)
  ret void
}
; REPEATEND: M4 status M4_EXECUTOR_COMPLETE

define i8 @zero_size_start() {
  %p = alloca i8, align 1
  call void @llvm.lifetime.start.p0(i64 0, ptr %p)
  store i8 7, ptr %p, align 1
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; ZEROSTART: M4 status UNKNOWN
; ZEROSTART: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION

define i8 @zero_size_end() {
  %p = alloca i8, align 1
  store i8 7, ptr %p, align 1
  call void @llvm.lifetime.end.p0(i64 0, ptr %p)
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; ZEROEND: M4 status UNKNOWN
; ZEROEND: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION

define void @mismatched_start_before_access() {
  %p = alloca i8, align 1
  store i8 7, ptr %p, align 1
  call void @llvm.lifetime.start.p0(i64 0, ptr %p)
  ret void
}
; MISMATCH: M4 status UNKNOWN
; MISMATCH: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION

define i8 @repeated_start_poison() {
  %p = alloca i8, align 1
  call void @llvm.lifetime.start.p0(i64 1, ptr %p)
  store i8 7, ptr %p, align 1
  call void @llvm.lifetime.start.p0(i64 1, ptr %p)
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; RESTART: M4 status UNKNOWN
; RESTART: reason=UNINITIALIZED_READ
