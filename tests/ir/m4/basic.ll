; RUN: %scalar-executor %s --m4 --entry stack_roundtrip | %FileCheck %s --check-prefix=STACK
; RUN: %scalar-executor %s --m4 --entry zero_global | %FileCheck %s --check-prefix=ZERO
; RUN: %scalar-executor %s --m4 --entry scalar_global | %FileCheck %s --check-prefix=SCALAR
; RUN: %scalar-executor %s --m4 --entry null_access | %FileCheck %s --check-prefix=NULL
; RUN: %scalar-executor %s --m4 --entry one_past | %FileCheck %s --check-prefix=ONEPAST
; RUN: %scalar-executor %s --m4 --entry one_past_access | %FileCheck %s --check-prefix=ONEPASTBAD
; RUN: %scalar-executor %s --m4 --entry readonly_write | %FileCheck %s --check-prefix=READONLY

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@zero = global i32 0
@scalar = global i24 66051
@readonly = constant i8 42

define i32 @stack_roundtrip() {
entry:
  %slot = alloca i32, align 4
  store i32 123, ptr %slot, align 4
  %value = load i32, ptr %slot, align 4
  ret i32 %value
}
; STACK: M4 status M4_EXECUTOR_COMPLETE
; STACK: return=bv32:123

define i32 @zero_global() {
entry:
  %v = load i32, ptr @zero, align 4
  ret i32 %v
}
; ZERO: return=bv32:0

define i24 @scalar_global() {
entry:
  %v = load i24, ptr @scalar, align 4
  ret i24 %v
}
; SCALAR: return=bv24:66051

define i8 @null_access() {
entry:
  %v = load i8, ptr null, align 1
  ret i8 %v
}
; NULL: M4 status ERROR
; NULL: reason=NULL_DEREFERENCE

define void @one_past() {
entry:
  %slot = alloca i8, align 1
  %past = getelementptr i8, ptr %slot, i64 1
  ret void
}
; ONEPAST: M4 status M4_EXECUTOR_COMPLETE

define i8 @one_past_access() {
entry:
  %slot = alloca i8, align 1
  %past = getelementptr i8, ptr %slot, i64 1
  %v = load i8, ptr %past, align 1
  ret i8 %v
}
; ONEPASTBAD: M4 status ERROR
; ONEPASTBAD: reason=OUT_OF_BOUNDS

define void @readonly_write() {
entry:
  store i8 0, ptr @readonly, align 1
  ret void
}
; READONLY: M4 status ERROR
; READONLY: reason=READ_ONLY_WRITE
