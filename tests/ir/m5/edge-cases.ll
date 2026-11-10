; RUN: %not %scalar-executor %s --m5 --entry assume_i8 --output-dir %t.assume8 | %FileCheck %s --check-prefix=ASSUME8
; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=assume_i8 -formal-output-dir=%t.preflight-assume8 %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ASSUME8-PREFLIGHT
; RUN: %not %scalar-executor %s --m5 --entry null_zero --output-dir %t.nullzero | %FileCheck %s --check-prefix=NULLZERO
; RUN: %not %scalar-executor %s --m5 --entry null_nonzero --output-dir %t.nullnonzero | %FileCheck %s --check-prefix=NULLNONZERO
; RUN: %not %scalar-executor %s --m5 --entry zero_size --output-dir %t.zero | %FileCheck %s --check-prefix=ZERO
; RUN: %not %scalar-executor %s --m5 --entry out_of_bounds --output-dir %t.bounds | %FileCheck %s --check-prefix=BOUNDS
; RUN: %not %scalar-executor %s --m5 --entry size_overflow --output-dir %t.overflow | %FileCheck %s --check-prefix=OVERFLOW
; RUN: %not %scalar-executor %s --m5 --entry same_input_region --output-dir %t.same | %FileCheck %s --check-prefix=SAME
; RUN: %not %scalar-executor %s --m5 --entry exact_duplicate_input --output-dir %t.exact | %FileCheck %s --check-prefix=EXACT
; RUN: %scalar-executor %s --m5 --entry consistent_paths --output-dir %t.consistent | %FileCheck %s --check-prefix=CONSISTENT
; RUN: %not %scalar-executor %s --m5 --entry partial_path --output-dir %t.partial | %FileCheck %s --check-prefix=PARTIAL
; RUN: %not %scalar-executor %s --m5 --entry error_and_unknown --output-dir %t.error-unknown | %FileCheck %s --check-prefix=ERROR-UNKNOWN

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
@name_z = private constant [2 x i8] c"z\00"

declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @__formal_assume(i8)
declare void @unknown_call()

define void @assume_i8() {
entry:
  call void @__formal_assume(i8 2)
  ret void
}
; ASSUME8: M5 status UNKNOWN
; ASSUME8: reason=UNKNOWN-REGISTRATION-UNSUPPORTED
; ASSUME8-PREFLIGHT: INVALID_TASK [INVALID_REGISTRATION]

define void @null_zero() {
entry:
  call void @__formal_register_input(ptr null, i64 0, ptr @name_x)
  ret void
}
; NULLZERO: M5 status UNKNOWN
; NULLZERO: reason=UNKNOWN-REGISTRATION-UNSUPPORTED

define void @null_nonzero() {
entry:
  call void @__formal_register_input(ptr null, i64 1, ptr @name_x)
  ret void
}
; NULLNONZERO: M5 status UNKNOWN
; NULLNONZERO: reason=UNKNOWN-REGISTRATION-UNSUPPORTED

define void @zero_size() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 0, ptr @name_x)
  ret void
}
; ZERO: M5 status UNKNOWN
; ZERO: reason=UNKNOWN-REGISTRATION-UNSUPPORTED

define void @out_of_bounds() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 2, ptr @name_x)
  ret void
}
; BOUNDS: M5 status UNKNOWN
; BOUNDS: reason=UNKNOWN-REGISTRATION-UNSUPPORTED

define void @size_overflow() {
entry:
  %x = alloca i8, align 1
  %before = getelementptr i8, ptr %x, i64 -1
  call void @__formal_register_input(ptr %before, i64 -1, ptr @name_x)
  ret void
}
; OVERFLOW: M5 status UNKNOWN
; OVERFLOW: reason=UNKNOWN-REGISTRATION-UNSUPPORTED

define void @same_input_region() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_z)
  ret void
}
; SAME: M5 status UNKNOWN
; SAME: reason=UNKNOWN-REGISTRATION-OVERLAP

define void @exact_duplicate_input() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  ret void
}
; EXACT: M5 status UNKNOWN
; EXACT: reason=UNKNOWN-REGISTRATION-NAME

define void @consistent_paths() {
entry:
  %x = alloca i8, align 1
  %left = alloca i8, align 1
  %right = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  %v = load i8, ptr %x, align 1
  %condition = icmp eq i8 %v, 0
  br i1 %condition, label %yes, label %no
yes:
  store i8 1, ptr %left, align 1
  call void @__formal_register_output(ptr %left, i64 1, ptr @name_y)
  ret void
no:
  store i8 2, ptr %right, align 1
  call void @__formal_register_output(ptr %right, i64 1, ptr @name_y)
  ret void
}
; CONSISTENT: M5 status SAFE

define void @partial_path() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  %v = load i8, ptr %x, align 1
  %condition = icmp eq i8 %v, 0
  br i1 %condition, label %complete, label %incomplete
complete:
  store i8 7, ptr %y, align 1
  ret void
incomplete:
  call void @unknown_call()
  ret void
}
; PARTIAL: M5 status UNKNOWN
; PARTIAL: reason=UNKNOWN-UNSUPPORTED-CALL

define void @error_and_unknown() {
entry:
  %x = alloca [1 x i8], align 1
  %first = getelementptr [1 x i8], ptr %x, i64 0, i64 0
  call void @__formal_register_input(ptr %first, i64 1, ptr @name_x)
  %v = load i8, ptr %first, align 1
  %condition = icmp eq i8 %v, 0
  br i1 %condition, label %bad, label %unknown
bad:
  %past = getelementptr [1 x i8], ptr %x, i64 0, i64 1
  store i8 7, ptr %past, align 1
  ret void
unknown:
  call void @unknown_call()
  ret void
}
; ERROR-UNKNOWN: M5 status UNSAFE
; ERROR-UNKNOWN-DAG: reason=OUT_OF_BOUNDS
; ERROR-UNKNOWN-DAG: reason=UNKNOWN-UNSUPPORTED-CALL
