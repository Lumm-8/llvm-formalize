; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry abort_path | %FileCheck %s --check-prefix=ABORT
; RUN: %scalar-executor %s --m4 --entry assert_path | %FileCheck %s --check-prefix=ASSERT

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
@message = private constant [5 x i8] c"fail\00"
declare void @abort() noreturn
declare void @__assert_fail(ptr, ptr, i32, ptr) noreturn

define void @abort_path() {
  call void @abort()
  unreachable
}
; ABORT: M4 status ERROR
; ABORT: reason=ABORT_CALLED

define void @assert_path() {
  call void @__assert_fail(ptr @message, ptr @message, i32 1, ptr @message)
  unreachable
}
; ASSERT: M4 status ERROR
; ASSERT: reason=ASSERTION_FAILED
