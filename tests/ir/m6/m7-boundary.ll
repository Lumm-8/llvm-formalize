; RUN: %scalar-executor %s --m4 --entry ordinary_direct_call | %FileCheck %s --check-prefix=DIRECT
; RUN: %scalar-executor %s --m4 --entry heap_call | %FileCheck %s --check-prefix=HEAP
; RUN: %scalar-executor %s --m4 --entry recursive_call | %FileCheck %s --check-prefix=RECURSION

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare ptr @malloc(i64)

define i32 @callee() {
  ret i32 7
}

define i32 @ordinary_direct_call() {
  %value = call i32 @callee()
  ret i32 %value
}
; DIRECT: M4 status M4_EXECUTOR_COMPLETE
; DIRECT: return=bv32:7

define void @heap_call() {
  %value = call ptr @malloc(i64 8)
  ret void
}
; HEAP: M4 status UNKNOWN
; HEAP: reason=UNKNOWN-UNSUPPORTED-CALL

define void @recursive_call() {
  call void @recursive_call()
  ret void
}
; RECURSION: M4 status UNKNOWN
; RECURSION: reason=UNKNOWN-CALL-DEPTH
