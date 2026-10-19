; RUN: %scalar-executor %s --m4 --entry global_gep_pointer | %FileCheck %s --check-prefix=GEP
; RUN: %scalar-executor %s --m4 --entry null_pointer_global | %FileCheck %s --check-prefix=NULL
; RUN: %scalar-executor %s --m4 --entry mergeable_global_eq | %FileCheck %s --check-prefix=MERGE
; RUN: %scalar-executor %s --m4 --entry mergeable_global_self_eq | %FileCheck %s --check-prefix=MERGE-SELF
; RUN: %scalar-executor %s --m4 --entry externally_initialized_read | %FileCheck %s --check-prefix=EXTERNAL-INIT
; RUN: %scalar-executor %s --m4 --entry externally_initialized_overwrite | %FileCheck %s --check-prefix=EXTERNAL-WRITE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@word = global i32 16909060
@byteptr = global ptr getelementptr (i8, ptr @word, i64 1)
@nullp = global ptr null
@mergeable_a = unnamed_addr constant i8 7
@mergeable_b = constant i8 7
@external_init = externally_initialized global i8 0

define i8 @global_gep_pointer() {
entry:
  %p = load ptr, ptr @byteptr, align 8
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; GEP: return=bv8:3

define i1 @null_pointer_global() {
entry:
  %p = load ptr, ptr @nullp, align 8
  %isnull = icmp eq ptr %p, null
  ret i1 %isnull
}
; NULL: return=bv1:1

define i1 @mergeable_global_eq() {
entry:
  %same = icmp eq ptr @mergeable_a, @mergeable_b
  ret i1 %same
}
; MERGE: M4 status UNKNOWN
; MERGE: reason=UNKNOWN-POINTER-COMPARISON

define i1 @mergeable_global_self_eq() {
entry:
  %same = icmp eq ptr @mergeable_a, @mergeable_a
  ret i1 %same
}
; MERGE-SELF: M4 status M4_EXECUTOR_COMPLETE
; MERGE-SELF: return=bv1:1

define i8 @externally_initialized_read() {
entry:
  %value = load i8, ptr @external_init, align 1
  ret i8 %value
}
; EXTERNAL-INIT: M4 status UNKNOWN
; EXTERNAL-INIT: reason=UNKNOWN-UNSUPPORTED-GLOBAL

define i8 @externally_initialized_overwrite() {
entry:
  store i8 13, ptr @external_init, align 1
  %value = load i8, ptr @external_init, align 1
  ret i8 %value
}
; EXTERNAL-WRITE: M4 status M4_EXECUTOR_COMPLETE
; EXTERNAL-WRITE: return=bv8:13
