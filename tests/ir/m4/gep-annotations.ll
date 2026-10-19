; RUN: %scalar-executor %s --m4 --entry inrange_derived_write | %FileCheck %s --check-prefix=INRANGE
; RUN: %scalar-executor %s --m4 --entry zero_inbounds_null | %FileCheck %s --check-prefix=ZERO
; RUN: %scalar-executor %s --m4 --entry nonzero_inbounds_null | %FileCheck %s --check-prefix=NONZERO

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@array = global [2 x i8] zeroinitializer

define void @inrange_derived_write() {
entry:
  %outside = getelementptr i8, ptr getelementptr inrange(0, 1) ([2 x i8], ptr @array, i64 0, i64 0), i64 1
  store i8 9, ptr %outside, align 1
  ret void
}
; INRANGE: M4 status UNKNOWN
; INRANGE: reason=UNKNOWN-INBOUNDS-GEP

define i1 @zero_inbounds_null() {
entry:
  %same = getelementptr inbounds i8, ptr null, i64 0
  %result = icmp eq ptr %same, null
  ret i1 %result
}
; ZERO: M4 status M4_EXECUTOR_COMPLETE
; ZERO: return=bv1:1

define i1 @nonzero_inbounds_null() {
entry:
  %outside = getelementptr inbounds i8, ptr null, i64 1
  %result = icmp eq ptr %outside, null
  ret i1 %result
}
; NONZERO: M4 status UNKNOWN
; NONZERO: reason=UNKNOWN-INBOUNDS-GEP
