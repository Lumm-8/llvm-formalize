; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry nested_continuation | %FileCheck %s --check-prefix=NESTED
; RUN: %scalar-executor %s --m4 --entry pointer_return | %FileCheck %s --check-prefix=POINTER
; RUN: %scalar-executor %s --m4 --entry noundef_argument | %FileCheck %s --check-prefix=NOUNDEF
; RUN: %scalar-executor %s --m4 --entry indirect_call | %FileCheck %s --check-prefix=INDIRECT
; RUN: %scalar-executor %s --m4 --entry returned_attribute | %FileCheck %s --check-prefix=RETURNED
; RUN: %scalar-executor %s --m4 --entry dereferenceable_or_null_attribute | %FileCheck %s --check-prefix=DEREF-NULL
; RUN: %scalar-executor %s --m4 --entry readnone_attribute | %FileCheck %s --check-prefix=READNONE
; RUN: %scalar-executor %s --m4 --entry noundef_undef_aggregate_argument | %FileCheck %s --check-prefix=NOUNDEF-AGG-ARG
; RUN: %scalar-executor %s --m4 --entry noundef_undef_aggregate_return | %FileCheck %s --check-prefix=NOUNDEF-AGG-RET
; RUN: %scalar-executor %s --m4 --entry noundef_aggregate_padding | %FileCheck %s --check-prefix=NOUNDEF-AGG-PADDING
; RUN: %scalar-executor %s --m4 --entry noundef_partial_aggregate | %FileCheck %s --check-prefix=NOUNDEF-AGG-PARTIAL
; RUN: %scalar-executor %s --m4 --entry noundef_callsite_return | %FileCheck %s --check-prefix=NOUNDEF-CALLSITE-RET

target triple = "x86_64-unknown-linux-gnu"
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

define void @third(ptr %out) {
  store i32 7, ptr %out, align 4
  ret void
}
define void @second(ptr %out) {
  call void @third(ptr %out)
  %v = load i32, ptr %out, align 4
  %next = add i32 %v, 1
  store i32 %next, ptr %out, align 4
  ret void
}
define i32 @first(ptr %out) {
  call void @second(ptr %out)
  %v = load i32, ptr %out, align 4
  ret i32 %v
}
define i32 @nested_continuation() {
  %out = alloca i32, align 4
  %v = call i32 @first(ptr %out)
  %w = add i32 %v, 1
  ret i32 %w
}
; NESTED: M4 status M4_EXECUTOR_COMPLETE
; NESTED: return=bv32:9

define ptr @identity_pointer(ptr %p) {
  ret ptr %p
}
define i32 @pointer_return() {
  %p = alloca i32, align 4
  store i32 23, ptr %p, align 4
  %returned = call ptr @identity_pointer(ptr %p)
  %v = load i32, ptr %returned, align 4
  ret i32 %v
}
; POINTER: M4 status M4_EXECUTOR_COMPLETE
; POINTER: return=bv32:23

define i32 @take_noundef(i32 noundef %x) {
  ret i32 %x
}
define i32 @noundef_argument() {
  %v = call i32 @take_noundef(i32 noundef 13)
  ret i32 %v
}
; NOUNDEF: M4 status M4_EXECUTOR_COMPLETE
; NOUNDEF: return=bv32:13

@indirect_target = global ptr null
define i32 @indirect_call() {
  %target = load ptr, ptr @indirect_target
  %v = call i32 %target()
  ret i32 %v
}
; INDIRECT: M4 status UNKNOWN
; INDIRECT: reason=UNKNOWN-UNSUPPORTED-CALL

define ptr @with_returned(ptr returned %p) {
  ret ptr %p
}
define i32 @returned_attribute() {
  %p = alloca i32, align 4
  %q = call ptr @with_returned(ptr returned %p)
  ret i32 0
}
; RETURNED: M4 status UNKNOWN
; RETURNED: reason=UNKNOWN-POISON-ANNOTATION

define i32 @with_deref_or_null(ptr dereferenceable_or_null(4) %p) {
  ret i32 0
}
define i32 @dereferenceable_or_null_attribute() {
  %v = call i32 @with_deref_or_null(ptr dereferenceable_or_null(4) null)
  ret i32 %v
}
; DEREF-NULL: M4 status UNKNOWN
; DEREF-NULL: reason=UNKNOWN-POISON-ANNOTATION

define i32 @declared_readnone() memory(none) {
  ret i32 1
}
define i32 @readnone_attribute() {
  %v = call i32 @declared_readnone() memory(none)
  ret i32 %v
}
; READNONE: M4 status UNKNOWN
; READNONE: reason=UNKNOWN-POISON-ANNOTATION

%Aggregate = type { i8, i32 }
define void @ignore_aggregate(%Aggregate noundef %value) {
  ret void
}
define void @noundef_undef_aggregate_argument() {
  call void @ignore_aggregate(%Aggregate noundef undef)
  ret void
}
; NOUNDEF-AGG-ARG: M4 status UNKNOWN
; NOUNDEF-AGG-ARG: reason=UNKNOWN-POISON-ANNOTATION

define noundef %Aggregate @undefined_aggregate_result() {
  ret %Aggregate undef
}
define void @noundef_undef_aggregate_return() {
  %value = call noundef %Aggregate @undefined_aggregate_result()
  ret void
}
; NOUNDEF-AGG-RET: M4 status UNKNOWN
; NOUNDEF-AGG-RET: reason=UNKNOWN-POISON-ANNOTATION

define void @noundef_aggregate_padding() {
  call void @ignore_aggregate(%Aggregate noundef { i8 1, i32 2 })
  ret void
}
; NOUNDEF-AGG-PADDING: M4 status M4_EXECUTOR_COMPLETE

define void @noundef_partial_aggregate() {
  %value = insertvalue %Aggregate undef, i8 1, 0
  call void @ignore_aggregate(%Aggregate noundef %value)
  ret void
}
; NOUNDEF-AGG-PARTIAL: M4 status UNKNOWN
; NOUNDEF-AGG-PARTIAL: reason=UNKNOWN-POISON-ANNOTATION

define %Aggregate @undefined_unannotated_result() {
  ret %Aggregate undef
}
define void @noundef_callsite_return() {
  %value = call noundef %Aggregate @undefined_unannotated_result()
  ret void
}
; NOUNDEF-CALLSITE-RET: M4 status UNKNOWN
; NOUNDEF-CALLSITE-RET: reason=UNKNOWN-POISON-ANNOTATION
