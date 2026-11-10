; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry scalar | %FileCheck %s --check-prefix=SCALAR
; RUN: %scalar-executor %s --m4 --entry repeated | %FileCheck %s --check-prefix=REPEATED
; RUN: %scalar-executor %s --m4 --entry pointer_side_effect | %FileCheck %s --check-prefix=SIDE
; RUN: %scalar-executor %s --m4 --entry aggregate_return | %FileCheck %s --check-prefix=AGG
; RUN: %scalar-executor %s --m4 --entry alias_arguments | %FileCheck %s --check-prefix=ALIAS
; RUN: %scalar-executor %s --m4 --entry use_after_return | %FileCheck %s --check-prefix=UAR
; RUN: %scalar-executor %s --m4 --entry unsupported_external | %FileCheck %s --check-prefix=EXTERNAL
; RUN: %scalar-executor %s --m4 --entry unsupported_vararg | %FileCheck %s --check-prefix=VARARG
; RUN: %scalar-executor %s --m4 --entry unsupported_byval | %FileCheck %s --check-prefix=BYVAL
; RUN: %scalar-executor %s --m4 --entry recurse --max-call-depth 3 | %FileCheck %s --check-prefix=DEPTH
; RUN: %scalar-executor %s --m4 --entry byval_copy | %FileCheck %s --check-prefix=BYVAL-COPY
; RUN: %scalar-executor %s --m4 --entry byval_pointer_shadow | %FileCheck %s --check-prefix=BYVAL-SHADOW
; RUN: %scalar-executor %s --m4 --entry valid_pointer_attrs | %FileCheck %s --check-prefix=PTR-ATTR
; RUN: %scalar-executor %s --m4 --entry null_pointer_attr | %FileCheck %s --check-prefix=NULL-ATTR
; RUN: %scalar-executor %s --m4 --entry readonly_rejected | %FileCheck %s --check-prefix=READONLY
; RUN: %scalar-executor %s --m4 --entry unsupported_cc | %FileCheck %s --check-prefix=CC
; RUN: %scalar-executor %s --m4 --entry recursive_base | %FileCheck %s --check-prefix=BASE
; RUN: %scalar-executor %s --m4 --entry readonly_valid | %FileCheck %s --check-prefix=READONLY-OK
; RUN: %scalar-executor %s --m4 --entry sret_alias | %FileCheck %s --check-prefix=SRET-ALIAS
; RUN: %scalar-executor %s --m4 --entry nonnull_return_contract | %FileCheck %s --check-prefix=RET-NONNULL
; RUN: %scalar-executor %s --m4 --entry nocapture_contract | %FileCheck %s --check-prefix=NOCAPTURE
; RUN: %scalar-executor %s --m4 --entry byval_lifetime | %FileCheck %s --check-prefix=BYVAL-LIFETIME
; RUN: %scalar-executor %s --m4 --entry defined_noreturn | %FileCheck %s --check-prefix=DEFINED-NORETURN
; RUN: %scalar-executor %s --m4 --entry noreturn_violation | %FileCheck %s --check-prefix=NORETURN-VIOLATION

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
%Pair = type { i32, i32 }
%PointerBox = type { ptr, i64, i64 }
declare void @llvm.lifetime.start.p0(i64, ptr nocapture)
declare void @llvm.lifetime.end.p0(i64, ptr nocapture)
declare void @abort() noreturn

define i32 @plus_one(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

define i32 @scalar(i32 %x) {
  %y = call i32 @plus_one(i32 %x)
  ret i32 %y
}
; SCALAR: M4 status M4_EXECUTOR_COMPLETE
; SCALAR: return=bv32:symbolic

define i32 @repeated() {
  %a = call i32 @plus_one(i32 5)
  %b = call i32 @plus_one(i32 %a)
  ret i32 %b
}
; REPEATED: M4 status M4_EXECUTOR_COMPLETE
; REPEATED: return=bv32:7

define void @set_value(ptr %p) {
  store i32 42, ptr %p, align 4
  ret void
}

define i32 @pointer_side_effect() {
  %p = alloca i32, align 4
  call void @set_value(ptr %p)
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; SIDE: M4 status M4_EXECUTOR_COMPLETE
; SIDE: return=bv32:42

define %Pair @make_pair() {
  ret %Pair { i32 4, i32 9 }
}

define i32 @aggregate_return() {
  %p = call %Pair @make_pair()
  %v = extractvalue %Pair %p, 1
  ret i32 %v
}
; AGG: M4 status M4_EXECUTOR_COMPLETE
; AGG: return=bv32:9

define i32 @alias_write(ptr %p, ptr %q) {
  store i32 1, ptr %p, align 4
  store i32 2, ptr %q, align 4
  %v = load i32, ptr %p, align 4
  ret i32 %v
}

define i32 @alias_arguments(i1 %same) {
  %p = alloca i32, align 4
  %q = alloca i32, align 4
  %chosen = select i1 %same, ptr %p, ptr %q
  %v = call i32 @alias_write(ptr %p, ptr %chosen)
  ret i32 %v
}
; ALIAS: M4 status M4_EXECUTOR_COMPLETE
; ALIAS-DAG: return=bv32:1
; ALIAS-DAG: return=bv32:2

define ptr @stack_pointer() {
  %p = alloca i32, align 4
  store i32 3, ptr %p, align 4
  ret ptr %p
}

define i32 @use_after_return() {
  %p = call ptr @stack_pointer()
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; UAR: M4 status ERROR
; UAR: reason=DEAD_OBJECT_ACCESS

declare i32 @external(i32)
define i32 @unsupported_external() {
  %v = call i32 @external(i32 1)
  ret i32 %v
}
; EXTERNAL: M4 status UNKNOWN
; EXTERNAL: reason=UNKNOWN-UNSUPPORTED-CALL

define i32 @vararg(i32 %x, ...) {
  ret i32 %x
}
define i32 @unsupported_vararg() {
  %v = call i32 (i32, ...) @vararg(i32 1, i32 2)
  ret i32 %v
}
; VARARG: M4 status UNKNOWN
; VARARG: reason=UNKNOWN-UNSUPPORTED-CALL

define void @take_byval(ptr byval(%Pair) %p) {
  ret void
}
define void @unsupported_byval() {
  %p = alloca %Pair, align 4
  call void @take_byval(ptr byval(%Pair) %p)
  ret void
}
; BYVAL: M4 status M4_EXECUTOR_COMPLETE

define void @recurse() {
  call void @recurse()
  ret void
}
; DEPTH: M4 status UNKNOWN
; DEPTH: reason=UNKNOWN-CALL-DEPTH

define void @mutate_byval(ptr byval(%Pair) %p) {
  store i32 2, ptr %p, align 4
  ret void
}
define i32 @byval_copy() {
  %p = alloca %Pair, align 4
  store i32 1, ptr %p, align 4
  call void @mutate_byval(ptr byval(%Pair) %p)
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
; BYVAL-COPY: M4 status M4_EXECUTOR_COMPLETE
; BYVAL-COPY: return=bv32:1

define void @write_through_byval_pointer(ptr byval(%PointerBox) %box) {
  %field = getelementptr %PointerBox, ptr %box, i32 0, i32 0
  %target = load ptr, ptr %field, align 8
  store i32 23, ptr %target, align 4
  store ptr null, ptr %field, align 8
  ret void
}
define i32 @byval_pointer_shadow() {
  %target = alloca i32, align 4
  %box = alloca %PointerBox, align 8
  %field = getelementptr %PointerBox, ptr %box, i32 0, i32 0
  store ptr %target, ptr %field, align 8
  %first = getelementptr %PointerBox, ptr %box, i32 0, i32 1
  store i64 1, ptr %first, align 8
  %second = getelementptr %PointerBox, ptr %box, i32 0, i32 2
  store i64 2, ptr %second, align 8
  call void @write_through_byval_pointer(ptr byval(%PointerBox) %box)
  %original = load ptr, ptr %field, align 8
  %same = icmp eq ptr %original, %target
  %value = load i32, ptr %target, align 4
  %result = select i1 %same, i32 %value, i32 0
  ret i32 %result
}
; BYVAL-SHADOW: M4 status M4_EXECUTOR_COMPLETE
; BYVAL-SHADOW: return=bv32:23

define void @lifetime_byval(ptr byval(%Pair) %p) {
  call void @llvm.lifetime.end.p0(i64 8, ptr %p)
  ret void
}
define void @byval_lifetime() {
  %p = alloca %Pair, align 4
  call void @lifetime_byval(ptr byval(%Pair) %p)
  ret void
}
; BYVAL-LIFETIME: M4 status M4_EXECUTOR_COMPLETE

define void @always_abort() noreturn {
  call void @abort()
  unreachable
}
define void @defined_noreturn() {
  call void @always_abort()
  ret void
}
; DEFINED-NORETURN: M4 status ERROR
; DEFINED-NORETURN: reason=ABORT_CALLED

define void @returns_anyway() noreturn {
  ret void
}
define void @noreturn_violation() {
  call void @returns_anyway()
  ret void
}
; NORETURN-VIOLATION: M4 status ERROR
; NORETURN-VIOLATION: reason=NORETURN_VIOLATION

define i32 @load_with_attrs(ptr nonnull dereferenceable(4) align 4 %p) {
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
define i32 @valid_pointer_attrs() {
  %p = alloca i32, align 4
  store i32 29, ptr %p, align 4
  %v = call i32 @load_with_attrs(ptr nonnull dereferenceable(4) align 4 %p)
  ret i32 %v
}
; PTR-ATTR: M4 status M4_EXECUTOR_COMPLETE
; PTR-ATTR: return=bv32:29

define i32 @null_pointer_attr() {
  %v = call i32 @load_with_attrs(ptr nonnull dereferenceable(4) null)
  ret i32 %v
}
; NULL-ATTR: M4 status UNKNOWN
; NULL-ATTR: reason=UNKNOWN-POISON-ANNOTATION

define void @writes_readonly(ptr readonly %p) {
  store i32 1, ptr %p, align 4
  ret void
}
define void @readonly_rejected() {
  %p = alloca i32, align 4
  call void @writes_readonly(ptr readonly %p)
  ret void
}
; READONLY: M4 status UNKNOWN
; READONLY: reason=UNKNOWN-POISON-ANNOTATION

define fastcc i32 @fastcallee() {
  ret i32 1
}
define i32 @unsupported_cc() {
  %v = call fastcc i32 @fastcallee()
  ret i32 %v
}
; CC: M4 status UNKNOWN
; CC: reason=UNKNOWN-UNSUPPORTED-CALL

define i32 @recursive_count(i32 %remaining) {
  %done = icmp eq i32 %remaining, 0
  br i1 %done, label %base, label %step
base:
  ret i32 0
step:
  %next = sub i32 %remaining, 1
  %tail = call i32 @recursive_count(i32 %next)
  %result = add i32 %tail, 1
  ret i32 %result
}
define i32 @recursive_base() {
  %v = call i32 @recursive_count(i32 3)
  ret i32 %v
}
; BASE: M4 status M4_EXECUTOR_COMPLETE
; BASE: return=bv32:3

define i32 @read_readonly(ptr readonly %p) {
  %v = load i32, ptr %p, align 4
  ret i32 %v
}
define i32 @readonly_valid() {
  %p = alloca i32, align 4
  store i32 31, ptr %p, align 4
  %v = call i32 @read_readonly(ptr readonly %p)
  ret i32 %v
}
; READONLY-OK: M4 status M4_EXECUTOR_COMPLETE
; READONLY-OK: return=bv32:31

define void @sret_alias_callee(ptr noalias sret(%Pair) %out, ptr %alias) {
  ret void
}
define void @sret_alias() {
  %p = alloca %Pair, align 4
  call void @sret_alias_callee(ptr noalias sret(%Pair) %p, ptr %p)
  ret void
}
; SRET-ALIAS: M4 status UNKNOWN
; SRET-ALIAS: reason=UNKNOWN-POISON-ANNOTATION

define nonnull ptr @invalid_nonnull_result() {
  ret ptr null
}
define i32 @nonnull_return_contract() {
  %p = call nonnull ptr @invalid_nonnull_result()
  %isnull = icmp eq ptr %p, null
  %answer = select i1 %isnull, i32 1, i32 2
  ret i32 %answer
}
; RET-NONNULL: M4 status UNKNOWN
; RET-NONNULL: reason=UNKNOWN-POISON-ANNOTATION

@escaped_pointer = global ptr null
define void @invalid_capture(ptr nocapture %p) {
  store ptr %p, ptr @escaped_pointer
  ret void
}
define i32 @nocapture_contract() {
  %p = alloca i32
  call void @invalid_capture(ptr nocapture %p)
  ret i32 0
}
; NOCAPTURE: M4 status UNKNOWN
; NOCAPTURE: reason=UNKNOWN-POISON-ANNOTATION
