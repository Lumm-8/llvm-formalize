; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry allocation_failure --allocation-failures | %FileCheck %s --check-prefix=FAIL-ON
; RUN: %scalar-executor %s --m4 --entry allocation_failure | %FileCheck %s --check-prefix=FAIL-OFF
; RUN: %scalar-executor %s --m4 --entry malloc_zero | %FileCheck %s --check-prefix=ZERO
; RUN: %scalar-executor %s --m4 --entry realloc_null | %FileCheck %s --check-prefix=REALLOC-NULL
; RUN: %scalar-executor %s --m4 --entry realloc_shrink | %FileCheck %s --check-prefix=SHRINK
; RUN: %scalar-executor %s --m4 --entry realloc_failure --allocation-failures | %FileCheck %s --check-prefix=REALLOC-FAIL
; RUN: %scalar-executor %s --m4 --entry realloc_zero | %FileCheck %s --check-prefix=REALLOC-ZERO
; RUN: %scalar-executor %s --m4 --entry interior_free | %FileCheck %s --check-prefix=INTERIOR
; RUN: %scalar-executor %s --m4 --entry realloc_candidates | %FileCheck %s --check-prefix=CANDIDATES
; RUN: %scalar-executor %s --m4 --entry realloc_mixed_candidates | %FileCheck %s --check-prefix=MIXED
; RUN: %scalar-executor %s --m4 --entry calloc_product_overflow | %FileCheck %s --check-prefix=CALLOC-OVERFLOW
; RUN: %scalar-executor %s --m4 --entry realloc_pointer_shadow_grow | %FileCheck %s --check-prefix=SHADOW-GROW
; RUN: %scalar-executor %s --m4 --entry realloc_pointer_shadow_shrink | %FileCheck %s --check-prefix=SHADOW-SHRINK

target triple = "x86_64-unknown-linux-gnu"
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

declare ptr @malloc(i64)
declare ptr @calloc(i64, i64)
declare ptr @realloc(ptr, i64)
declare void @free(ptr)

define i32 @allocation_failure() {
  %p = call ptr @malloc(i64 1)
  %null = icmp eq ptr %p, null
  br i1 %null, label %failed, label %ok
failed:
  ret i32 2
ok:
  call void @free(ptr %p)
  ret i32 1
}
; FAIL-ON: M4 status M4_EXECUTOR_COMPLETE
; FAIL-ON-DAG: return=bv32:1
; FAIL-ON-DAG: return=bv32:2
; FAIL-OFF: M4 status M4_EXECUTOR_COMPLETE
; FAIL-OFF: M4 paths 1
; FAIL-OFF: return=bv32:1

define i32 @malloc_zero() {
  %p = call ptr @malloc(i64 0)
  %null = icmp eq ptr %p, null
  br i1 %null, label %failed, label %ok
failed:
  ret i32 2
ok:
  call void @free(ptr %p)
  ret i32 1
}
; ZERO: M4 status M4_EXECUTOR_COMPLETE
; ZERO: M4 paths 1
; ZERO: return=bv32:1

define i32 @realloc_null() {
  %p = call ptr @realloc(ptr null, i64 4)
  store i32 5, ptr %p, align 4
  %v = load i32, ptr %p, align 4
  call void @free(ptr %p)
  ret i32 %v
}
; REALLOC-NULL: M4 status M4_EXECUTOR_COMPLETE
; REALLOC-NULL: return=bv32:5

define i32 @realloc_shrink() {
  %p = call ptr @malloc(i64 8)
  store i32 5, ptr %p, align 4
  %q = call ptr @realloc(ptr %p, i64 4)
  %v = load i32, ptr %q, align 4
  call void @free(ptr %q)
  ret i32 %v
}
; SHRINK: M4 status M4_EXECUTOR_COMPLETE
; SHRINK: return=bv32:5

define i32 @realloc_failure() {
  %p = call ptr @malloc(i64 4)
  %malloc_null = icmp eq ptr %p, null
  br i1 %malloc_null, label %malloc_failed, label %allocated
malloc_failed:
  ret i32 0
allocated:
  store i32 9, ptr %p, align 4
  %q = call ptr @realloc(ptr %p, i64 8)
  %null = icmp eq ptr %q, null
  br i1 %null, label %failed, label %ok
failed:
  %old = load i32, ptr %p, align 4
  call void @free(ptr %p)
  ret i32 %old
ok:
  %new = load i32, ptr %q, align 4
  call void @free(ptr %q)
  ret i32 %new
}
; REALLOC-FAIL: M4 status M4_EXECUTOR_COMPLETE
; REALLOC-FAIL: M4 paths 3
; REALLOC-FAIL-DAG: return=bv32:9
; REALLOC-FAIL-DAG: return=bv32:0

define void @realloc_zero() {
  %p = call ptr @malloc(i64 4)
  %q = call ptr @realloc(ptr %p, i64 0)
  ret void
}
; REALLOC-ZERO: M4 status UNKNOWN
; REALLOC-ZERO: reason=SYMBOLIC_ALLOCATION_SIZE

define void @interior_free() {
  %p = call ptr @malloc(i64 4)
  %q = getelementptr i8, ptr %p, i64 1
  call void @free(ptr %q)
  ret void
}
; INTERIOR: M4 status ERROR
; INTERIOR: reason=INVALID_FREE

define i32 @realloc_candidates(i1 %choose_first) {
  %p = call ptr @malloc(i64 4)
  %q = call ptr @malloc(i64 4)
  store i32 11, ptr %p, align 4
  store i32 22, ptr %q, align 4
  %old = select i1 %choose_first, ptr %p, ptr %q
  %new = call ptr @realloc(ptr %old, i64 8)
  %value = load i32, ptr %new, align 4
  call void @free(ptr %new)
  ret i32 %value
}
; CANDIDATES: M4 status M4_EXECUTOR_COMPLETE
; CANDIDATES: M4 paths 2
; CANDIDATES-DAG: return=bv32:11
; CANDIDATES-DAG: return=bv32:22

define void @realloc_mixed_candidates(i1 %choose_heap) {
  %p = call ptr @malloc(i64 4)
  %stack = alloca i32
  %old = select i1 %choose_heap, ptr %p, ptr %stack
  %new = call ptr @realloc(ptr %old, i64 8)
  call void @free(ptr %new)
  ret void
}
; MIXED: M4 status ERROR
; MIXED: M4 paths 2
; MIXED: reason=INVALID_FREE

define void @realloc_event_trace() {
  %old = call ptr @malloc(i64 8)
  store i32 19, ptr %old, align 4
  %new = call ptr @realloc(ptr %old, i64 4)
  %v = load i32, ptr %new, align 4
  %ok = icmp eq i32 %v, 19
  br i1 %ok, label %done, label %bad
done:
  call void @free(ptr %new)
  ret void
bad:
  call void @abort()
  unreachable
}

declare void @abort()

define void @malloc_failure_handled() {
  %p = call ptr @malloc(i64 4)
  %is_null = icmp eq ptr %p, null
  br i1 %is_null, label %done, label %release
release:
  call void @free(ptr %p)
  br label %done
done:
  ret void
}

define void @calloc_failure_handled() {
  %p = call ptr @calloc(i64 2, i64 4)
  %is_null = icmp eq ptr %p, null
  br i1 %is_null, label %done, label %release
release:
  call void @free(ptr %p)
  br label %done
done:
  ret void
}

define void @realloc_failure_handled() {
  %p = call ptr @realloc(ptr null, i64 4)
  %is_null = icmp eq ptr %p, null
  br i1 %is_null, label %done, label %release
release:
  call void @free(ptr %p)
  br label %done
done:
  ret void
}

define void @calloc_product_overflow() {
  %p = call ptr @calloc(i64 -1, i64 2)
  ret void
}
; CALLOC-OVERFLOW: M4 status UNKNOWN
; CALLOC-OVERFLOW: reason=SYMBOLIC_ALLOCATION_SIZE

define i32 @realloc_pointer_shadow_grow() {
  %target = call ptr @malloc(i64 4)
  %box = call ptr @malloc(i64 16)
  store ptr %target, ptr %box, align 8
  %copy = call ptr @realloc(ptr %box, i64 32)
  %saved = load ptr, ptr %copy, align 8
  store i32 77, ptr %saved, align 4
  %value = load i32, ptr %target, align 4
  call void @free(ptr %copy)
  call void @free(ptr %target)
  ret i32 %value
}
; SHADOW-GROW: M4 status M4_EXECUTOR_COMPLETE
; SHADOW-GROW: return=bv32:77

define i32 @realloc_pointer_shadow_shrink() {
  %target = call ptr @malloc(i64 4)
  %box = call ptr @malloc(i64 16)
  store ptr %target, ptr %box, align 8
  %copy = call ptr @realloc(ptr %box, i64 8)
  %saved = load ptr, ptr %copy, align 8
  store i32 78, ptr %saved, align 4
  %value = load i32, ptr %target, align 4
  call void @free(ptr %copy)
  call void @free(ptr %target)
  ret i32 %value
}
; SHADOW-SHRINK: M4 status M4_EXECUTOR_COMPLETE
; SHADOW-SHRINK: return=bv32:78
