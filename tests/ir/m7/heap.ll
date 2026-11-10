; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry malloc_success --allocation-failures | %FileCheck %s --check-prefix=MALLOC
; RUN: %scalar-executor %s --m4 --entry calloc_zero --allocation-failures | %FileCheck %s --check-prefix=CALLOC
; RUN: %scalar-executor %s --m4 --entry free_null | %FileCheck %s --check-prefix=NULL
; RUN: %scalar-executor %s --m4 --entry double_free | %FileCheck %s --check-prefix=DOUBLE
; RUN: %scalar-executor %s --m4 --entry use_after_free | %FileCheck %s --check-prefix=UAF
; RUN: %scalar-executor %s --m4 --entry invalid_free | %FileCheck %s --check-prefix=INVALID
; RUN: %scalar-executor %s --m4 --entry symbolic_size | %FileCheck %s --check-prefix=SIZE
; RUN: %scalar-executor %s --m4 --entry realloc_growth --allocation-failures | %FileCheck %s --check-prefix=REALLOC
; RUN: %scalar-executor %s --m4 --entry realloc_old_pointer | %FileCheck %s --check-prefix=REALLOC-UAF
; RUN: %scalar-executor %s --m4 --entry double_delete | %FileCheck %s --check-prefix=DELETE
; RUN: %scalar-executor %s --m4 --entry free_new | %FileCheck %s --check-prefix=FREE-NEW

target triple = "x86_64-unknown-linux-gnu"
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare ptr @malloc(i64)
declare ptr @calloc(i64, i64)
declare ptr @realloc(ptr, i64)
declare void @free(ptr)
declare ptr @_Znwm(i64)
declare void @_ZdlPv(ptr)

define i32 @malloc_success() {
  %p = call ptr @malloc(i64 4)
  %isnull = icmp eq ptr %p, null
  br i1 %isnull, label %failed, label %allocated
allocated:
  store i32 77, ptr %p, align 4
  %v = load i32, ptr %p, align 4
  call void @free(ptr %p)
  ret i32 %v
failed:
  ret i32 0
}
; MALLOC: M4 status M4_EXECUTOR_COMPLETE
; MALLOC-DAG: return=bv32:77
; MALLOC-DAG: return=bv32:0

define i32 @calloc_zero() {
  %p = call ptr @calloc(i64 1, i64 4)
  %isnull = icmp eq ptr %p, null
  br i1 %isnull, label %failed, label %allocated
allocated:
  %v = load i32, ptr %p, align 4
  call void @free(ptr %p)
  ret i32 %v
failed:
  ret i32 9
}
; CALLOC: M4 status M4_EXECUTOR_COMPLETE
; CALLOC-DAG: return=bv32:0
; CALLOC-DAG: return=bv32:9

define void @free_null() {
  call void @free(ptr null)
  ret void
}
; NULL: M4 status M4_EXECUTOR_COMPLETE

define void @double_free() {
  %p = call ptr @malloc(i64 4)
  %isnull = icmp eq ptr %p, null
  br i1 %isnull, label %done, label %allocated
allocated:
  call void @free(ptr %p)
  call void @free(ptr %p)
  ret void
done:
  ret void
}
; DOUBLE: M4 status ERROR
; DOUBLE: reason=DOUBLE_FREE

define i32 @use_after_free() {
  %p = call ptr @malloc(i64 4)
  %isnull = icmp eq ptr %p, null
  br i1 %isnull, label %failed, label %allocated
allocated:
  call void @free(ptr %p)
  %v = load i32, ptr %p, align 4
  ret i32 %v
failed:
  ret i32 0
}
; UAF: M4 status ERROR
; UAF: reason=USE_AFTER_FREE

define void @invalid_free() {
  %p = alloca i32, align 4
  call void @free(ptr %p)
  ret void
}
; INVALID: M4 status ERROR
; INVALID: reason=INVALID_FREE

define void @symbolic_size(i64 %n) {
  %p = call ptr @malloc(i64 %n)
  ret void
}
; SIZE: M4 status UNKNOWN
; SIZE: reason=SYMBOLIC_ALLOCATION_SIZE

define i32 @realloc_growth() {
  %p = call ptr @malloc(i64 4)
  %isnull = icmp eq ptr %p, null
  br i1 %isnull, label %failed, label %allocated
allocated:
  store i32 5, ptr %p, align 4
  %q = call ptr @realloc(ptr %p, i64 8)
  %resize_failed = icmp eq ptr %q, null
  br i1 %resize_failed, label %keep_old, label %resized
resized:
  %v = load i32, ptr %q, align 4
  call void @free(ptr %q)
  ret i32 %v
keep_old:
  %old = load i32, ptr %p, align 4
  call void @free(ptr %p)
  ret i32 %old
failed:
  ret i32 0
}
; REALLOC: M4 status M4_EXECUTOR_COMPLETE
; REALLOC-DAG: return=bv32:5
; REALLOC-DAG: return=bv32:0

define i32 @realloc_old_pointer() {
  %p = call ptr @malloc(i64 4)
  %isnull = icmp eq ptr %p, null
  br i1 %isnull, label %failed, label %allocated
allocated:
  %q = call ptr @realloc(ptr %p, i64 8)
  %resize_failed = icmp eq ptr %q, null
  br i1 %resize_failed, label %failed, label %resized
resized:
  %v = load i32, ptr %p, align 4
  ret i32 %v
failed:
  ret i32 0
}
; REALLOC-UAF: M4 status ERROR
; REALLOC-UAF: reason=USE_AFTER_FREE

define void @double_delete() {
  %p = call ptr @_Znwm(i64 4)
  call void @_ZdlPv(ptr %p)
  call void @_ZdlPv(ptr %p)
  ret void
}
; DELETE: M4 status ERROR
; DELETE: reason=DOUBLE_FREE

define void @free_new() {
  %p = call ptr @_Znwm(i64 4)
  call void @free(ptr %p)
  ret void
}
; FREE-NEW: M4 status ERROR
; FREE-NEW: reason=INVALID_FREE
