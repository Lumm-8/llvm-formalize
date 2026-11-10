; RUN: %scalar-executor %s --m5 --entry assumed --output-dir %t.assumed | %FileCheck %s --check-prefix=ASSUMED
; RUN: %scalar-executor %s --m5 --entry empty_domain --output-dir %t.empty | %FileCheck %s --check-prefix=EMPTY
; RUN: %not %scalar-executor %s --m5 --entry varying --output-dir %t.varying | %FileCheck %s --check-prefix=VARYING
; RUN: %scalar-executor %s --m5 --entry global_output --output-dir %t.global | %FileCheck %s --check-prefix=GLOBAL
; RUN: %not %scalar-executor %s --m5 --entry pointer_input --output-dir %t.pointer | %FileCheck %s --check-prefix=POINTER
; RUN: %not %scalar-executor %s --m5 --entry heap_input --output-dir %t.heap | %FileCheck %s --check-prefix=HEAP
; RUN: %not %scalar-executor %s --m5 --entry dead_output --output-dir %t.dead | %FileCheck %s --check-prefix=DEAD

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
@name_z = private constant [2 x i8] c"z\00"
@g_output = global i8 0
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @__formal_assume(i32)
declare ptr @malloc(i64)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture)

define void @assumed() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  %v = load i8, ptr %x, align 1
  %in = icmp ult i8 %v, 4
  %cond = zext i1 %in to i32
  call void @__formal_assume(i32 %cond)
  store i8 %v, ptr %y, align 1
  ret void
}
; ASSUMED: M5 status SAFE

define void @empty_domain() {
entry:
  call void @__formal_assume(i32 0)
  ret void
}
; EMPTY: M5 status SAFE
; EMPTY: M5 paths 0

define void @varying() {
entry:
  %x = alloca i8, align 1
  %y = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  %v = load i8, ptr %x, align 1
  %cond = icmp eq i8 %v, 0
  br i1 %cond, label %left, label %right
left:
  store i8 1, ptr %y, align 1
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  ret void
right:
  store i8 2, ptr %y, align 1
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_z)
  ret void
}
; VARYING: M5 status UNKNOWN
; VARYING: reason=UNKNOWN-REGISTRATION-DESCRIPTOR-MISMATCH

define void @global_output() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr @g_output, i64 1, ptr @name_y)
  %v = load i8, ptr %x, align 1
  store i8 %v, ptr @g_output, align 1
  ret void
}
; GLOBAL: M5 status SAFE

define void @pointer_input() {
entry:
  %slot = alloca ptr, align 8
  call void @__formal_register_input(ptr %slot, i64 8, ptr @name_x)
  ret void
}
; POINTER: M5 status UNKNOWN
; POINTER: reason=UNKNOWN-REGISTRATION-UNSUPPORTED

define void @heap_input() {
entry:
  %slot = call ptr @malloc(i64 1)
  call void @__formal_register_input(ptr %slot, i64 1, ptr @name_x)
  ret void
}
; HEAP: M5 status UNKNOWN
; HEAP: reason=UNKNOWN-UNSUPPORTED-CALL

define void @dead_output() {
entry:
  %y = alloca i8, align 1
  store i8 1, ptr %y, align 1
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  call void @llvm.lifetime.end.p0(i64 1, ptr %y)
  ret void
}
; DEAD: M5 status UNKNOWN
; DEAD: reason=UNKNOWN-DEAD-REGISTERED-OUTPUT
