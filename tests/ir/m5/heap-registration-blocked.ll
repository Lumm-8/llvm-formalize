; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m5 --entry heap_registration --allocation-failures --output-dir %t.heap | %FileCheck %s --check-prefix=SAFE
; RUN: %FileCheck %s --check-prefix=RELATION < %t.heap/relation.smt2
; RUN: %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=heap_registration -formal-output-dir=%t.preflight %s -disable-output 2>&1 | %FileCheck %s --check-prefix=PREFLIGHT
; SAFE: M5 status SAFE
; RELATION: (define-fun |formal_relation|
; PREFLIGHT: preflight validated

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
declare ptr @malloc(i64)
declare void @__formal_assume(i32)
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @heap_registration() {
entry:
  %allocation = call ptr @malloc(i64 1)
  %alive = icmp ne ptr %allocation, null
  %alive.i32 = zext i1 %alive to i32
  call void @__formal_assume(i32 %alive.i32)
  call void @__formal_register_input(ptr %allocation, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %allocation, i64 1, ptr @name_y)
  ret void
}
