; RUN: %opt -passes=verify -disable-output %s
; RUN: %not %scalar-executor %s --m5 --entry input_decl_memory_none --output-dir %t.input | %FileCheck %s --check-prefix=UNKNOWN
; RUN: %FileCheck %s --check-prefix=NO-NORMAL < %t.input/relation.smt2
; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=input_decl_memory_none -formal-output-dir=%t.preflight %s -disable-output 2>&1 | %FileCheck %s --check-prefix=PREFLIGHT
; UNKNOWN: M5 status UNKNOWN
; UNKNOWN: reason=UNKNOWN-REGISTRATION-UNSUPPORTED
; NO-NORMAL: (define-fun |formal_relation| () Bool false)
; PREFLIGHT: INVALID_TASK [INVALID_REGISTRATION]

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@name_x = private constant [2 x i8] c"x\00"
declare void @__formal_register_input(ptr, i64, ptr) #0

define void @input_decl_memory_none() {
entry:
  %x = alloca i8, align 1
  store i8 7, ptr %x, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x)
  %value = load i8, ptr %x, align 1
  ret void
}

attributes #0 = { memory(none) }
