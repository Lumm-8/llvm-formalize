; RUN: %opt -passes=verify -disable-output %s
; RUN: %not %scalar-executor %s --m5 --entry input_call_noreturn --output-dir %t.input | %FileCheck %s --check-prefix=UNKNOWN
; RUN: %FileCheck %s --check-prefix=NO-NORMAL < %t.input/relation.smt2
; RUN: %not %scalar-executor %s --m5 --entry output_decl_noreturn --output-dir %t.output | %FileCheck %s --check-prefix=UNKNOWN
; RUN: %not %scalar-executor %s --m5 --entry assume_call_noreturn --output-dir %t.assume | %FileCheck %s --check-prefix=UNKNOWN
; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=input_call_noreturn -formal-output-dir=%t.preflight-input %s -disable-output 2>&1 | %FileCheck %s --check-prefix=PREFLIGHT
; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=output_decl_noreturn -formal-output-dir=%t.preflight-output %s -disable-output 2>&1 | %FileCheck %s --check-prefix=PREFLIGHT
; UNKNOWN: M5 status UNKNOWN
; UNKNOWN: reason=UNKNOWN-REGISTRATION-UNSUPPORTED
; NO-NORMAL: (define-fun |formal_relation| () Bool false)
; PREFLIGHT: INVALID_TASK [INVALID_REGISTRATION]

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr) #0
declare void @__formal_assume(i32)

define void @input_call_noreturn() {
entry:
  %x = alloca i8, align 1
  call void @__formal_register_input(ptr %x, i64 1, ptr @name_x) #0
  ret void
}

define void @output_decl_noreturn() {
entry:
  %y = alloca i8, align 1
  store i8 7, ptr %y, align 1
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  ret void
}

define void @assume_call_noreturn() {
entry:
  call void @__formal_assume(i32 1) #0
  ret void
}

attributes #0 = { noreturn }
