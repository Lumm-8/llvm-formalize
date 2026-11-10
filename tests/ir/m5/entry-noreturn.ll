; RUN: %opt -passes=verify -disable-output %s
; RUN: %not %scalar-executor %s --m5 --entry entry_noreturn --output-dir %t.raw | %FileCheck %s --check-prefix=UNKNOWN
; RUN: %FileCheck %s --check-prefix=NO-NORMAL < %t.raw/relation.smt2
; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=entry_noreturn -formal-output-dir=%t.preflight %s -disable-output 2>&1 | %FileCheck %s --check-prefix=PREFLIGHT
; UNKNOWN: M5 status UNKNOWN
; UNKNOWN: reason=UNKNOWN-ENTRY-ATTRIBUTE-CONFLICT
; NO-NORMAL: (define-fun |formal_relation| () Bool false)
; PREFLIGHT: INVALID_TASK [ENTRY_INVALID_ATTRIBUTES]

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@name_y = private constant [2 x i8] c"y\00"
declare void @__formal_register_output(ptr, i64, ptr)

define void @entry_noreturn() #0 {
entry:
  %y = alloca i8, align 1
  store i8 7, ptr %y, align 1
  call void @__formal_register_output(ptr %y, i64 1, ptr @name_y)
  ret void
}

attributes #0 = { noreturn }
