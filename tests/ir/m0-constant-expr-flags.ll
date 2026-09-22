; RUN: rm -rf %t && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t %s -disable-output 2>&1 | %FileCheck %s

; CHECK: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; CHECK-SAME: global initializer 'constant_gep'

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@bytes = global [8 x i8] zeroinitializer
@constant_gep = global ptr getelementptr inrange(0, 8) ([8 x i8], ptr @bytes, i64 0, i64 1)

define void @formal_top() {
  ret void
}
