; RUN: rm -rf %t.width && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-integer-bit-width=256 -formal-output-dir=%t.width %s -disable-output 2>&1 | %FileCheck %s --check-prefix=WIDTH
; RUN: rm -rf %t.depth && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-aggregate-depth=2 -formal-output-dir=%t.depth %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DEPTH

; WIDTH: INVALID_TASK [RESOURCE_LIMIT] LLVM integer bit width limit exceeded
; DEPTH: INVALID_TASK [RESOURCE_LIMIT] LLVM aggregate type nesting limit exceeded

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  %wide = alloca i257
  %deep = getelementptr [1 x [1 x [1 x i8]]], ptr null, i64 1
  ret void
}
