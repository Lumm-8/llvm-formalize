; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t/../escape %s -disable-output 2>&1 | %FileCheck %s
; RUN: %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-type-work=0 -formal-output-dir=%t.zero %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ZERO
; CHECK: INVALID_CONFIGURATION formal output directory must not contain '..' components
; ZERO: INVALID_CONFIGURATION all resource limits must be positive

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  ret void
}
