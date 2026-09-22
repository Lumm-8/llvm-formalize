; RUN: rm -rf %t.triple && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.triple %s -disable-output 2>&1 | %FileCheck %s --check-prefix=TRIPLE
; RUN: %FileCheck %s --check-prefix=JSON < %t.triple/runs/run-*/result.json
; TRIPLE: INVALID_TASK [UNSUPPORTED_TARGET]
; JSON: "reason": "UNSUPPORTED_TARGET"

target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"
target triple = "wasm32-unknown-unknown"

define void @formal_top() {
  ret void
}
