; RUN: rm -rf %t.dir && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ERR
; RUN: %FileCheck %s --check-prefix=JSON < %t.dir/runs/run-*/result.json

; ERR: INVALID_TASK [INCOMPATIBLE_PRODUCER]
; JSON: "producer": "clang version 20.1.8; clang version 20.0.0git"
; JSON: "reason": "INCOMPATIBLE_PRODUCER"

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  ret void
}

!llvm.ident = !{!0, !1}
!0 = !{!"clang version 20.1.8"}
!1 = !{!"clang version 20.0.0git"}
