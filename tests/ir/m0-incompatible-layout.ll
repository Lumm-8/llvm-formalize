; RUN: rm -rf %t.dir && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ERR
; RUN: %FileCheck %s --check-prefix=JSON < %t.dir/runs/run-*/result.json
; ERR: INVALID_TASK [INCOMPATIBLE_DATA_LAYOUT]
; JSON: "reason": "INCOMPATIBLE_DATA_LAYOUT"

target datalayout = "E-p:32:32"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  ret void
}
