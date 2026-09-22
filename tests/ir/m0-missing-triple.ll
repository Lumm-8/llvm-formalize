; RUN: rm -rf %t.dir && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ERR
; RUN: %FileCheck %s --check-prefix=JSON < %t.dir/runs/run-*/result.json
; ERR: INVALID_TASK [MISSING_TARGET_TRIPLE]
; JSON: "reason": "MISSING_TARGET_TRIPLE"

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"

define void @formal_top() {
  ret void
}
