; RUN: rm -rf %t.dir && %not %opt -disable-verify -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ERR
; RUN: %FileCheck %s --check-prefix=JSON < %t.dir/runs/run-*/result.json

; ERR: INVALID_TASK [MALFORMED_IR]
; JSON: "reason": "MALFORMED_IR"

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top(i1 %condition) {
entry:
  br i1 %condition, label %left, label %right
left:
  br label %exit
right:
  br label %exit
exit:
  %value = phi i32 [ 1, %left ]
  ret void
}
