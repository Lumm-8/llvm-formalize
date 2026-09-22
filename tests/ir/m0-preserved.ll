; RUN: rm -rf %t.dir && %opt -S %s -o %t.before.ll
; RUN: %opt -S -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -o %t.after.ll
; RUN: diff %t.before.ll %t.after.ll

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
entry:
  ret void
}
