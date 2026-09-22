; RUN: rm -rf %t.reject && %not %opt -load-pass-plugin=%bare-plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.reject %s -disable-output 2>&1 | %FileCheck %s --check-prefix=REJECT
; RUN: %FileCheck %s --check-prefix=REJECT-JSON < %t.reject/runs/run-*/result.json
; RUN: rm -rf %t.manual && %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.manual %s -disable-output 2>&1 | %FileCheck %s --check-prefix=MANUAL
; RUN: %FileCheck %s --check-prefix=MANUAL-JSON < %t.manual/runs/run-*/result.json
; REJECT: INVALID_TASK [UNVERSIONED_PRODUCER]
; REJECT-JSON: "reason": "UNVERSIONED_PRODUCER"
; REJECT-JSON: "status": "INVALID_TASK"
; MANUAL: warning: trusted unversioned/manual IR mode bypassed producer authentication
; MANUAL: warning: module has no compile-unit debug information
; MANUAL-JSON: "producer": ""
; MANUAL-JSON: "status": "VALIDATED"

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  ret void
}
