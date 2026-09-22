; RUN: rm -rf %t.decl && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.decl %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DECL
; RUN: %FileCheck %s --check-prefix=DECL-JSON < %t.decl/runs/run-*/result.json
; RUN: rm -rf %t.weak && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=weak_caller -formal-output-dir=%t.weak %s -disable-output 2>&1 | %FileCheck %s --check-prefix=WEAK
; RUN: rm -rf %t.available && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=available_caller -formal-output-dir=%t.available %s -disable-output 2>&1 | %FileCheck %s --check-prefix=AVAILABLE
; RUN: rm -rf %t.linkonce && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=linkonce_caller -formal-output-dir=%t.linkonce %s -disable-output 2>&1 | %FileCheck %s --check-prefix=LINKONCE
; DECL: UNKNOWN [REACHABLE_DECLARATION]
; DECL-JSON: "reason": "REACHABLE_DECLARATION"
; DECL-JSON: "status": "UNKNOWN"
; DECL-JSON: "verdict": "UNKNOWN"
; WEAK: INVALID_TASK [REACHABLE_INEXACT_DEFINITION]
; AVAILABLE: UNKNOWN [REACHABLE_DECLARATION]
; LINKONCE: INVALID_TASK [REACHABLE_INEXACT_DEFINITION]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare void @external()
define void @formal_top() {
  call void @external()
  ret void
}

define weak void @weak_callee() {
  ret void
}

define void @weak_caller() {
  call void @weak_callee()
  ret void
}

define available_externally void @available_callee() {
  ret void
}

define void @available_caller() {
  call void @available_callee()
  ret void
}

define linkonce_odr void @linkonce_callee() {
  ret void
}

define void @linkonce_caller() {
  call void @linkonce_callee()
  ret void
}
