; RUN: rm -rf %t.missing && %not %opt -load-pass-plugin=%plugin -disable-verify -passes=formal-exec -formal-entry=missing -formal-output-dir=%t.missing %s -disable-output 2>&1 | %FileCheck %s --check-prefix=MISSING
; RUN: %FileCheck %s --check-prefix=MISSING-JSON < %t.missing/runs/run-*/result.json
; RUN: rm -rf %t.decl && %not %opt -load-pass-plugin=%plugin -disable-verify -passes=formal-exec -formal-entry=declaration_only -formal-output-dir=%t.decl %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DECL
; RUN: rm -rf %t.sig && %not %opt -load-pass-plugin=%plugin -disable-verify -passes=formal-exec -formal-entry=wrong_signature -formal-output-dir=%t.sig %s -disable-output 2>&1 | %FileCheck %s --check-prefix=SIG
; RUN: rm -rf %t.linkage && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=local_entry -formal-output-dir=%t.linkage %s -disable-output 2>&1 | %FileCheck %s --check-prefix=LINKAGE
; RUN: rm -rf %t.cc && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=fast_entry -formal-output-dir=%t.cc %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CC
; RUN: rm -rf %t.available && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=available_entry -formal-output-dir=%t.available %s -disable-output 2>&1 | %FileCheck %s --check-prefix=AVAILABLE
; RUN: rm -rf %t.weak && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=weak_entry -formal-output-dir=%t.weak %s -disable-output 2>&1 | %FileCheck %s --check-prefix=WEAK
; RUN: rm -rf %t.linkonce && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=linkonce_entry -formal-output-dir=%t.linkonce %s -disable-output 2>&1 | %FileCheck %s --check-prefix=LINKONCE

; MISSING: INVALID_TASK [ENTRY_NOT_FOUND]
; MISSING-JSON: "reason": "ENTRY_NOT_FOUND"
; MISSING-JSON: "status": "INVALID_TASK"
; DECL: INVALID_TASK [ENTRY_DECLARATION_ONLY]
; SIG: INVALID_TASK [ENTRY_INVALID_SIGNATURE]
; LINKAGE: INVALID_TASK [ENTRY_INVALID_LINKAGE]
; CC: INVALID_TASK [ENTRY_INVALID_CALLING_CONVENTION]
; AVAILABLE: INVALID_TASK [ENTRY_DECLARATION_ONLY]
; WEAK: INVALID_TASK [ENTRY_INVALID_LINKAGE]
; LINKONCE: INVALID_TASK [ENTRY_INVALID_LINKAGE]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare void @declaration_only()

define i32 @wrong_signature() {
  ret i32 0
}

define internal void @local_entry() {
  ret void
}

define fastcc void @fast_entry() {
  ret void
}

define available_externally void @available_entry() {
  ret void
}

define weak void @weak_entry() {
  ret void
}

define linkonce void @linkonce_entry() {
  ret void
}
