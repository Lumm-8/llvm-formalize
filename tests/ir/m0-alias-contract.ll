; RUN: rm -rf %t.weak && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=weak_alias_entry -formal-output-dir=%t.weak %s -disable-output 2>&1 | %FileCheck %s --check-prefix=WEAK
; RUN: rm -rf %t.weak-odr && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=weak_odr_alias_entry -formal-output-dir=%t.weak-odr %s -disable-output 2>&1 | %FileCheck %s --check-prefix=WEAK-ODR
; RUN: rm -rf %t.linkonce && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=linkonce_alias_entry -formal-output-dir=%t.linkonce %s -disable-output 2>&1 | %FileCheck %s --check-prefix=LINKONCE
; RUN: rm -rf %t.linkonce-odr && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=linkonce_odr_alias_entry -formal-output-dir=%t.linkonce-odr %s -disable-output 2>&1 | %FileCheck %s --check-prefix=LINKONCE-ODR

; WEAK: INVALID_TASK [REACHABLE_INEXACT_DEFINITION]
; WEAK-SAME: function alias 'weak_hook'
; WEAK-ODR: INVALID_TASK [REACHABLE_INEXACT_DEFINITION]
; WEAK-ODR-SAME: function alias 'weak_odr_hook'
; LINKONCE: INVALID_TASK [REACHABLE_INEXACT_DEFINITION]
; LINKONCE-SAME: function alias 'linkonce_hook'
; LINKONCE-ODR: INVALID_TASK [REACHABLE_INEXACT_DEFINITION]
; LINKONCE-ODR-SAME: function alias 'linkonce_odr_hook'

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @implementation() {
  ret void
}

@weak_hook = weak alias void (), ptr @implementation
@weak_odr_hook = weak_odr alias void (), ptr @implementation
@linkonce_hook = linkonce alias void (), ptr @implementation
@linkonce_odr_hook = linkonce_odr alias void (), ptr @implementation

define void @weak_alias_entry() {
  call void @weak_hook()
  ret void
}

define void @weak_odr_alias_entry() {
  call void @weak_odr_hook()
  ret void
}

define void @linkonce_alias_entry() {
  call void @linkonce_hook()
  ret void
}

define void @linkonce_odr_alias_entry() {
  call void @linkonce_odr_hook()
  ret void
}
