; RUN: rm -rf %t.atomic && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=atomic_entry -formal-output-dir=%t.atomic %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ATOMIC
; RUN: rm -rf %t.indirect && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=indirect_entry -formal-output-dir=%t.indirect %s -disable-output 2>&1 | %FileCheck %s --check-prefix=INDIRECT
; RUN: rm -rf %t.asm && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=asm_entry -formal-output-dir=%t.asm %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ASM
; RUN: rm -rf %t.cc && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=calling_convention_entry -formal-output-dir=%t.cc %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CC
; RUN: rm -rf %t.callsitecc && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=callsite_calling_convention_entry -formal-output-dir=%t.callsitecc %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CALLSITE-CC
; RUN: rm -rf %t.scalable && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=scalable_entry -formal-output-dir=%t.scalable %s -disable-output 2>&1 | %FileCheck %s --check-prefix=SCALABLE

; ATOMIC: UNKNOWN [ATOMIC_INSTRUCTION]
; INDIRECT: UNKNOWN [INDIRECT_CALL]
; ASM: UNKNOWN [INLINE_ASSEMBLY]
; CC: UNKNOWN [UNSUPPORTED_CALLING_CONVENTION]
; CALLSITE-CC: UNKNOWN [UNSUPPORTED_CALLING_CONVENTION]
; CALLSITE-CC-SAME: reachable call site uses a non-C calling convention
; SCALABLE: UNKNOWN [SCALABLE_VECTOR]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@value = global i32 0
@callee = global ptr null

define void @atomic_entry() {
  atomicrmw add ptr @value, i32 1 seq_cst
  ret void
}

define void @indirect_entry() {
  %function = load ptr, ptr @callee
  call void %function()
  ret void
}

define void @asm_entry() {
  call void asm sideeffect "", ""()
  ret void
}

define fastcc void @fast_callee() {
  ret void
}

define void @calling_convention_entry() {
  call fastcc void @fast_callee()
  ret void
}

define void @plain_callee() {
  ret void
}

define void @callsite_calling_convention_entry() {
  call fastcc void @plain_callee()
  ret void
}

define void @scalable_entry() {
  %vector = insertelement <vscale x 4 x i32> poison, i32 0, i64 0
  ret void
}
