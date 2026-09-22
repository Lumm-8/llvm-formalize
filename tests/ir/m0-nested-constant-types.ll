; RUN: rm -rf %t.addrspace && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=addrspace_constant_entry -formal-output-dir=%t.addrspace %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ADDRSPACE
; RUN: rm -rf %t.vector && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=vector_constant_entry -formal-output-dir=%t.vector %s -disable-output 2>&1 | %FileCheck %s --check-prefix=VECTOR

; ADDRSPACE: UNKNOWN [UNSUPPORTED_ADDRESS_SPACE]
; VECTOR: UNKNOWN [FIXED_VECTOR]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@default_anchor = global i8 0
@other_address_space = addrspace(1) global i8 0

declare void @__formal_assume(i32)
declare void @use_i64(i64)

define void @addrspace_constant_entry() {
  call void @__formal_assume(i32 ptrtoint (ptr addrspace(1) @other_address_space to i32))
  ret void
}

define void @vector_constant_entry() {
  call void @use_i64(i64 bitcast (<2 x i32> <i32 ptrtoint (ptr @default_anchor to i32), i32 0> to i64))
  ret void
}
