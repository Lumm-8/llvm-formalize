; RUN: rm -rf %t.valid && %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=registration_entry -formal-output-dir=%t.valid %s -disable-output
; RUN: %FileCheck %s --check-prefix=VALID-JSON < %t.valid/runs/run-*/result.json
; RUN: rm -rf %t.bad-name && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=bad_registration_entry -formal-output-dir=%t.bad-name %s -disable-output 2>&1 | %FileCheck %s --check-prefix=BAD-NAME
; RUN: rm -rf %t.duplicate && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=duplicate_registration_entry -formal-output-dir=%t.duplicate %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DUPLICATE
; RUN: rm -rf %t.address-space && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=address_space_entry -formal-output-dir=%t.address-space %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ADDRESS-SPACE
; RUN: rm -rf %t.limit && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=registration_entry -formal-max-instructions=1 -formal-output-dir=%t.limit %s -disable-output 2>&1 | %FileCheck %s --check-prefix=LIMIT
; RUN: rm -rf %t.alias && %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=alias_entry -formal-output-dir=%t.alias %s -disable-output

; VALID-JSON: "reason": "VALID"
; BAD-NAME: INVALID_TASK [INVALID_REGISTRATION]
; DUPLICATE: INVALID_TASK [INVALID_REGISTRATION]
; ADDRESS-SPACE: UNKNOWN [UNSUPPORTED_ADDRESS_SPACE]
; LIMIT: INVALID_TASK [RESOURCE_LIMIT]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private unnamed_addr constant [6 x i8] c"input\00"
@output_name = private unnamed_addr constant [7 x i8] c"output\00"
@storage = global i8 0

declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @__formal_assume(i32)
@defined_alias = alias void (), ptr @defined_callee

define void @registration_entry() {
  call void @__formal_register_input(ptr @storage, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr @storage, i64 1, ptr @output_name)
  call void @__formal_assume(i32 1)
  ret void
}

define void @bad_registration_entry() {
  %dynamic_name = alloca [2 x i8]
  call void @__formal_register_input(ptr @storage, i64 1, ptr %dynamic_name)
  ret void
}

define void @duplicate_registration_entry() {
  call void @__formal_register_input(ptr @storage, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr @storage, i64 1, ptr @input_name)
  ret void
}

define void @address_space_entry() {
  %other = addrspacecast ptr @storage to ptr addrspace(1)
  ret void
}

define void @defined_callee() {
  ret void
}

define void @alias_entry() {
  call void @defined_alias()
  ret void
}
