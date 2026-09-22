; RUN: rm -rf %t.alloca-fp && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=alloca_fp_entry -formal-output-dir=%t.alloca-fp %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ALLOCA-FP
; RUN: rm -rf %t.alloca-vector && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=alloca_vector_entry -formal-output-dir=%t.alloca-vector %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ALLOCA-VECTOR
; RUN: rm -rf %t.gep-fp && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=gep_fp_entry -formal-output-dir=%t.gep-fp %s -disable-output 2>&1 | %FileCheck %s --check-prefix=GEP-FP
; RUN: rm -rf %t.gep-vector && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=gep_vector_entry -formal-output-dir=%t.gep-vector %s -disable-output 2>&1 | %FileCheck %s --check-prefix=GEP-VECTOR
; RUN: rm -rf %t.constant-gep-fp && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=constant_gep_fp_entry -formal-output-dir=%t.constant-gep-fp %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CONSTANT-GEP-FP
; RUN: rm -rf %t.constant-gep-vector && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=constant_gep_vector_entry -formal-output-dir=%t.constant-gep-vector %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CONSTANT-GEP-VECTOR
; RUN: rm -rf %t.alloca-address-space && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=alloca_address_space_entry -formal-output-dir=%t.alloca-address-space %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ALLOCA-ADDRESS-SPACE
; RUN: rm -rf %t.gep-address-space && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=gep_address_space_entry -formal-output-dir=%t.gep-address-space %s -disable-output 2>&1 | %FileCheck %s --check-prefix=GEP-ADDRESS-SPACE

; ALLOCA-FP: UNKNOWN [FLOATING_POINT]
; ALLOCA-VECTOR: UNKNOWN [FIXED_VECTOR]
; GEP-FP: UNKNOWN [FLOATING_POINT]
; GEP-VECTOR: UNKNOWN [FIXED_VECTOR]
; CONSTANT-GEP-FP: UNKNOWN [FLOATING_POINT]
; CONSTANT-GEP-VECTOR: UNKNOWN [FIXED_VECTOR]
; ALLOCA-ADDRESS-SPACE: UNKNOWN [UNSUPPORTED_ADDRESS_SPACE]
; GEP-ADDRESS-SPACE: UNKNOWN [UNSUPPORTED_ADDRESS_SPACE]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@anchor = global i8 0

declare void @use_ptr(ptr)

define void @alloca_fp_entry() {
  %allocation = alloca double
  ret void
}

define void @alloca_vector_entry() {
  %allocation = alloca <4 x i32>
  ret void
}

define void @gep_fp_entry() {
  %address = getelementptr double, ptr null, i64 1
  ret void
}

define void @gep_vector_entry() {
  %address = getelementptr <4 x i32>, ptr null, i64 1
  ret void
}

define void @constant_gep_fp_entry() {
  call void @use_ptr(ptr getelementptr (double, ptr @anchor, i64 1))
  ret void
}

define void @constant_gep_vector_entry() {
  call void @use_ptr(ptr getelementptr (<4 x i32>, ptr @anchor, i64 1))
  ret void
}

define void @alloca_address_space_entry() {
  %allocation = alloca ptr addrspace(1)
  ret void
}

define void @gep_address_space_entry() {
  %address = getelementptr [1 x ptr addrspace(1)], ptr null, i64 1
  ret void
}
