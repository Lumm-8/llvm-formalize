; RUN: rm -rf %t.width && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-integer-bit-width=256 -formal-output-dir=%t.width %s -disable-output 2>&1 | %FileCheck %s --check-prefix=WIDTH
; RUN: rm -rf %t.depth && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-aggregate-depth=2 -formal-output-dir=%t.depth %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DEPTH
; RUN: rm -rf %t.initializer && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-global-initializer-bytes=16 -formal-output-dir=%t.initializer %s -disable-output 2>&1 | %FileCheck %s --check-prefix=INITIALIZER
; RUN: rm -rf %t.constants && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-constant-nodes=2 -formal-output-dir=%t.constants %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CONSTANTS
; RUN: rm -rf %t.constant-depth && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-constant-depth=4 -formal-output-dir=%t.constant-depth %s -disable-output 2>&1 | %FileCheck %s --check-prefix=CONSTANT-DEPTH
; RUN: rm -rf %t.types && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-type-work=2 -formal-output-dir=%t.types %s -disable-output 2>&1 | %FileCheck %s --check-prefix=TYPES

; WIDTH: INVALID_TASK [RESOURCE_LIMIT] LLVM integer bit width limit exceeded
; DEPTH: INVALID_TASK [RESOURCE_LIMIT] LLVM aggregate type nesting limit exceeded
; INITIALIZER: INVALID_TASK [RESOURCE_LIMIT] global initializer size limit exceeded
; CONSTANTS: INVALID_TASK [RESOURCE_LIMIT] constant
; CONSTANT-DEPTH: INVALID_TASK [RESOURCE_LIMIT] constant graph nesting limit exceeded
; TYPES: INVALID_TASK [RESOURCE_LIMIT] type traversal work limit exceeded

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@wide = global i257 0
@nested = global [1 x [1 x [1 x i8]]] zeroinitializer
@large = global [32 x i8] zeroinitializer
@constants = global [4 x i8] [i8 1, i8 2, i8 3, i8 4]
@constant_depth_anchor = global i8 0
@constant_depth = global i64 xor (i64 add (i64 xor (i64 add (i64 xor (i64 add (i64 ptrtoint (ptr @constant_depth_anchor to i64), i64 1), i64 2), i64 3), i64 4), i64 5), i64 6)

define void @formal_top() {
  ret void
}
