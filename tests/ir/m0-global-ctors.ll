; RUN: rm -rf %t.dir && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -disable-output 2>&1 | %FileCheck %s
; RUN: %FileCheck %s --check-prefix=JSON < %t.dir/runs/run-*/result.json
; CHECK: INVALID_TASK [DYNAMIC_GLOBAL_INITIALIZERS]
; JSON: "reason": "DYNAMIC_GLOBAL_INITIALIZERS"

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @ctor, ptr null }]

define void @ctor() { ret void }
define void @formal_top() { ret void }
