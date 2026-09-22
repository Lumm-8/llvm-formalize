; RUN: rm -rf %t && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t %s -disable-output 2>&1 | %FileCheck %s

; CHECK: UNKNOWN [FLOATING_POINT]
; CHECK-SAME: global initializer 'floating_constant_tree'

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@floating_constant_tree = global [2 x double] [double 1.000000e+00, double 2.000000e+00]

define void @formal_top() {
  ret void
}
