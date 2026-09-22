; RUN: rm -rf %t.entries && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-producer-entries=1 -formal-output-dir=%t.entries %s -disable-output 2>&1 | %FileCheck %s --check-prefix=ENTRIES
; RUN: rm -rf %t.producerbytes && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-producer-bytes=8 -formal-output-dir=%t.producerbytes %s -disable-output 2>&1 | %FileCheck %s --check-prefix=PRODUCERBYTES
; RUN: rm -rf %t.nodes && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-metadata-nodes=2 -formal-output-dir=%t.nodes %s -disable-output 2>&1 | %FileCheck %s --check-prefix=NODES
; RUN: rm -rf %t.bytes && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-metadata-bytes=8 -formal-output-dir=%t.bytes %s -disable-output 2>&1 | %FileCheck %s --check-prefix=BYTES
; RUN: rm -rf %t.depth && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-metadata-depth=1 -formal-output-dir=%t.depth %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DEPTH
; RUN: rm -rf %t.text && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-module-text-bytes=64 -formal-output-dir=%t.text %s -disable-output 2>&1 | %FileCheck %s --check-prefix=TEXT
; RUN: rm -rf %t.valuewidth && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-integer-bit-width=256 -formal-output-dir=%t.valuewidth %s -disable-output 2>&1 | %FileCheck %s --check-prefix=VALUEWIDTH
; RUN: rm -rf %t.valueconstants && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-max-constant-nodes=2 -formal-output-dir=%t.valueconstants %s -disable-output 2>&1 | %FileCheck %s --check-prefix=VALUECONSTANTS

; ENTRIES: INVALID_TASK [RESOURCE_LIMIT] llvm.ident entry count limit exceeded
; PRODUCERBYTES: INVALID_TASK [RESOURCE_LIMIT] llvm.ident string byte limit exceeded
; NODES: INVALID_TASK [RESOURCE_LIMIT] metadata
; BYTES: INVALID_TASK [RESOURCE_LIMIT] metadata string byte limit exceeded
; DEPTH: INVALID_TASK [RESOURCE_LIMIT] metadata nesting depth limit exceeded
; TEXT: INVALID_TASK [RESOURCE_LIMIT] canonical module text byte limit exceeded
; VALUEWIDTH: INVALID_TASK [RESOURCE_LIMIT] LLVM integer bit width limit exceeded
; VALUECONSTANTS: INVALID_TASK [RESOURCE_LIMIT] constant

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  ret void
}

!llvm.ident = !{!0, !1}
!formal.deep = !{!2}
!formal.values = !{!4, !5}
!0 = !{!"clang version 20.0.0git eccf4d44d346eee498b0ff709e625e3104448751"}
!1 = !{!"second producer entry"}
!2 = !{!3}
!3 = !{!"deep metadata string"}
!4 = !{i257 0}
!5 = !{i8 1, i8 2, i8 3}
