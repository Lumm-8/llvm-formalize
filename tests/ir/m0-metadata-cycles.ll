; RUN: rm -rf %t && %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-allow-unversioned-ir -formal-max-metadata-depth=1 -formal-output-dir=%t %s -disable-output 2>&1 | %FileCheck %s

; CHECK: FormalPass: preflight validated (opt operation succeeded; no formal verdict)

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
entry:
  br label %loop

loop:
  br i1 false, label %loop, label %exit, !llvm.loop !0

exit:
  ret void
}

; Canonical loop metadata is self-referential.
!0 = distinct !{!0}

; Generic cyclic metadata can also contain more than one node.
!formal.multi_cycle = !{!1}
!1 = distinct !{!2}
!2 = distinct !{!1}
