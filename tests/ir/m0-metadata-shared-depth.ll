; RUN: rm -rf %t && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-allow-unversioned-ir -formal-max-metadata-depth=3 -formal-output-dir=%t %s -disable-output 2>&1 | %FileCheck %s

; CHECK: INVALID_TASK [RESOURCE_LIMIT] metadata nesting depth limit exceeded

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
  ret void
}

; The deep root is deliberately listed before all of its descendants. The
; LIFO walker therefore observes each shared node at depth zero before it
; reaches the same node through the deep path.
!formal.shared_depth = !{!0, !1, !2, !3, !4, !5, !6}
!0 = distinct !{!1}
!1 = distinct !{!2}
!2 = distinct !{!3}
!3 = distinct !{!4}
!4 = distinct !{!5}
!5 = distinct !{!6}
!6 = distinct !{}
