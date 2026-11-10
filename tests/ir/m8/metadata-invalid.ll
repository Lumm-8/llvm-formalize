; RUN: %scalar-executor %s --entry malformed --loop-bound 10 | %FileCheck %s
; CHECK: M3 status UNKNOWN
; CHECK: reason=UNKNOWN-LOOP-UNSUPPORTED

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @malformed() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done
latch:
  br label %header, !llvm.loop !0
done:
  ret void
}

define void @duplicate() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done
latch:
  br label %header, !llvm.loop !3
done:
  ret void
}

define void @out_of_range() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done
latch:
  br label %header, !llvm.loop !4
done:
  ret void
}

define void @negative() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done
latch:
  br label %header, !llvm.loop !11
done:
  ret void
}

define void @conflicting_latches() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %a ], [ 1, %b ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %body, label %done
body:
  br i1 true, label %a, label %b
a:
  br label %header, !llvm.loop !5
b:
  br label %header, !llvm.loop !6
done:
  ret void
}

!0 = distinct !{!0, !1}
!1 = !{!"formal.loop.bound", !"two"}
!3 = distinct !{!3, !7, !7}
!4 = distinct !{!4, !8}
!5 = distinct !{!5, !9}
!6 = distinct !{!6, !10}
!7 = !{!"formal.loop.bound", i32 1}
!8 = !{!"formal.loop.bound", i128 18446744073709551616}
!9 = !{!"formal.loop.bound", i32 1}
!10 = !{!"formal.loop.bound", i32 2}
!11 = distinct !{!11, !12}
!12 = !{!"formal.loop.bound", i32 -1}
