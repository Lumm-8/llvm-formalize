; RUN: %scalar-executor %s --entry custom --loop-bound 0 | %FileCheck %s --check-prefix=CUSTOM
; RUN: %scalar-executor %s --entry consumed --loop-bound 2 | %FileCheck %s --check-prefix=LOST
; CUSTOM: M3 status M3_EXECUTOR_COMPLETE
; LOST: M3 status UNKNOWN
; LOST: reason=UNKNOWN-LOOP-METADATA-LOST

define i32 @custom() {
start:
  br label %header
header:
  %i = phi i32 [ 0, %start ], [ %next, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header, !llvm.loop !0
done:
  ret i32 %i
}

define i32 @consumed() {
start:
  br label %header
header:
  %i = phi i32 [ 0, %start ], [ %next, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header, !llvm.loop !3
done:
  ret i32 %i
}

!0 = distinct !{!0, !1, !2}
!1 = !{!"formal.loop.bound", i32 2}
!2 = !{!"llvm.loop.unroll.count", i32 1}
!3 = distinct !{!3, !4}
!4 = !{!"llvm.loop.unroll.disable"}
