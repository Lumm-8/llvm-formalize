; RUN: %scalar-executor %s --m5 --entry formal_top --output-dir %t.registered --loop-bound 2 | %FileCheck %s --check-prefix=LOCAL
; LOCAL: M5 status SAFE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@output_name = private constant [2 x i8] c"y\00"
declare void @__formal_register_output(ptr, i64, ptr)

define void @formal_top() {
entry:
  %output = alloca i8, align 1
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, 3
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header, !llvm.loop !0
done:
  %value = trunc i32 %i to i8
  store i8 %value, ptr %output, align 1
  ret void
}

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.unroll.count", i32 3}
