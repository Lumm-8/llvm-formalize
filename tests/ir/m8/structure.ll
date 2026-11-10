; RUN: %scalar-executor %s --entry break_loop --loop-bound 1 | %FileCheck %s
; CHECK: M3 status M3_EXECUTOR_COMPLETE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
declare void @__formal_register_input(ptr, i64, ptr)

define void @break_loop() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, 4
  br i1 %more, label %body, label %done
body:
  %break = icmp eq i32 %i, 1
  br i1 %break, label %done, label %latch
latch:
  %next = add i32 %i, 1
  br label %header
done:
  ret void
}

define void @continue_loop() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %continue ], [ 2, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %body, label %done
body:
  %first = icmp eq i32 %i, 0
  br i1 %first, label %continue, label %latch
continue:
  br label %header
latch:
  br label %header
done:
  ret void
}

define void @multiple_exits() {
entry:
  %input = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  %x = load i8, ptr %input, align 1
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %body, label %done
body:
  %second = icmp eq i32 %i, 1
  br i1 %second, label %choose_exit, label %latch
latch:
  br label %header
choose_exit:
  %which = icmp eq i8 %x, 0
  br i1 %which, label %exit_a, label %exit_b
exit_a:
  ret void
exit_b:
  ret void
done:
  ret void
}

define void @nested_loops() {
entry:
  br label %outer_header
outer_header:
  %i = phi i32 [ 0, %entry ], [ %next_i, %outer_latch ]
  %outer_more = icmp ult i32 %i, 2
  br i1 %outer_more, label %outer_body, label %done
outer_body:
  br label %inner_header
inner_header:
  %j = phi i32 [ 0, %outer_body ], [ %next_j, %inner_latch ]
  %inner_more = icmp ult i32 %j, 2
  br i1 %inner_more, label %inner_latch, label %outer_latch
inner_latch:
  %next_j = add i32 %j, 1
  br label %inner_header
outer_latch:
  %next_i = add i32 %i, 1
  br label %outer_header
done:
  ret void
}

define void @callee_loop() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done
latch:
  br label %header
done:
  ret void
}

define void @repeated_callee() {
entry:
  call void @callee_loop()
  call void @callee_loop()
  ret void
}

define void @recursive_loop(i32 %depth) {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %after
latch:
  br label %header
after:
  %recurse = icmp ult i32 %depth, 2
  br i1 %recurse, label %call, label %done
call:
  %next_depth = add i32 %depth, 1
  call void @recursive_loop(i32 %next_depth)
  br label %done
done:
  ret void
}

define void @recursive_top() {
entry:
  call void @recursive_loop(i32 0)
  ret void
}

define void @forked_loop() {
entry:
  %input = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  %x = load i8, ptr %input, align 1
  %short = icmp eq i8 %x, 0
  %limit = select i1 %short, i32 1, i32 2
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, %limit
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header
done:
  ret void
}

define void @infinite_loop() {
entry:
  br label %header
header:
  br label %header
}

define void @formal_override() {
entry:
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header, !llvm.loop !0
done:
  ret void
}

define void @unroll_override() {
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

!0 = distinct !{!0, !1, !2}
!1 = !{!"formal.loop.bound", i32 2}
!2 = !{!"llvm.loop.unroll.count", i32 1}
!3 = distinct !{!3, !4}
!4 = !{!"llvm.loop.unroll.count", i32 1}
