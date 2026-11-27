; ModuleID = '/home/dengshy/github/llvm-formalize/tests/ir/m8/structure.ll'
source_filename = "/home/dengshy/github/llvm-formalize/tests/ir/m8/structure.ll"
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"

declare void @__formal_register_input(ptr, i64, ptr)

define void @break_loop() {
entry:
  br label %header

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, 4
  br i1 %more, label %body, label %done

body:                                             ; preds = %header
  %break = icmp eq i32 %i, 1
  br i1 %break, label %done, label %latch

latch:                                            ; preds = %body
  %next = add i32 %i, 1
  br label %header

done:                                             ; preds = %body, %header
  ret void
}

define void @continue_loop() {
entry:
  br label %header

header:                                           ; preds = %latch, %continue, %entry
  %i = phi i32 [ 0, %entry ], [ 1, %continue ], [ 2, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %body, label %done

body:                                             ; preds = %header
  %first = icmp eq i32 %i, 0
  br i1 %first, label %continue, label %latch

continue:                                         ; preds = %body
  br label %header

latch:                                            ; preds = %body
  br label %header

done:                                             ; preds = %header
  ret void
}

define void @multiple_exits() {
entry:
  %input = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  %x = load i8, ptr %input, align 1
  br label %header

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %body, label %done

body:                                             ; preds = %header
  %second = icmp eq i32 %i, 1
  br i1 %second, label %choose_exit, label %latch

latch:                                            ; preds = %body
  br label %header

choose_exit:                                      ; preds = %body
  %which = icmp eq i8 %x, 0
  br i1 %which, label %exit_a, label %exit_b

exit_a:                                           ; preds = %choose_exit
  ret void

exit_b:                                           ; preds = %choose_exit
  ret void

done:                                             ; preds = %header
  ret void
}

define void @nested_loops() {
entry:
  br label %outer_header

outer_header:                                     ; preds = %outer_latch, %entry
  %i = phi i32 [ 0, %entry ], [ %next_i, %outer_latch ]
  %outer_more = icmp ult i32 %i, 2
  br i1 %outer_more, label %outer_body, label %done

outer_body:                                       ; preds = %outer_header
  br label %inner_header

inner_header:                                     ; preds = %inner_latch, %outer_body
  %j = phi i32 [ 0, %outer_body ], [ %next_j, %inner_latch ]
  %inner_more = icmp ult i32 %j, 2
  br i1 %inner_more, label %inner_latch, label %outer_latch

inner_latch:                                      ; preds = %inner_header
  %next_j = add i32 %j, 1
  br label %inner_header

outer_latch:                                      ; preds = %inner_header
  %next_i = add i32 %i, 1
  br label %outer_header

done:                                             ; preds = %outer_header
  ret void
}

define void @callee_loop() {
entry:
  br label %header

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done

latch:                                            ; preds = %header
  br label %header

done:                                             ; preds = %header
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

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %after

latch:                                            ; preds = %header
  br label %header

after:                                            ; preds = %header
  %recurse = icmp ult i32 %depth, 2
  br i1 %recurse, label %call, label %done

call:                                             ; preds = %after
  %next_depth = add i32 %depth, 1
  call void @recursive_loop(i32 %next_depth)
  br label %done

done:                                             ; preds = %call, %after
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

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, %limit
  br i1 %more, label %latch, label %done

latch:                                            ; preds = %header
  %next = add i32 %i, 1
  br label %header

done:                                             ; preds = %header
  ret void
}

define void @infinite_loop() {
entry:
  br label %header

header:                                           ; preds = %header, %entry
  br label %header
}

define void @formal_override() {
entry:
  br label %header

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %more = icmp ult i32 %i, 2
  br i1 %more, label %latch, label %done

latch:                                            ; preds = %header
  %next = add i32 %i, 1
  br label %header, !llvm.loop !0

done:                                             ; preds = %header
  ret void
}

define void @unroll_override() {
entry:
  br label %header

header:                                           ; preds = %latch, %entry
  %i = phi i32 [ 0, %entry ], [ 1, %latch ]
  %more = icmp ult i32 %i, 1
  br i1 %more, label %latch, label %done

latch:                                            ; preds = %header
  br label %header, !llvm.loop !3

done:                                             ; preds = %header
  ret void
}

!0 = distinct !{!0, !1, !2}
!1 = !{!"formal.loop.bound", i32 2}
!2 = !{!"llvm.loop.unroll.count", i32 1}
!3 = distinct !{!3, !2}
