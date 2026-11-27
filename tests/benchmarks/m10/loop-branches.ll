target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
@output_name = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @loop_top() {
entry:
  %input = alloca i8, align 1
  %output = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  %x = load i8, ptr %input, align 1
  br label %header

header:
  %i = phi i8 [ 0, %entry ], [ %next, %join ]
  %sum = phi i8 [ 0, %entry ], [ %value, %join ]
  %more = icmp ult i8 %i, 5
  br i1 %more, label %branch, label %done

branch:
  %shifted = lshr i8 %x, %i
  %bit = and i8 %shifted, 1
  %test = icmp ne i8 %bit, 0
  br i1 %test, label %set, label %clear

set:
  %high = add i8 %sum, %i
  br label %join

clear:
  %low = xor i8 %sum, 11
  br label %join

join:
  %value = phi i8 [ %high, %set ], [ %low, %clear ]
  %next = add i8 %i, 1
  br label %header

done:
  store i8 %sum, ptr %output, align 1
  ret void
}
