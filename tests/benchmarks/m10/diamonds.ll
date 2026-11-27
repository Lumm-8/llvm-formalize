target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
@output_name = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @diamond_top() {
entry:
  %input = alloca i8, align 1
  %output = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  %x = load i8, ptr %input, align 1
  %bit0 = and i8 %x, 1
  %test0 = icmp ne i8 %bit0, 0
  br i1 %test0, label %set0, label %clear0

set0:
  %high0 = add i8 0, 1
  br label %join0

clear0:
  %low0 = xor i8 0, 2
  br label %join0

join0:
  %result0 = phi i8 [ %high0, %set0 ], [ %low0, %clear0 ]
  %bit1 = and i8 %x, 2
  %test1 = icmp ne i8 %bit1, 0
  br i1 %test1, label %set1, label %clear1

set1:
  %high1 = add i8 %result0, 2
  br label %join1

clear1:
  %low1 = xor i8 %result0, 4
  br label %join1

join1:
  %result1 = phi i8 [ %high1, %set1 ], [ %low1, %clear1 ]
  %bit2 = and i8 %x, 4
  %test2 = icmp ne i8 %bit2, 0
  br i1 %test2, label %set2, label %clear2

set2:
  %high2 = add i8 %result1, 4
  br label %join2

clear2:
  %low2 = xor i8 %result1, 8
  br label %join2

join2:
  %result2 = phi i8 [ %high2, %set2 ], [ %low2, %clear2 ]
  %bit3 = and i8 %x, 8
  %test3 = icmp ne i8 %bit3, 0
  br i1 %test3, label %set3, label %clear3

set3:
  %high3 = add i8 %result2, 8
  br label %join3

clear3:
  %low3 = xor i8 %result2, 16
  br label %join3

join3:
  %result3 = phi i8 [ %high3, %set3 ], [ %low3, %clear3 ]
  %bit4 = and i8 %x, 16
  %test4 = icmp ne i8 %bit4, 0
  br i1 %test4, label %set4, label %clear4

set4:
  %high4 = add i8 %result3, 16
  br label %join4

clear4:
  %low4 = xor i8 %result3, 32
  br label %join4

join4:
  %result4 = phi i8 [ %high4, %set4 ], [ %low4, %clear4 ]
  store i8 %result4, ptr %output, align 1
  ret void
}

define void @unsafe_after_join() {
entry:
  %input = alloca i8, align 1
  %output = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  %x = load i8, ptr %input, align 1
  %zero = icmp eq i8 %x, 0
  br i1 %zero, label %bad, label %good

bad:
  br label %join

good:
  br label %join

join:
  %denominator = phi i8 [ 0, %bad ], [ 1, %good ]
  %value = udiv i8 42, %denominator
  store i8 %value, ptr %output, align 1
  ret void
}
