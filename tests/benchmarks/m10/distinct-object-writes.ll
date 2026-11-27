target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
@output_name = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @distinct_writes_top() {
entry:
  %input = alloca i8, align 1
  %output = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  %x = load i8, ptr %input, align 1
  %a = alloca i8, align 1
  %b = alloca i8, align 1
  store i8 0, ptr %a, align 1
  store i8 0, ptr %b, align 1
  %bit0 = and i8 %x, 1
  %test0 = icmp ne i8 %bit0, 0
  br i1 %test0, label %set0, label %clear0

set0:
  %high0 = add i8 0, 1
  store i8 %high0, ptr %a, align 1
  br label %join0

clear0:
  %low0 = xor i8 0, 2
  store i8 %low0, ptr %b, align 1
  br label %join0

join0:
  %av0 = load i8, ptr %a, align 1
  %bv0 = load i8, ptr %b, align 1
  %sum0 = add i8 %av0, %bv0
  %bit1 = and i8 %x, 2
  %test1 = icmp ne i8 %bit1, 0
  br i1 %test1, label %set1, label %clear1

set1:
  %high1 = add i8 %sum0, 2
  store i8 %high1, ptr %a, align 1
  br label %join1

clear1:
  %low1 = xor i8 %sum0, 4
  store i8 %low1, ptr %b, align 1
  br label %join1

join1:
  %av1 = load i8, ptr %a, align 1
  %bv1 = load i8, ptr %b, align 1
  %sum1 = add i8 %av1, %bv1
  %bit2 = and i8 %x, 4
  %test2 = icmp ne i8 %bit2, 0
  br i1 %test2, label %set2, label %clear2

set2:
  %high2 = add i8 %sum1, 4
  store i8 %high2, ptr %a, align 1
  br label %join2

clear2:
  %low2 = xor i8 %sum1, 8
  store i8 %low2, ptr %b, align 1
  br label %join2

join2:
  %av2 = load i8, ptr %a, align 1
  %bv2 = load i8, ptr %b, align 1
  %sum2 = add i8 %av2, %bv2
  %bit3 = and i8 %x, 8
  %test3 = icmp ne i8 %bit3, 0
  br i1 %test3, label %set3, label %clear3

set3:
  %high3 = add i8 %sum2, 8
  store i8 %high3, ptr %a, align 1
  br label %join3

clear3:
  %low3 = xor i8 %sum2, 16
  store i8 %low3, ptr %b, align 1
  br label %join3

join3:
  %av3 = load i8, ptr %a, align 1
  %bv3 = load i8, ptr %b, align 1
  %sum3 = add i8 %av3, %bv3
  %bit4 = and i8 %x, 16
  %test4 = icmp ne i8 %bit4, 0
  br i1 %test4, label %set4, label %clear4

set4:
  %high4 = add i8 %sum3, 16
  store i8 %high4, ptr %a, align 1
  br label %join4

clear4:
  %low4 = xor i8 %sum3, 32
  store i8 %low4, ptr %b, align 1
  br label %join4

join4:
  %av4 = load i8, ptr %a, align 1
  %bv4 = load i8, ptr %b, align 1
  %sum4 = add i8 %av4, %bv4
  store i8 %sum4, ptr %output, align 1
  ret void
}
