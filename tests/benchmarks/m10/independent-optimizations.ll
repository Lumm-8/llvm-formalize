target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input_name = private constant [2 x i8] c"x\00"
@output_name = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @formal_top() {
entry:
  %input = alloca i8, align 1
  %output = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  %x = load i8, ptr %input, align 1
  %z = xor i8 %x, 7
  %a = add i8 %x, %z
  %b = add i8 %z, %x
  %sum = xor i8 %a, %b
  %decision = icmp ult i8 %x, 128
  br i1 %decision, label %lower, label %higher

lower:
  br i1 %decision, label %lower_again, label %impossible
lower_again:
  br i1 %decision, label %left, label %impossible
left:
  %l = add i8 %sum, 17
  br label %done

higher:
  br i1 %decision, label %impossible, label %higher_again
higher_again:
  br i1 %decision, label %impossible, label %right
right:
  %r = add i8 %sum, 42
  br label %done

impossible:
  unreachable

done:
  %y = phi i8 [ %l, %left ], [ %r, %right ]
  store i8 %y, ptr %output, align 1
  ret void
}

define void @unsafe_top() {
entry:
  %input = alloca i8, align 1
  %output = alloca i8, align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  %x = load i8, ptr %input, align 1
  %a = add i8 %x, 1
  %b = add i8 1, %x
  %difference = sub i8 %a, %b
  %denominator = or i8 %difference, %x
  %y = udiv i8 42, %denominator
  store i8 %y, ptr %output, align 1
  ret void
}
