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
  %eq = icmp eq i8 %x, 23
  %reordered = icmp eq i8 23, %x
  br i1 %eq, label %equal, label %different

equal:
  br i1 %reordered, label %equal_again, label %impossible
equal_again:
  br i1 %eq, label %same_result, label %impossible
same_result:
  br label %done

different:
  br i1 %reordered, label %impossible, label %different_again
different_again:
  br i1 %eq, label %impossible, label %different_result
different_result:
  br label %done

impossible:
  unreachable

done:
  %y = phi i8 [ 1, %same_result ], [ 0, %different_result ]
  store i8 %y, ptr %output, align 1
  ret void
}
