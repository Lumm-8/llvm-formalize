target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@input = internal global i8 0
@output = internal global i8 0
@x = private constant [2 x i8] c"x\00"
@y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define internal i8 @prepare() {
  call void @__formal_register_input(ptr @input, i64 1, ptr @x)
  call void @__formal_register_output(ptr @output, i64 1, ptr @y)
  %value = load i8, ptr @input, align 1
  ret i8 %value
}

define void @pointer_phi() {
entry:
  %p = alloca i8, align 1
  %q = alloca i8, align 1
  store i8 10, ptr %p, align 1
  store i8 20, ptr %q, align 1
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %chosen = phi ptr [ %p, %l ], [ %q, %r ]
  %v = load i8, ptr %chosen, align 1
  store i8 %v, ptr @output, align 1
  ret void
}
