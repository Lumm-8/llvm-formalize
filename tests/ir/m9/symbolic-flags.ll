; RUN: %opt -passes=verify -disable-output %s
; The operands of the flagged operations are unrestricted registered bytes.
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@a = private constant [2 x i8] c"a\00"
@b = private constant [2 x i8] c"b\00"
@y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define internal void @consume128(i128 noundef %value) {
  ret void
}

define internal void @consume24(i24 noundef %value) {
  ret void
}

define void @signed_add_exact_shift() {
  %a = alloca i128, align 16
  %b = alloca i128, align 16
  %y = alloca i128, align 16
  call void @__formal_register_input(ptr %a, i64 16, ptr @a)
  call void @__formal_register_input(ptr %b, i64 16, ptr @b)
  call void @__formal_register_output(ptr %y, i64 16, ptr @y)
  %left = load i128, ptr %a, align 16
  %right = load i128, ptr %b, align 16
  %sum = add nsw i128 %left, %right
  %result = lshr exact i128 %sum, 7
  call void @consume128(i128 %result)
  ; Check the flagged payload before storing the ordinary, defined SSA value.
  %plain.sum = add i128 %left, %right
  %plain = lshr i128 %plain.sum, 7
  %same = icmp eq i128 %result, %plain
  br i1 %same, label %done, label %mismatch
done:
  store i128 %plain, ptr %y, align 16
  ret void
mismatch:
  unreachable
}

define void @unsigned_sub() {
  %a = alloca i24, align 4
  %b = alloca i24, align 4
  %y = alloca i24, align 4
  call void @__formal_register_input(ptr %a, i64 3, ptr @a)
  call void @__formal_register_input(ptr %b, i64 3, ptr @b)
  call void @__formal_register_output(ptr %y, i64 3, ptr @y)
  %left = load i24, ptr %a, align 4
  %right = load i24, ptr %b, align 4
  %result = sub nuw i24 %left, %right
  call void @consume24(i24 %result)
  %plain = sub i24 %left, %right
  %same = icmp eq i24 %result, %plain
  br i1 %same, label %done, label %mismatch
done:
  store i24 %plain, ptr %y, align 4
  ret void
mismatch:
  unreachable
}
