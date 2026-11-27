; ModuleID = 'tests/benchmarks/m10/merge-compatibility.ll'
source_filename = "tests/benchmarks/m10/merge-compatibility.ll"
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@input = internal global i8 0
@output = internal global i8 0
@extra = internal global i8 0
@x = private constant [2 x i8] c"x\00"
@y = private constant [2 x i8] c"y\00"
@a = private constant [2 x i8] c"a\00"
@b = private constant [2 x i8] c"b\00"

declare void @__formal_register_input(ptr, i64, ptr)

declare void @__formal_register_output(ptr, i64, ptr)

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #0

declare ptr @malloc(i64)

declare void @free(ptr)

define internal i8 @prepare() {
  call void @__formal_register_input(ptr @input, i64 1, ptr @x)
  call void @__formal_register_output(ptr @output, i64 1, ptr @y)
  %value = load i8, ptr @input, align 1
  ret i8 %value
}

define void @nested_join() {
entry:
  %x = call i8 @prepare()
  %c = icmp ult i8 %x, -128
  %d = trunc i8 %x to i1
  br i1 %c, label %inner, label %outer_other

inner:                                            ; preds = %entry
  br i1 %d, label %l, label %r

l:                                                ; preds = %inner
  br label %inner_join

r:                                                ; preds = %inner
  br label %inner_join

inner_join:                                       ; preds = %r, %l
  %v = phi i8 [ 1, %l ], [ 2, %r ]
  br label %outer_join

outer_other:                                      ; preds = %entry
  br label %outer_join

outer_join:                                       ; preds = %outer_other, %inner_join
  %out = phi i8 [ %v, %inner_join ], [ 3, %outer_other ]
  store i8 %out, ptr @output, align 1
  ret void
}

define void @equal_scalar() {
entry:
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %v = phi i8 [ %x, %l ], [ %x, %r ]
  store i8 %v, ptr @output, align 1
  ret void
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

define void @preserved_shadow() {
entry:
  %p = alloca i8, align 1
  %slot = alloca { ptr, i8 }, align 8
  %byte = getelementptr { ptr, i8 }, ptr %slot, i64 0, i32 1
  store i8 5, ptr %p, align 1
  store ptr %p, ptr %slot, align 8
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  store i8 10, ptr %byte, align 1
  br label %join

r:                                                ; preds = %entry
  store i8 20, ptr %byte, align 1
  br label %join

join:                                             ; preds = %r, %l
  %loaded = load ptr, ptr %slot, align 8
  %a = load i8, ptr %loaded, align 1
  %b = load i8, ptr %byte, align 1
  %v = add i8 %a, %b
  store i8 %v, ptr @output, align 1
  ret void
}

define void @aggregate_phi() {
entry:
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %tuple = phi { i8, i32 } [ { i8 1, i32 111 }, %l ], [ { i8 2, i32 222 }, %r ]
  %low = extractvalue { i8, i32 } %tuple, 0
  %high = extractvalue { i8, i32 } %tuple, 1
  %n = trunc i32 %high to i8
  %v = add i8 %n, %low
  store i8 %v, ptr @output, align 1
  ret void
}

define void @poison_phi() {
entry:
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %maybe = phi i8 [ poison, %l ], [ 7, %r ]
  %safe = select i1 %c, i8 17, i8 %maybe
  %test = icmp ult i8 %safe, 30
  br i1 %test, label %done, label %impossible

done:                                             ; preds = %join
  store i8 1, ptr @output, align 1
  ret void

impossible:                                       ; preds = %join
  unreachable
}

define void @freeze_identity() {
entry:
  %x = call i8 @prepare()
  %choice = freeze i1 undef
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %same = phi i1 [ %choice, %l ], [ %choice, %r ]
  %bad = xor i1 %choice, %same
  br i1 %bad, label %impossible, label %done

done:                                             ; preds = %join
  %v = zext i1 %same to i8
  store i8 %v, ptr @output, align 1
  ret void

impossible:                                       ; preds = %join
  unreachable
}

define internal i8 @recur(i8 %x, i8 %n) {
entry:
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %v = phi i8 [ 3, %l ], [ 7, %r ]
  %last = icmp eq i8 %n, 0
  br i1 %last, label %done, label %step

done:                                             ; preds = %join
  ret i8 %v

step:                                             ; preds = %join
  %next = sub i8 %n, 1
  %next_x = lshr i8 %x, 1
  %tail = call i8 @recur(i8 %next_x, i8 %next)
  %sum = add i8 %tail, %v
  ret i8 %sum
}

define internal i8 @call_recur(i8 %x) {
  %v = call i8 @recur(i8 %x, i8 2)
  ret i8 %v
}

define void @nested_calls() {
  %x = call i8 @prepare()
  %v = call i8 @call_recur(i8 %x)
  store i8 %v, ptr @output, align 1
  ret void
}

define void @initialization_differs() {
entry:
  %p = alloca i16, align 2
  %other = getelementptr i8, ptr %p, i64 1
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  store i8 1, ptr %p, align 1
  br label %join

r:                                                ; preds = %entry
  store i8 2, ptr %other, align 1
  br label %join

join:                                             ; preds = %r, %l
  ret void
}

define void @allocations_differ() {
entry:
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  %p = alloca i8, align 1
  br label %join

r:                                                ; preds = %entry
  %unused = add i8 %x, 1
  br label %join

join:                                             ; preds = %r, %l
  ret void
}

define void @lifetimes_differ() {
entry:
  %p = alloca i8, align 1
  %heap = call ptr @malloc(i64 1)
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  call void @llvm.lifetime.end.p0(i64 1, ptr %p)
  br label %join

r:                                                ; preds = %entry
  call void @free(ptr %heap)
  br label %join

join:                                             ; preds = %r, %l
  ret void
}

define void @descriptors_differ() {
entry:
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  call void @__formal_register_output(ptr @extra, i64 1, ptr @a)
  br label %join

r:                                                ; preds = %entry
  call void @__formal_register_output(ptr @extra, i64 1, ptr @b)
  br label %join

join:                                             ; preds = %r, %l
  ret void
}

define void @undef_on_one_edge() {
entry:
  %x = call i8 @prepare()
  %c = trunc i8 %x to i1
  br i1 %c, label %l, label %r

l:                                                ; preds = %entry
  br label %join

r:                                                ; preds = %entry
  br label %join

join:                                             ; preds = %r, %l
  %v = phi i8 [ undef, %l ], [ 1, %r ]
  store i8 %v, ptr @output, align 1
  ret void
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
