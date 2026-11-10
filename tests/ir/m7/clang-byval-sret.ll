; Generated from tests/c/m7_abi.c by pinned clang-20 -O0 -S -emit-llvm
; -Xclang -disable-O0-optnone. Keep the ABI attributes intact.
; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry abi_entry | %FileCheck %s --check-prefix=ABI
; ABI: M4 status M4_EXECUTOR_COMPLETE
; ABI: return=bv64:24
; ModuleID = 'tests/c/m7_abi.c'
source_filename = "tests/c/m7_abi.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.Triple = type { i64, i64, i64 }
%struct.PointerTriple = type { ptr, i64, i64 }

@__const.make_triple.value = private unnamed_addr constant %struct.Triple { i64 7, i64 8, i64 9 }, align 8

; Function Attrs: noinline nounwind uwtable
define dso_local void @make_triple(ptr dead_on_unwind noalias writable sret(%struct.Triple) align 8 %agg.result) #0 {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %agg.result, ptr align 8 @__const.make_triple.value, i64 24, i1 false)
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i64 @sum_triple(ptr noundef byval(%struct.Triple) align 8 %value) #0 {
entry:
  %a = getelementptr inbounds nuw %struct.Triple, ptr %value, i32 0, i32 0
  store i64 17, ptr %a, align 8
  %a1 = getelementptr inbounds nuw %struct.Triple, ptr %value, i32 0, i32 0
  %0 = load i64, ptr %a1, align 8
  ret i64 %0
}

; Function Attrs: noinline nounwind uwtable
define dso_local i64 @mutate_pointer_triple(ptr noundef byval(%struct.PointerTriple) align 8 %value) #0 {
entry:
  %target = getelementptr inbounds nuw %struct.PointerTriple, ptr %value, i32 0, i32 0
  %0 = load ptr, ptr %target, align 8
  store i64 31, ptr %0, align 8
  %target1 = getelementptr inbounds nuw %struct.PointerTriple, ptr %value, i32 0, i32 0
  store ptr null, ptr %target1, align 8
  ret i64 2
}

; Function Attrs: noinline nounwind uwtable
define dso_local i64 @abi_entry() #0 {
entry:
  %retval = alloca i64, align 8
  %value = alloca %struct.Triple, align 8
  %mutated = alloca i64, align 8
  %target = alloca i64, align 8
  %pointer_value = alloca %struct.PointerTriple, align 8
  %pointer_result = alloca i64, align 8
  call void @make_triple(ptr dead_on_unwind writable sret(%struct.Triple) align 8 %value)
  %call = call i64 @sum_triple(ptr noundef byval(%struct.Triple) align 8 %value)
  store i64 %call, ptr %mutated, align 8
  store i64 1, ptr %target, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %pointer_value, i8 0, i64 24, i1 false)
  %target1 = getelementptr inbounds nuw %struct.PointerTriple, ptr %pointer_value, i32 0, i32 0
  store ptr %target, ptr %target1, align 8
  %call2 = call i64 @mutate_pointer_triple(ptr noundef byval(%struct.PointerTriple) align 8 %pointer_value)
  store i64 %call2, ptr %pointer_result, align 8
  %0 = load i64, ptr %mutated, align 8
  %cmp = icmp ne i64 %0, 17
  br i1 %cmp, label %if.then, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %a = getelementptr inbounds nuw %struct.Triple, ptr %value, i32 0, i32 0
  %1 = load i64, ptr %a, align 8
  %cmp3 = icmp ne i64 %1, 7
  br i1 %cmp3, label %if.then, label %lor.lhs.false4

lor.lhs.false4:                                   ; preds = %lor.lhs.false
  %2 = load i64, ptr %pointer_result, align 8
  %cmp5 = icmp ne i64 %2, 2
  br i1 %cmp5, label %if.then, label %lor.lhs.false6

lor.lhs.false6:                                   ; preds = %lor.lhs.false4
  %3 = load i64, ptr %target, align 8
  %cmp7 = icmp ne i64 %3, 31
  br i1 %cmp7, label %if.then, label %lor.lhs.false8

lor.lhs.false8:                                   ; preds = %lor.lhs.false6
  %target9 = getelementptr inbounds nuw %struct.PointerTriple, ptr %pointer_value, i32 0, i32 0
  %4 = load ptr, ptr %target9, align 8
  %cmp10 = icmp ne ptr %4, %target
  br i1 %cmp10, label %if.then, label %if.end

if.then:                                          ; preds = %lor.lhs.false8, %lor.lhs.false6, %lor.lhs.false4, %lor.lhs.false, %entry
  store i64 0, ptr %retval, align 8
  br label %return

if.end:                                           ; preds = %lor.lhs.false8
  store i64 24, ptr %retval, align 8
  br label %return

return:                                           ; preds = %if.end, %if.then
  %5 = load i64, ptr %retval, align 8
  ret i64 %5
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #2

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: write) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 20.0.0git (https://github.com/llvm/llvm-project.git eccf4d44d346eee498b0ff709e625e3104448751)"}
