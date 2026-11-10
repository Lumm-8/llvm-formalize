; Generated from tests/c/m7_new_delete.cpp by pinned clang-20 -O0
; -fno-exceptions -S -emit-llvm.
; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry _Z16new_delete_entryv | %FileCheck %s --check-prefix=NEW
; NEW: M4 status UNKNOWN
; NEW-DAG: reason=UNKNOWN-UNSUPPORTED-EXCEPTION-INSTRUCTION
; NEW-DAG: return=bv32:3
; ModuleID = 'tests/c/m7_new_delete.cpp'
source_filename = "tests/c/m7_new_delete.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local noundef i32 @_Z16new_delete_entryv() #0 {
entry:
  %value = alloca ptr, align 8
  %result = alloca i32, align 4
  %call = call noalias noundef nonnull ptr @_Znwm(i64 noundef 4) #3
  store i32 3, ptr %call, align 4
  store ptr %call, ptr %value, align 8
  %0 = load ptr, ptr %value, align 8
  %1 = load i32, ptr %0, align 4
  store i32 %1, ptr %result, align 4
  %2 = load ptr, ptr %value, align 8
  %isnull = icmp eq ptr %2, null
  br i1 %isnull, label %delete.end, label %delete.notnull

delete.notnull:                                   ; preds = %entry
  call void @_ZdlPvm(ptr noundef %2, i64 noundef 4) #4
  br label %delete.end

delete.end:                                       ; preds = %delete.notnull, %entry
  %3 = load i32, ptr %result, align 4
  ret i32 %3
}

; Function Attrs: nobuiltin allocsize(0)
declare noundef nonnull ptr @_Znwm(i64 noundef) #1

; Function Attrs: nobuiltin nounwind
declare void @_ZdlPvm(ptr noundef, i64 noundef) #2

attributes #0 = { mustprogress noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nobuiltin allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nobuiltin nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { builtin allocsize(0) }
attributes #4 = { builtin nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 20.0.0git (https://github.com/llvm/llvm-project.git eccf4d44d346eee498b0ff709e625e3104448751)"}
