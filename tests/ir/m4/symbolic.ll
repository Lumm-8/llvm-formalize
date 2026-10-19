; RUN: %scalar-executor %s --m4 --entry symbolic_index | %FileCheck %s --check-prefix=INDEX
; RUN: %scalar-executor %s --m4 --entry pointer_select | %FileCheck %s --check-prefix=SELECT
; RUN: %scalar-executor %s --m4 --entry nullable_select | %FileCheck %s --check-prefix=NULLABLE
; RUN: %scalar-executor %s --m4 --entry branch_cow | %FileCheck %s --check-prefix=COW
; RUN: %scalar-executor %s --m4 --entry pointer_select --timeout-ms 0 | %FileCheck %s --check-prefix=TIMEOUT

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

define i8 @symbolic_index(i8 %index) {
entry:
  %array = alloca [4 x i8], align 1
  %p0 = getelementptr [4 x i8], ptr %array, i64 0, i64 0
  %p1 = getelementptr [4 x i8], ptr %array, i64 0, i64 1
  %p2 = getelementptr [4 x i8], ptr %array, i64 0, i64 2
  %p3 = getelementptr [4 x i8], ptr %array, i64 0, i64 3
  store i8 10, ptr %p0, align 1
  store i8 11, ptr %p1, align 1
  store i8 12, ptr %p2, align 1
  store i8 13, ptr %p3, align 1
  %p = getelementptr [4 x i8], ptr %array, i64 0, i8 %index
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; INDEX: M4 status ERROR
; INDEX: reason=OUT_OF_BOUNDS

define i8 @pointer_select(i1 %choose) {
entry:
  %a = alloca i8, align 1
  %b = alloca i8, align 1
  store i8 4, ptr %a, align 1
  store i8 5, ptr %b, align 1
  %p = select i1 %choose, ptr %a, ptr %b
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; SELECT: M4 status M4_EXECUTOR_COMPLETE
; SELECT: M4 paths 2
; SELECT: return=bv8:4
; SELECT: return=bv8:5

define i8 @nullable_select(i1 %choose) {
entry:
  %a = alloca i8, align 1
  store i8 4, ptr %a, align 1
  %p = select i1 %choose, ptr %a, ptr null
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; NULLABLE: M4 status ERROR
; NULLABLE: reason=NULL_DEREFERENCE

define i8 @branch_cow(i1 %choose) {
entry:
  %slot = alloca i8, align 1
  br i1 %choose, label %yes, label %no
yes:
  store i8 1, ptr %slot, align 1
  br label %done
no:
  store i8 2, ptr %slot, align 1
  br label %done
done:
  %v = load i8, ptr %slot, align 1
  ret i8 %v
}
; COW: M4 status M4_EXECUTOR_COMPLETE
; COW: return=bv8:1
; COW: return=bv8:2

; TIMEOUT: M4 status UNKNOWN
; TIMEOUT: reason=UNKNOWN-SOLVER-TIMEOUT
