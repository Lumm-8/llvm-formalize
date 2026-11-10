; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry alias_entry | %FileCheck %s --check-prefix=ALIAS
; RUN: %scalar-executor %s --m4 --entry chained_alias_entry | %FileCheck %s --check-prefix=CHAIN
; RUN: %scalar-executor %s --m4 --entry alias_side_effect | %FileCheck %s --check-prefix=SIDE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@increment_alias = alias i32 (i32), ptr @increment
@chained_alias = alias i32 (i32), ptr @increment_alias
@mutate_alias = alias void (ptr), ptr @mutate

define i32 @increment(i32 %value) {
  %result = add i32 %value, 1
  ret i32 %result
}

define i32 @alias_entry() {
  %result = call i32 @increment_alias(i32 41)
  ret i32 %result
}
; ALIAS: M4 status M4_EXECUTOR_COMPLETE
; ALIAS: return=bv32:42

define i32 @chained_alias_entry() {
  %result = call i32 @chained_alias(i32 41)
  ret i32 %result
}
; CHAIN: M4 status M4_EXECUTOR_COMPLETE
; CHAIN: return=bv32:42

define void @mutate(ptr %destination) {
  store i32 9, ptr %destination, align 4
  ret void
}

define i32 @alias_side_effect() {
  %slot = alloca i32, align 4
  call void @mutate_alias(ptr %slot)
  %result = load i32, ptr %slot, align 4
  ret i32 %result
}
; SIDE: M4 status M4_EXECUTOR_COMPLETE
; SIDE: return=bv32:9
