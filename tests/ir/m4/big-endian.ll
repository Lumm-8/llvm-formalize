; RUN: %scalar-executor %s --m4 --entry big_i32 | %FileCheck %s --check-prefix=I32
; RUN: %scalar-executor %s --m4 --entry big_i24 | %FileCheck %s --check-prefix=I24

target datalayout = "E-p:64:64-i64:64-n8:16:32:64-S128"

define i8 @big_i32() {
entry:
  %slot = alloca i32, align 4
  store i32 16909060, ptr %slot, align 4
  %high = load i8, ptr %slot, align 1
  ret i8 %high
}
; I32: return=bv8:1

define i8 @big_i24() {
entry:
  %slot = alloca i24, align 4
  store i24 66051, ptr %slot, align 4
  %high = load i8, ptr %slot, align 1
  ret i8 %high
}
; I24: return=bv8:1
