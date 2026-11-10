; RUN: %scalar-executor %s --m4 --entry first_field_byte | %FileCheck %s --check-prefix=FIRST
; RUN: %scalar-executor %s --m4 --entry extracted_field | %FileCheck %s --check-prefix=EXTRACT

target datalayout = "E-p:64:64-i64:64-n8:16:32:64-S128"
%U = type { i8, i32 }

define i8 @first_field_byte() {
  %slot = alloca %U, align 4
  store %U { i8 7, i32 287454020 }, ptr %slot, align 4
  %field = getelementptr %U, ptr %slot, i64 0, i32 1
  %first = load i8, ptr %field, align 1
  ret i8 %first
}
; FIRST: return=bv8:17

define i32 @extracted_field() {
  %slot = alloca %U, align 4
  store %U { i8 7, i32 287454020 }, ptr %slot, align 4
  %value = load %U, ptr %slot, align 4
  %field = extractvalue %U %value, 1
  ret i32 %field
}
; EXTRACT: return=bv32:287454020
