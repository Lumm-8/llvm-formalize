; RUN: %scalar-executor %s --m4 --entry external_global | %FileCheck %s --check-prefix=EXTERNAL
; RUN: %scalar-executor %s --m4 --entry alias_global | %FileCheck %s --check-prefix=ALIAS
; RUN: %scalar-executor %s --m4 --entry aggregate_global | %FileCheck %s --check-prefix=AGGREGATE

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

@external = external global i32
@target = global i32 5
@aliased = alias i32, ptr @target
@aggregate = global { i8, i8 } { i8 1, i8 2 }

define i32 @external_global() {
entry:
  %value = load i32, ptr @external, align 4
  ret i32 %value
}
; EXTERNAL: reason=UNKNOWN-UNSUPPORTED-GLOBAL

define i32 @alias_global() {
entry:
  %value = load i32, ptr @aliased, align 4
  ret i32 %value
}
; ALIAS: reason=UNKNOWN-UNSUPPORTED-GLOBAL

define i8 @aggregate_global() {
entry:
  %field = getelementptr { i8, i8 }, ptr @aggregate, i64 0, i32 1
  %value = load i8, ptr %field, align 1
  ret i8 %value
}
; AGGREGATE: return=bv8:2
