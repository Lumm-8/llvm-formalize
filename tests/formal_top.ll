target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
entry:
  ret void
}

define i32 @not_void() {
entry:
  ret i32 0
}

define void @has_argument(i32 %value) {
entry:
  ret void
}

define void @variadic(...) {
entry:
  ret void
}

declare void @declaration_only()

@atomic_value = global i32 0

define void @unsupported_atomic() {
entry:
  atomicrmw add ptr @atomic_value, i32 1 seq_cst
  ret void
}
