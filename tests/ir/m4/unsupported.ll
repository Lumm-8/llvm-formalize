; RUN: %scalar-executor %s --m4 --entry uninitialized | %FileCheck %s --check-prefix=UNINIT
; RUN: %scalar-executor %s --m4 --entry addrspace_alloca | %FileCheck %s --check-prefix=ADDRSPACE
; RUN: %scalar-executor %s --m4 --entry scalable_alloca | %FileCheck %s --check-prefix=SCALABLE
; RUN: %scalar-executor %s --m4 --entry cast_pointer | %FileCheck %s --check-prefix=INTTOPTR
; RUN: %scalar-executor %s --m4 --entry bulk_mem | %FileCheck %s --check-prefix=BULK
; RUN: %scalar-executor %s --m4 --entry ordinary_call | %FileCheck %s --check-prefix=CALL

target datalayout = "e-p:64:64-p1:64:64-i64:64-n8:16:32:64-S128"

declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare i8 @callee()

define i8 @uninitialized() {
entry:
  %slot = alloca i8, align 1
  %v = load i8, ptr %slot, align 1
  ret i8 %v
}
; UNINIT: reason=UNINITIALIZED_READ

define void @addrspace_alloca() {
entry:
  %slot = alloca i8, addrspace(1)
  ret void
}
; ADDRSPACE: reason=UNSUPPORTED_ADDRESS_SPACE

define void @scalable_alloca() {
entry:
  %slot = alloca <vscale x 4 x i32>
  ret void
}
; SCALABLE: reason=UNKNOWN-SCALABLE-TYPE

define i8 @cast_pointer() {
entry:
  %p = inttoptr i64 65536 to ptr
  %v = load i8, ptr %p, align 1
  ret i8 %v
}
; INTTOPTR: reason=POINTER_PROVENANCE_LOST

define void @bulk_mem() {
entry:
  %slot = alloca i8, align 1
  call void @llvm.memset.p0.i64(ptr %slot, i8 0, i64 1, i1 false)
  ret void
}
; BULK: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION

define i8 @ordinary_call() {
entry:
  %value = call i8 @callee()
  ret i8 %value
}
; CALL: reason=UNKNOWN-UNSUPPORTED-CALL
