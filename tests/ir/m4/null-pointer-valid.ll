; RUN: %scalar-executor %s --m4 --entry valid_load | %FileCheck %s --check-prefix=VALID-LOAD
; RUN: %scalar-executor %s --m4 --entry valid_store | %FileCheck %s --check-prefix=VALID-STORE
; RUN: %scalar-executor %s --m4 --entry valid_memcpy | %FileCheck %s --check-prefix=VALID-COPY
; RUN: %scalar-executor %s --m4 --entry ordinary_load | %FileCheck %s --check-prefix=ORDINARY

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define i8 @valid_load() #0 {
entry:
  %value = load i8, ptr null, align 1
  ret i8 %value
}
; VALID-LOAD: M4 status UNKNOWN
; VALID-LOAD: reason=POINTER_PROVENANCE_LOST

define void @valid_store() #0 {
entry:
  store i8 1, ptr null, align 1
  ret void
}
; VALID-STORE: M4 status UNKNOWN
; VALID-STORE: reason=POINTER_PROVENANCE_LOST

define void @valid_memcpy() #0 {
entry:
  %source = alloca i8, align 1
  store i8 1, ptr %source, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr null, ptr %source, i64 1, i1 false)
  ret void
}
; VALID-COPY: M4 status UNKNOWN
; VALID-COPY: reason=POINTER_PROVENANCE_LOST

define i8 @ordinary_load() {
entry:
  %value = load i8, ptr null, align 1
  ret i8 %value
}
; ORDINARY: M4 status ERROR
; ORDINARY: reason=NULL_DEREFERENCE

attributes #0 = { null_pointer_is_valid }
