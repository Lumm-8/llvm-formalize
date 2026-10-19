; RUN: %opt -passes=verify -disable-output %s
; RUN: %scalar-executor %s --m4 --entry zero_noundef_nonnull_dest | %FileCheck %s --check-prefix=BAD-DEST
; RUN: %scalar-executor %s --m4 --entry zero_noundef_nonnull_source | %FileCheck %s --check-prefix=BAD-SOURCE
; RUN: %scalar-executor %s --m4 --entry zero_nonnull_only | %FileCheck %s --check-prefix=NONNULL
; RUN: %scalar-executor %s --m4 --entry zero_dereferenceable | %FileCheck %s --check-prefix=DEREF
; RUN: %scalar-executor %s --m4 --entry zero_noundef_only | %FileCheck %s --check-prefix=NOUNDEF
; RUN: %scalar-executor %s --m4 --entry zero_noundef_undef | %FileCheck %s --check-prefix=UNDEF
; RUN: %scalar-executor %s --m4 --entry zero_noundef_poison | %FileCheck %s --check-prefix=POISON
; RUN: %scalar-executor %s --m4 --entry zero_aligned_null | %FileCheck %s --check-prefix=ALIGNED
; RUN: %scalar-executor %s --m4 --entry zero_misaligned_pointer | %FileCheck %s --check-prefix=MISALIGNED
; RUN: %scalar-executor %s --m4 --entry nonzero_noundef_nonnull | %FileCheck %s --check-prefix=NONZERO

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define void @zero_noundef_nonnull_dest() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull null, ptr null, i64 0, i1 false)
  ret void
}
; BAD-DEST: M4 status UNKNOWN
; BAD-DEST: reason=UNKNOWN-POISON-ANNOTATION

define void @zero_noundef_nonnull_source() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr null, ptr noundef nonnull null, i64 0, i1 false)
  ret void
}
; BAD-SOURCE: M4 status UNKNOWN
; BAD-SOURCE: reason=UNKNOWN-POISON-ANNOTATION

define void @zero_nonnull_only() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr nonnull null, ptr null, i64 0, i1 false)
  ret void
}
; NONNULL: M4 status UNKNOWN
; NONNULL: reason=UNKNOWN-POISON-ANNOTATION

define void @zero_dereferenceable() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr noundef dereferenceable(1) null, ptr null, i64 0, i1 false)
  ret void
}
; DEREF: M4 status UNKNOWN
; DEREF: reason=UNKNOWN-POISON-ANNOTATION

define void @zero_noundef_only() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr noundef null, ptr null, i64 0, i1 false)
  ret void
}
; NOUNDEF: M4 status M4_EXECUTOR_COMPLETE

define void @zero_noundef_undef() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr noundef undef, ptr null, i64 0, i1 false)
  ret void
}
; UNDEF: M4 status UNKNOWN
; UNDEF: reason=UNKNOWN-UNDEF

define void @zero_noundef_poison() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr noundef poison, ptr null, i64 0, i1 false)
  ret void
}
; POISON: M4 status UNKNOWN
; POISON: reason=UNKNOWN-POISON

define void @zero_aligned_null() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 null, ptr align 8 null, i64 0, i1 false)
  ret void
}
; ALIGNED: M4 status M4_EXECUTOR_COMPLETE

define void @zero_misaligned_pointer() {
entry:
  %offset = getelementptr i8, ptr null, i64 1
  call void @llvm.memcpy.p0.p0.i64(ptr noundef align 8 %offset, ptr null, i64 0, i1 false)
  ret void
}
; MISALIGNED: M4 status UNKNOWN
; MISALIGNED: reason=UNKNOWN-POISON-ANNOTATION

define void @nonzero_noundef_nonnull() {
entry:
  %source = alloca i8, align 1
  store i8 1, ptr %source, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull null, ptr %source, i64 1, i1 false)
  ret void
}
; NONZERO: M4 status UNKNOWN
; NONZERO: reason=UNKNOWN-POISON-ANNOTATION
