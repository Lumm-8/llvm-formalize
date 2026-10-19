; RUN: %scalar-executor %s --m4 --entry widths | %FileCheck %s --check-prefix=WIDTHS
; RUN: %scalar-executor %s --m4 --entry byte_order | %FileCheck %s --check-prefix=BYTES
; RUN: %scalar-executor %s --m4 --entry nested_array | %FileCheck %s --check-prefix=NESTED
; RUN: %scalar-executor %s --m4 --entry struct_padding | %FileCheck %s --check-prefix=PADDING
; RUN: %scalar-executor %s --m4 --entry negative_back | %FileCheck %s --check-prefix=NEGATIVE
; RUN: %scalar-executor %s --m4 --entry negative_bad | %FileCheck %s --check-prefix=UNDERFLOW
; RUN: %scalar-executor %s --m4 --entry zero_size | %FileCheck %s --check-prefix=ZERO
; RUN: %scalar-executor %s --m4 --entry misaligned | %FileCheck %s --check-prefix=MISALIGNED
; RUN: %scalar-executor %s --m4 --entry pointer_roundtrip | %FileCheck %s --check-prefix=POINTER
; RUN: %scalar-executor %s --m4 --entry shadow_invalidated | %FileCheck %s --check-prefix=SHADOW
; RUN: %scalar-executor %s --m4 --entry null_roundtrip | %FileCheck %s --check-prefix=NULL
; RUN: %scalar-executor %s --m4 --entry symbolic_alloca | %FileCheck %s --check-prefix=SYMBOLIC
; RUN: %scalar-executor %s --m4 --entry inbounds_good | %FileCheck %s --check-prefix=INBOUNDS
; RUN: %scalar-executor %s --m4 --entry inbounds_bad | %FileCheck %s --check-prefix=INBOUNDSBAD
; RUN: %scalar-executor %s --m4 --entry partial_byte | %FileCheck %s --check-prefix=PARTIAL
; RUN: %scalar-executor %s --m4 --entry same_object_alias | %FileCheck %s --check-prefix=ALIAS
; RUN: %scalar-executor %s --m4 --entry crosses_end | %FileCheck %s --check-prefix=CROSS
; RUN: %scalar-executor %s --m4 --entry offset_plus_size_overflow | %FileCheck %s --check-prefix=OVERFLOW
; RUN: %scalar-executor %s --m4 --entry gep_multiply_overflow | %FileCheck %s --check-prefix=GEPOVERFLOW
; RUN: %scalar-executor %s --m4 --entry pointer_eq_ne | %FileCheck %s --check-prefix=POINTERCMP
; RUN: %scalar-executor %s --m4 --entry onepast_cross_compare | %FileCheck %s --check-prefix=CROSSCMP
; RUN: %scalar-executor %s --m4 --entry underaligned_base | %FileCheck %s --check-prefix=UNDERALIGNED

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"

%Padded = type { i8, i32 }

define i128 @widths() {
entry:
  %a = alloca i1, align 1
  %b = alloca i8, align 1
  %c = alloca i16, align 2
  %d = alloca i24, align 4
  %e = alloca i32, align 4
  %f = alloca i64, align 8
  %g = alloca i65, align 8
  %h = alloca i128, align 16
  store i1 true, ptr %a, align 1
  store i8 7, ptr %b, align 1
  store i16 9, ptr %c, align 2
  store i24 11, ptr %d, align 4
  store i32 13, ptr %e, align 4
  store i64 15, ptr %f, align 8
  store i65 17, ptr %g, align 8
  store i128 123456789012345678901234567890, ptr %h, align 16
  %v1 = load i1, ptr %a, align 1
  %v8 = load i8, ptr %b, align 1
  %v16 = load i16, ptr %c, align 2
  %v24 = load i24, ptr %d, align 4
  %v32 = load i32, ptr %e, align 4
  %v64 = load i64, ptr %f, align 8
  %v65 = load i65, ptr %g, align 8
  %v128 = load i128, ptr %h, align 16
  %ok1 = icmp eq i1 %v1, true
  %ok8 = icmp eq i8 %v8, 7
  %ok16 = icmp eq i16 %v16, 9
  %ok24 = icmp eq i24 %v24, 11
  %ok32 = icmp eq i32 %v32, 13
  %ok64 = icmp eq i64 %v64, 15
  %ok65 = icmp eq i65 %v65, 17
  %ok128 = icmp eq i128 %v128, 123456789012345678901234567890
  %q1 = and i1 %ok1, %ok8
  %q2 = and i1 %q1, %ok16
  %q3 = and i1 %q2, %ok24
  %q4 = and i1 %q3, %ok32
  %q5 = and i1 %q4, %ok64
  %q6 = and i1 %q5, %ok65
  %q7 = and i1 %q6, %ok128
  %result = zext i1 %q7 to i128
  ret i128 %result
}
; WIDTHS: return=bv128:1

define i8 @byte_order() {
entry:
  %slot = alloca i32, align 4
  store i32 16909060, ptr %slot, align 4
  %byte = load i8, ptr %slot, align 1
  ret i8 %byte
}
; BYTES: return=bv8:4

define i16 @nested_array() {
entry:
  %array = alloca [2 x [3 x i16]], align 2
  %element = getelementptr [2 x [3 x i16]], ptr %array, i64 0, i64 1, i64 2
  store i16 4660, ptr %element, align 2
  %value = load i16, ptr %element, align 2
  ret i16 %value
}
; NESTED: return=bv16:4660

define i8 @struct_padding() {
entry:
  %record = alloca %Padded, align 4
  %field = getelementptr %Padded, ptr %record, i64 0, i32 1
  store i32 287454020, ptr %field, align 4
  %byte = load i8, ptr %field, align 1
  ret i8 %byte
}
; PADDING: return=bv8:68

define i8 @negative_back() {
entry:
  %slot = alloca i8, align 1
  %past = getelementptr i8, ptr %slot, i64 1
  %back = getelementptr i8, ptr %past, i64 -1
  store i8 42, ptr %back, align 1
  %value = load i8, ptr %slot, align 1
  ret i8 %value
}
; NEGATIVE: return=bv8:42

define i8 @negative_bad() {
entry:
  %slot = alloca i8, align 1
  %before = getelementptr i8, ptr %slot, i64 -1
  %value = load i8, ptr %before, align 1
  ret i8 %value
}
; UNDERFLOW: reason=OUT_OF_BOUNDS

define i8 @zero_size() {
entry:
  %empty = alloca [0 x i8], align 1
  %value = load i8, ptr %empty, align 1
  ret i8 %value
}
; ZERO: reason=OUT_OF_BOUNDS

define i16 @misaligned() {
entry:
  %slot = alloca i32, align 4
  %odd = getelementptr i8, ptr %slot, i64 1
  %value = load i16, ptr %odd, align 2
  ret i16 %value
}
; MISALIGNED: reason=MISALIGNED_ACCESS

define i8 @pointer_roundtrip() {
entry:
  %target = alloca i8, align 1
  %slot = alloca ptr, align 8
  store ptr %target, ptr %slot, align 8
  %loaded = load ptr, ptr %slot, align 8
  store i8 71, ptr %loaded, align 1
  %value = load i8, ptr %target, align 1
  ret i8 %value
}
; POINTER: return=bv8:71

define i8 @shadow_invalidated() {
entry:
  %target = alloca i8, align 1
  %slot = alloca ptr, align 8
  store ptr %target, ptr %slot, align 8
  store i8 0, ptr %slot, align 1
  %loaded = load ptr, ptr %slot, align 8
  %value = load i8, ptr %loaded, align 1
  ret i8 %value
}
; SHADOW: reason=POINTER_PROVENANCE_LOST

define i8 @null_roundtrip() {
entry:
  %slot = alloca ptr, align 8
  store ptr null, ptr %slot, align 8
  %loaded = load ptr, ptr %slot, align 8
  %value = load i8, ptr %loaded, align 1
  ret i8 %value
}
; NULL: reason=NULL_DEREFERENCE

define void @symbolic_alloca(i8 %count) {
entry:
  %slot = alloca i8, i8 %count, align 1
  ret void
}
; SYMBOLIC: reason=SYMBOLIC_ALLOCATION_SIZE

define void @inbounds_good() {
entry:
  %slot = alloca [4 x i8], align 1
  %end = getelementptr inbounds [4 x i8], ptr %slot, i64 0, i64 4
  ret void
}
; INBOUNDS: M4 status M4_EXECUTOR_COMPLETE

define void @inbounds_bad() {
entry:
  %slot = alloca [4 x i8], align 1
  %bad = getelementptr inbounds [4 x i8], ptr %slot, i64 0, i64 5
  ret void
}
; INBOUNDSBAD: reason=UNKNOWN-INBOUNDS-GEP

define i8 @partial_byte() {
entry:
  %slot = alloca i1, align 1
  store i1 true, ptr %slot, align 1
  %wide = load i8, ptr %slot, align 1
  ret i8 %wide
}
; PARTIAL: reason=UNINITIALIZED_READ

define i8 @same_object_alias() {
entry:
  %slot = alloca i8, align 1
  %alias = getelementptr i8, ptr %slot, i64 0
  store i8 77, ptr %alias, align 1
  %value = load i8, ptr %slot, align 1
  ret i8 %value
}
; ALIAS: return=bv8:77

define i16 @crosses_end() {
entry:
  %a = alloca i8, align 1
  %b = alloca i8, align 1
  store i8 1, ptr %a, align 1
  store i8 2, ptr %b, align 1
  %v = load i16, ptr %a, align 1
  ret i16 %v
}
; CROSS: reason=OUT_OF_BOUNDS

define i16 @offset_plus_size_overflow() {
entry:
  %slot = alloca i8, align 1
  %far = getelementptr i8, ptr %slot, i64 9223372036854775807
  %v = load i16, ptr %far, align 1
  ret i16 %v
}
; OVERFLOW: reason=OUT_OF_BOUNDS

define void @gep_multiply_overflow() {
entry:
  %slot = alloca i64, align 8
  %far = getelementptr i64, ptr %slot, i64 9223372036854775807
  ret void
}
; GEPOVERFLOW: reason=UNKNOWN-INBOUNDS-GEP

define i1 @pointer_eq_ne() {
entry:
  %a = alloca i8, align 1
  %b = alloca i8, align 1
  %alias = getelementptr i8, ptr %a, i64 0
  %eq = icmp eq ptr %a, %alias
  %ne = icmp ne ptr %a, %b
  %result = and i1 %eq, %ne
  ret i1 %result
}
; POINTERCMP: return=bv1:1

define i1 @onepast_cross_compare() {
entry:
  %a = alloca i8, align 1
  %b = alloca i8, align 1
  %past = getelementptr i8, ptr %a, i64 1
  %equal = icmp eq ptr %past, %b
  ret i1 %equal
}
; CROSSCMP: reason=UNKNOWN-POINTER-COMPARISON

define i32 @underaligned_base() {
entry:
  %slot = alloca i32, align 1
  %v = load i32, ptr %slot, align 4
  ret i32 %v
}
; UNDERALIGNED: reason=UNKNOWN-ALIGNMENT-UNDETERMINED
