; RUN: %scalar-executor %s --entry icmps | %FileCheck %s --check-prefix=ICMPS
; RUN: %scalar-executor %s --entry overflow | %FileCheck %s --check-prefix=OVERFLOW
; RUN: %scalar-executor %s --entry casts | %FileCheck %s --check-prefix=CASTS
; RUN: %scalar-executor %s --entry integer_bitcast | %FileCheck %s --check-prefix=BITCAST
; RUN: %scalar-executor %s --entry shifts | %FileCheck %s --check-prefix=SHIFTS
; RUN: %scalar-executor %s --entry divisions | %FileCheck %s --check-prefix=DIVS
; RUN: %scalar-executor %s --entry width_i1 | %FileCheck %s --check-prefix=I1
; RUN: %scalar-executor %s --entry width_i24 | %FileCheck %s --check-prefix=I24
; RUN: %scalar-executor %s --entry width_i65 | %FileCheck %s --check-prefix=I65
; RUN: %scalar-executor %s --entry width_i128 | %FileCheck %s --check-prefix=I128
; RUN: %scalar-executor %s --entry select_value | %FileCheck %s --check-prefix=SELECT

define i16 @icmps() {
entry:
  %eq = icmp eq i8 -1, 1
  %ne = icmp ne i8 -1, 1
  %ult = icmp ult i8 -1, 1
  %ule = icmp ule i8 -1, 1
  %ugt = icmp ugt i8 -1, 1
  %uge = icmp uge i8 -1, 1
  %slt = icmp slt i8 -1, 1
  %sle = icmp sle i8 -1, 1
  %sgt = icmp sgt i8 -1, 1
  %sge = icmp sge i8 -1, 1
  %b0 = zext i1 %eq to i16
  %b1z = zext i1 %ne to i16
  %b1 = shl i16 %b1z, 1
  %b2z = zext i1 %ult to i16
  %b2 = shl i16 %b2z, 2
  %b3z = zext i1 %ule to i16
  %b3 = shl i16 %b3z, 3
  %b4z = zext i1 %ugt to i16
  %b4 = shl i16 %b4z, 4
  %b5z = zext i1 %uge to i16
  %b5 = shl i16 %b5z, 5
  %b6z = zext i1 %slt to i16
  %b6 = shl i16 %b6z, 6
  %b7z = zext i1 %sle to i16
  %b7 = shl i16 %b7z, 7
  %b8z = zext i1 %sgt to i16
  %b8 = shl i16 %b8z, 8
  %b9z = zext i1 %sge to i16
  %b9 = shl i16 %b9z, 9
  %x0 = or i16 %b0, %b1
  %x1 = or i16 %x0, %b2
  %x2 = or i16 %x1, %b3
  %x3 = or i16 %x2, %b4
  %x4 = or i16 %x3, %b5
  %x5 = or i16 %x4, %b6
  %x6 = or i16 %x5, %b7
  %x7 = or i16 %x6, %b8
  %x8 = or i16 %x7, %b9
  ret i16 %x8
}
; ICMPS: M3 status M3_EXECUTOR_COMPLETE
; ICMPS: return=bv16:242

define i8 @overflow() {
entry:
  %a = add i8 255, 1
  %s = sub i8 0, 1
  %m = mul i8 16, 16
  %x = xor i8 %a, %s
  %r = xor i8 %x, %m
  ret i8 %r
}
; OVERFLOW: return=bv8:255

define i8 @casts() {
entry:
  %t = trunc i24 16776961 to i8
  %z = zext i8 %t to i32
  %s = sext i8 %t to i32
  %x = xor i32 %z, %s
  %r = trunc i32 %x to i8
  ret i8 %r
}
; CASTS: return=bv8:0

define i8 @integer_bitcast(i8 %x) {
entry:
  %r = bitcast i8 %x to i8
  ret i8 %r
}
; BITCAST: M3 status M3_EXECUTOR_COMPLETE
; BITCAST: return=bv8:symbolic

define i8 @shifts() {
entry:
  %a = shl i8 3, 2
  %b = lshr i8 128, 2
  %c = ashr i8 128, 2
  %x = xor i8 %a, %b
  %r = xor i8 %x, %c
  ret i8 %r
}
; SHIFTS: return=bv8:204

define i8 @divisions() {
entry:
  %a = udiv i8 250, 10
  %b = sdiv i8 -100, 10
  %c = urem i8 250, 10
  %d = srem i8 -101, 10
  %x = xor i8 %a, %b
  %y = xor i8 %c, %d
  %r = xor i8 %x, %y
  ret i8 %r
}
; DIVS: return=bv8:16

define i1 @width_i1(i1 %x) {
entry:
  %r = xor i1 %x, true
  ret i1 %r
}
; I1: return=bv1:symbolic

define i24 @width_i24(i24 %x) {
entry:
  %r = add i24 %x, 1
  ret i24 %r
}
; I24: return=bv24:symbolic

define i65 @width_i65(i65 %x) {
entry:
  %r = sub i65 %x, 1
  ret i65 %r
}
; I65: return=bv65:symbolic

define i128 @width_i128(i128 %x) {
entry:
  %r = mul i128 %x, 3
  ret i128 %r
}
; I128: return=bv128:symbolic

define i32 @select_value(i1 %c, i32 %x, i32 %y) {
entry:
  %r = select i1 %c, i32 %x, i32 %y
  ret i32 %r
}
; SELECT: M3 status M3_EXECUTOR_COMPLETE
; SELECT: M3 paths 1
; SELECT: return=bv32:symbolic
; SELECT: M3 stats states=1 forks=0 pruned=0 instructions=2 queries=0
