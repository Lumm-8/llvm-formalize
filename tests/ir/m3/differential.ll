target triple = "x86_64-unknown-linux-gnu"

define i8 @diff_signed_unsigned(i8 %x) {
entry:
  %s = icmp slt i8 %x, 0
  br i1 %s, label %negative, label %nonnegative
negative:
  %a = ashr i8 %x, 1
  ret i8 %a
nonnegative:
  %u = icmp ugt i8 %x, 100
  %r = select i1 %u, i8 7, i8 3
  ret i8 %r
}

define i8 @diff_switch(i8 %x) {
entry:
  switch i8 %x, label %default [
    i8 0, label %zero
    i8 127, label %high
    i8 -1, label %ones
  ]
zero:
  ret i8 11
high:
  ret i8 22
ones:
  ret i8 33
default:
  %r = xor i8 %x, 90
  ret i8 %r
}

define i8 @diff_casts(i8 %x) {
entry:
  %t = trunc i8 %x to i4
  %s = sext i4 %t to i8
  %z = zext i4 %t to i8
  %r = xor i8 %s, %z
  ret i8 %r
}

define i8 @diff_two_i8(i8 %x, i8 %y) {
entry:
  %sum = add i8 %x, %y
  %c = icmp ult i8 %x, %y
  %r = select i1 %c, i8 %sum, i8 %x
  ret i8 %r
}
