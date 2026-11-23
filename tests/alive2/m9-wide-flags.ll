; Local refinement check for M9's widened add and lost-bit checks.
; The arguments are defined, matching the executor's registered input bytes.
define i128 @src(i128 noundef %a, i128 noundef %b) {
  %sum = add nsw i128 %a, %b
  %shifted = lshr exact i128 %sum, 7
  ret i128 %shifted
}

define i128 @tgt(i128 noundef %a, i128 noundef %b) {
  %wide.a = sext i128 %a to i129
  %wide.b = sext i128 %b to i129
  %wide.sum = add i129 %wide.a, %wide.b
  %sum = trunc i129 %wide.sum to i128
  %roundtrip = sext i128 %sum to i129
  %no.overflow = icmp eq i129 %wide.sum, %roundtrip
  %lost = and i128 %sum, 127
  %exact = icmp eq i128 %lost, 0
  %defined = and i1 %no.overflow, %exact
  %shifted = lshr i128 %sum, 7
  %result = select i1 %defined, i128 %shifted, i128 poison
  ret i128 %result
}
