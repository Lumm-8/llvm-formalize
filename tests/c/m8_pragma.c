void m8_annotated_loop(unsigned n, unsigned *output) {
#pragma clang loop unroll_count(3)
  for (unsigned i = 0; i < n; ++i)
    output[i] = i;
}
