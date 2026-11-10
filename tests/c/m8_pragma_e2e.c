void formal_top(void) {
  unsigned sink = 0;
#pragma clang loop unroll_count(3)
  for (unsigned i = 0; i < 10; ++i)
    sink += i;
  (void)sink;
}
