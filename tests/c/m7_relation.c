#include "formal_api.h"

#include <stdint.h>

__attribute__((noinline)) static uint8_t add_one(uint8_t value) {
  return (uint8_t)(value + 1u);
}

__attribute__((noinline)) static void write_result(uint8_t *output,
                                                   uint8_t input) {
  *output = add_one(input);
}

void m7_relation_top(void) {
  uint8_t x;
  uint8_t y;
  FORMAL_INPUT(x);
  FORMAL_OUTPUT(y);
  __formal_assume(x < 4);
  write_result(&y, x);
}
