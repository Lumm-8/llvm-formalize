#include "formal_api.h"
#include <stdint.h>
#include <stdio.h>

static uint8_t next_input;
static uint8_t output;

void __formal_register_input(void *addr, size_t size, const char *name) {
  if (size != 1 || name[0] != 'x' || name[1] != '\0')
    __builtin_trap();
  *(uint8_t *)addr = next_input;
}

void __formal_register_output(void *addr, size_t size, const char *name) {
  if (addr != &output || size != 1 || name[0] != 'y' || name[1] != '\0')
    __builtin_trap();
}

void __formal_assume(int condition) {
  (void)condition;
}

void formal_top(void) {
  uint8_t x;
  FORMAL_INPUT(x);
  FORMAL_OUTPUT_BUF(&output, 1, "y");
#ifdef ASSUMED
  __formal_assume(x < 4);
  if (x >= 4)
    return;
#ifdef OUTPUT_PLUS_ONE
  output = (uint8_t)(x + 1u);
#else
  output = x;
#endif
#else
  output = (uint8_t)(x + 1);
#endif
}

int main(void) {
  for (unsigned input = 0; input < 256; ++input) {
    next_input = (uint8_t)input;
    formal_top();
#ifdef ASSUMED
    if (input >= 4)
      continue;
#endif
    printf("%u %u\n", input, (unsigned)output);
  }
  return 0;
}
