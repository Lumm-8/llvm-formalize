#include "formal_api.h"
#include <stdint.h>
#include <stdio.h>

static uint8_t next_input;

void __formal_register_input(void *addr, size_t size, const char *name) {
  if (size != 1 || name[0] != 'x' || name[1] != '\0')
    __builtin_trap();
  *(uint8_t *)addr = next_input;
}

void __formal_register_output(void *addr, size_t size, const char *name) {
  if (!addr || size != 1 ||
      !((name[0] == 'y' || name[0] == 'z') && name[1] == '\0'))
    __builtin_trap();
}

void __formal_assume(int condition) {
  (void)condition;
}

static void two_outputs(void) {
  uint8_t x, y, z;
  FORMAL_INPUT(x);
  FORMAL_OUTPUT(y);
  FORMAL_OUTPUT(z);
  __formal_assume(x < 4);
  if (x >= 4)
    return;
  y = (uint8_t)(x + 1u);
  z = (uint8_t)(x ^ 3u);
  printf("%u %u %u\n", (unsigned)x, (unsigned)y, (unsigned)z);
}

int main(void) {
  for (unsigned value = 0; value < 256; ++value) {
    next_input = (uint8_t)value;
    two_outputs();
  }
  return 0;
}
