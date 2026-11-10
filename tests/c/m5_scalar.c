#include "formal_api.h"
#include <stdint.h>

void formal_top(void) {
  uint8_t x;
  uint8_t y;
  FORMAL_INPUT(x);
  FORMAL_OUTPUT(y);
  __formal_assume(x < 4);
  y = (uint8_t)(x + 1u);
}
