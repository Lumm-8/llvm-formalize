#include <stdint.h>
#include <stdio.h>

int main(void) {
  for (unsigned x = 0; x < 4; ++x) {
    uint8_t sum = 0;
    for (unsigned i = 0; i < x; ++i)
      sum = (uint8_t)(sum + i + 1u);
    printf("%u %u\n", x, (unsigned)sum);
  }
  return 0;
}
