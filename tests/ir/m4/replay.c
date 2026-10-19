// Native replay for concrete M4 error witnesses.  This is a standalone test
// program; it is not the M5 registration or model-injection runtime.
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static const uint8_t ReadOnly = 42;

int main(int argc, char **argv) {
  if (argc != 3)
    return 3;
  long Input = strtol(argv[2], 0, 10);
  if (strcmp(argv[1], "oob") == 0) {
    volatile uint8_t Array[1] = {7};
    return Array[Input];
  }
  if (strcmp(argv[1], "symbolic_oob") == 0) {
    volatile uint8_t Array[4] = {0};
    Array[Input] = 7;
    return 0;
  }
  if (strcmp(argv[1], "symbolic_oob_load") == 0) {
    volatile uint8_t Array[4] = {10, 11, 12, 13};
    return Array[Input];
  }
  if (strcmp(argv[1], "zero_size") == 0) {
    // GCC's zero-length array extension mirrors the LLVM zero-sized alloca.
    volatile uint8_t Array[0];
    return Array[Input];
  }
  if (strcmp(argv[1], "crosses_end") == 0) {
    _Alignas(2) volatile uint8_t Array[1] = {1};
    volatile uint16_t *Pointer = (uint16_t *)(void *)Array;
    return *Pointer;
  }
  if (strcmp(argv[1], "offset_plus_size_overflow") == 0) {
    volatile uint8_t Array[1] = {1};
    // The IR uses an align-1 i16 load from a wrapping, non-inbounds GEP.
    // Integer address arithmetic and memcpy preserve that byte alignment;
    // a typed uint16_t dereference would instead trip UBSan's alignment check.
    uintptr_t Address = (uintptr_t)Array + (uintptr_t)Input;
    uint16_t Value = 0;
    memcpy(&Value, (const void *)Address, sizeof(Value));
    return Value;
  }
  if (strcmp(argv[1], "nullable_select") == 0) {
    volatile uint8_t Value = 4;
    volatile uint8_t *Pointer = Input ? &Value : (uint8_t *)0;
    return *Pointer;
  }
  if (strcmp(argv[1], "null") == 0) {
    volatile uint8_t *Pointer = (uint8_t *)(uintptr_t)Input;
    return *Pointer;
  }
  if (strcmp(argv[1], "null_store") == 0) {
    volatile uint8_t *Pointer = (uint8_t *)(uintptr_t)Input;
    *Pointer = 1;
    return 0;
  }
  if (strcmp(argv[1], "null_memcpy") == 0) {
    uint8_t Value = 1;
    memcpy((void *)(uintptr_t)Input, &Value, 1);
    return 0;
  }
  if (strcmp(argv[1], "misaligned") == 0) {
    _Alignas(4) uint8_t Bytes[8] = {0};
    volatile uint32_t *Pointer = (uint32_t *)(void *)(Bytes + Input);
    return *Pointer;
  }
  if (strcmp(argv[1], "misaligned_i16") == 0) {
    _Alignas(4) uint8_t Bytes[4] = {0};
    volatile uint16_t *Pointer = (uint16_t *)(void *)(Bytes + Input);
    return *Pointer;
  }
  if (strcmp(argv[1], "readonly") == 0) {
    volatile uint8_t *Pointer = (uint8_t *)(uintptr_t)&ReadOnly;
    *Pointer = (uint8_t)Input;
    return 0;
  }
  return 3;
}
