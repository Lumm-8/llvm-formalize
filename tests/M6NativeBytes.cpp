#include <stdint.h>
#include <stdio.h>
#include <string.h>

struct Unpacked { uint8_t tag; uint32_t value; };
#pragma pack(push, 1)
struct Packed { uint8_t tag; uint32_t value; };
#pragma pack(pop)
struct Nested { struct Unpacked head; struct Packed items[2]; };
struct Tail { uint32_t value; uint8_t tag; };
union Reinterpreted { uint32_t word; uint8_t bytes[4]; };

extern "C" uint8_t m6_compiled_packed_copy_byte(void) {
  struct Packed source = {7, UINT32_C(0x11223344)};
  struct Packed target;
  memset(&target, 0xa5, sizeof target);
  target = source;
  return reinterpret_cast<const uint8_t *>(&target)[1];
}

extern "C" uint8_t m6_compiled_unpacked_copy_byte(void) {
  struct Unpacked source = {7, UINT32_C(0x11223344)};
  struct Unpacked target;
  memset(&target, 0xa5, sizeof target);
  target = source;
  return reinterpret_cast<const uint8_t *>(&target)[4];
}

extern "C" uint8_t m6_compiled_nested_copy_byte(void) {
  struct Nested source = {
      {9, UINT32_C(0x55667788)},
      {{1, UINT32_C(0x01020304)}, {2, UINT32_C(0xa0b0c0d0)}}};
  struct Nested target;
  memset(&target, 0xa5, sizeof target);
  target = source;
  return reinterpret_cast<const uint8_t *>(&target)[14];
}

extern "C" uint8_t m6_compiled_tail_copy_byte(void) {
  struct Tail source = {UINT32_C(0x12345678), 11};
  struct Tail target;
  memset(&target, 0xa5, sizeof target);
  target = source;
  return reinterpret_cast<const uint8_t *>(&target)[0];
}

static void dump(const char *name, const void *object, size_t size) {
  const uint8_t *bytes = (const uint8_t *)object;
  printf("%s %zu", name, size);
  for (size_t index = 0; index < size; ++index)
    printf(" %u", (unsigned)bytes[index]);
  putchar('\n');
}

int main(void) {
  struct Packed packed;
  memset(&packed, 0, sizeof packed);
  packed.tag = 7;
  packed.value = UINT32_C(0x11223344);
  dump("P", &packed, sizeof packed);
  struct Packed packed_copy;
  memset(&packed_copy, 0xa5, sizeof packed_copy);
  packed_copy = packed;
  dump("PC", &packed_copy, sizeof packed_copy);

  struct Unpacked unpacked;
  memset(&unpacked, 0, sizeof unpacked);
  unpacked.tag = 7;
  unpacked.value = UINT32_C(0x11223344);
  dump("U", &unpacked, sizeof unpacked);
  struct Unpacked unpacked_copy;
  memset(&unpacked_copy, 0xa5, sizeof unpacked_copy);
  unpacked_copy = unpacked;
  dump("UC", &unpacked_copy, sizeof unpacked_copy);

  struct Nested nested;
  memset(&nested, 0, sizeof nested);
  nested.head.tag = 9;
  nested.head.value = UINT32_C(0x55667788);
  nested.items[0].tag = 1;
  nested.items[0].value = UINT32_C(0x01020304);
  nested.items[1].tag = 2;
  nested.items[1].value = UINT32_C(0xa0b0c0d0);
  dump("N", &nested, sizeof nested);
  struct Nested nested_copy;
  memset(&nested_copy, 0xa5, sizeof nested_copy);
  nested_copy = nested;
  dump("NC", &nested_copy, sizeof nested_copy);

  struct Tail tail;
  memset(&tail, 0, sizeof tail);
  tail.value = UINT32_C(0x12345678);
  tail.tag = 11;
  dump("T", &tail, sizeof tail);
  struct Tail tail_copy;
  memset(&tail_copy, 0xa5, sizeof tail_copy);
  tail_copy = tail;
  dump("TC", &tail_copy, sizeof tail_copy);

  union Reinterpreted reinterpreted;
  memset(&reinterpreted, 0, sizeof reinterpreted);
  reinterpreted.word = UINT32_C(0x11223344);
  dump("R", &reinterpreted, sizeof reinterpreted);
  return 0;
}
