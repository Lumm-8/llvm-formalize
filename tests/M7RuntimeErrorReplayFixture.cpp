#include "formal_api.h"
#ifdef FORMAL_NATIVE_REPLAY
#include "formal_replay_runtime.h"
#endif

#include <cstddef>
#include <cstdint>
#include <cstdlib>

// These declarations name the pinned x86_64 Itanium ABI entry points that M7
// models. Calling them explicitly avoids Clang's extra new-expression promises.
extern "C" void *_Znwm(std::size_t);
extern "C" void _ZdlPv(void *);
extern "C" void *m7_runtime_memcpy(void *, const void *, std::size_t)
    __asm__("memcpy");
extern "C" [[noreturn]] void __assert_fail(const char *, const char *,
                                              unsigned int, const char *);

extern "C" __attribute__((noinline)) void
copyUnchecked(void *Destination, const void *Source, std::size_t Size) {
  (void)m7_runtime_memcpy(Destination, Source, Size);
}

extern "C" __attribute__((noinline)) void replayOverlapMemcpyTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    std::uint64_t Data = 0x0504030201ULL;
    void *Destination = reinterpret_cast<std::uint8_t *>(&Data) + 1;
    copyUnchecked(Destination, &Data, 4);
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayDoubleDeleteTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    void *Block = _Znwm(sizeof(std::uint32_t));
    _ZdlPv(Block);
    _ZdlPv(Block);
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayFreeNewTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    void *Block = _Znwm(sizeof(std::uint32_t));
    std::free(Block);
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayAbortTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1)
    std::abort();
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayAssertTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1)
    __assert_fail("M7 assertion", "M7RuntimeErrorReplayFixture.cpp", 1,
                  "replayAssertTop");
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}
