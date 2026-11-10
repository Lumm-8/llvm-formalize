#include "formal_api.h"
#ifdef FORMAL_NATIVE_REPLAY
#include "formal_replay_runtime.h"
#endif

#include <cstdint>

__attribute__((noinline)) static void escapeLocal(std::uint8_t **Output) {
  std::uint8_t Local = 7;
  *Output = &Local;
}

__attribute__((noinline)) static std::uint8_t
readEscaped(const std::uint8_t *Pointer) {
  return *Pointer;
}

extern "C" void replayUseAfterReturnTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    std::uint8_t *Escaped = nullptr;
    escapeLocal(&Escaped);
    Output = readEscaped(Escaped);
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}
