#include "formal_api.h"
#ifdef FORMAL_NATIVE_REPLAY
#include "formal_replay_runtime.h"
#endif

#include <cstdint>

extern "C" __attribute__((noinline)) void replayErrorTop() {
  std::uint8_t X;
  std::uint8_t Buffer[1];
  Buffer[0] = 0;
  FORMAL_INPUT_BUF(&X, sizeof(X), "x");
  FORMAL_OUTPUT_BUF(&Buffer, sizeof(Buffer), "y");
  if (X == 1) {
    unsigned Index = X;
    Buffer[Index] = 7;
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}
