#include "formal_api.h"
#ifdef FORMAL_NATIVE_REPLAY
#include "formal_replay_runtime.h"
#endif

#include <cstdint>
#include <cstdlib>

extern "C" __attribute__((noinline)) void replayHeapErrorTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    void *Block = std::malloc(4);
    if (Block) {
      std::free(Block);
      std::free(Block);
    }
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) std::uint8_t
readUnchecked(const std::uint8_t *Pointer) {
  return *Pointer;
}

extern "C" __attribute__((noinline)) void releaseUnchecked(void *Pointer) {
  std::free(Pointer);
}

extern "C" __attribute__((noinline)) void replayUseAfterFreeTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    auto *Block = static_cast<std::uint8_t *>(std::malloc(4));
    if (Block) {
      Block[0] = 7;
      std::free(Block);
      Output = readUnchecked(Block);
    }
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayInvalidFreeTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    std::uint8_t Local = 7;
    releaseUnchecked(&Local);
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayInteriorFreeTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    auto *Block = static_cast<std::uint8_t *>(std::malloc(4));
    if (Block) {
      releaseUnchecked(Block + 1);
      std::free(Block);
    }
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}

extern "C" __attribute__((noinline)) void replayUseAfterReallocTop() {
  std::uint8_t Choice;
  std::uint8_t Output = 0;
  FORMAL_INPUT_BUF(&Choice, sizeof(Choice), "choice");
  FORMAL_OUTPUT_BUF(&Output, sizeof(Output), "output");
  if (Choice == 1) {
    auto *Block = static_cast<std::uint8_t *>(std::malloc(4));
    if (Block) {
      Block[0] = 7;
      auto *NewBlock = static_cast<std::uint8_t *>(std::realloc(Block, 4096));
      if (NewBlock) {
        Output = readUnchecked(Block);
        std::free(NewBlock);
      } else {
        std::free(Block);
      }
    }
  }
#ifdef FORMAL_NATIVE_REPLAY
  formal_replay_capture();
#endif
}
