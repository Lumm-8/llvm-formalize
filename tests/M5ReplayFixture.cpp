#include "formal_api.h"
#include "formal_replay_runtime.h"

#include <cstdint>

extern "C" __attribute__((noinline)) void replay_top() {
  std::uint8_t Input;
  std::uint8_t Output = 0;
  FORMAL_INPUT(Input);
  FORMAL_OUTPUT(Output);
  __formal_assume(Input < 4);
  Output = static_cast<std::uint8_t>(Input + 1);
  formal_replay_capture();
}
