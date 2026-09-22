#include "formal/Solver.h"

#include <charconv>
#include <string_view>

namespace {

bool parseFileDescriptor(const char *Text, int &Value) {
  std::string_view Input(Text ? Text : "");
  const char *End = Input.data() + Input.size();
  auto Result = std::from_chars(Input.data(), End, Value);
  return Result.ec == std::errc{} && Result.ptr == End && Value >= 0;
}

} // namespace

int main(int ArgumentCount, char **Arguments) {
  int RequestFile = -1;
  int ResultFile = -1;
  if (ArgumentCount != 3 ||
      !parseFileDescriptor(Arguments[1], RequestFile) ||
      !parseFileDescriptor(Arguments[2], ResultFile))
    return 2;
  return formal::detail::runSTPWorkerProcess(RequestFile, ResultFile);
}
