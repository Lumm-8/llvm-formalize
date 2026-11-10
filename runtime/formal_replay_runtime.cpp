#include "formal_replay_runtime.h"
#include "formal_api.h"

#include "llvm/Support/JSON.h"

#include <csetjmp>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

namespace {

struct Region {
  std::string Name;
  std::vector<std::uint8_t> Bytes;
  const void *Address = nullptr;
  bool Used = false;
};

std::vector<Region> Inputs;
std::vector<Region> Outputs;
std::jmp_buf Escape;
bool Active = false;
bool Captured = false;
bool OutsideDomain = false;
std::string Failure;

bool validName(llvm::StringRef Name) {
  if (Name.empty() || Name.size() > 128)
    return false;
  for (unsigned char Character : Name)
    if (!((Character >= 'A' && Character <= 'Z') ||
          (Character >= 'a' && Character <= 'z') ||
          (Character >= '0' && Character <= '9') || Character == '_' ||
          Character == '.' || Character == '-'))
      return false;
  return true;
}

void fail(const char *Message) {
  if (!Active)
    std::abort();
  Failure = Message;
  std::longjmp(Escape, 1);
}

bool loadInputs(const char *Path) {
  if (!Path)
    return false;
  std::ifstream File(Path, std::ios::binary);
  if (!File)
    return false;
  std::string Text((std::istreambuf_iterator<char>(File)),
                   std::istreambuf_iterator<char>());
  if (!File.good() && !File.eof())
    return false;
  auto Parsed = llvm::json::parse(Text);
  if (!Parsed)
    return false;
  auto *Object = Parsed->getAsObject();
  auto *Array = Object ? Object->getArray("inputs") : nullptr;
  if (!Array || Array->size() > 256)
    return false;
  for (const llvm::json::Value &Value : *Array) {
    const auto *Item = Value.getAsObject();
    if (!Item)
      return false;
    auto Name = Item->getString("name");
    const auto *Bytes = Item->getArray("bytes");
    if (!Name || !validName(*Name) || !Bytes || Bytes->size() > 65536)
      return false;
    for (const Region &Old : Inputs)
      if (Old.Name == *Name)
        return false;
    Region New;
    New.Name = Name->str();
    for (const llvm::json::Value &Byte : *Bytes) {
      auto Integer = Byte.getAsInteger();
      if (!Integer || *Integer < 0 || *Integer > 255)
        return false;
      New.Bytes.push_back(static_cast<std::uint8_t>(*Integer));
    }
    Inputs.push_back(std::move(New));
  }
  return true;
}

bool writeOutputs(const char *Path, const char *Status) {
  if (!Path)
    return false;
  std::ofstream File(Path, std::ios::binary | std::ios::trunc);
  if (!File)
    return false;
  File << "{\"status\":\"" << Status << "\",\"outputs\":[";
  for (std::size_t Index = 0; Index < Outputs.size(); ++Index) {
    if (Index)
      File << ',';
    const Region &Output = Outputs[Index];
    File << "{\"name\":\"" << Output.Name << "\",\"bytes\":[";
    for (std::size_t Byte = 0; Byte < Output.Bytes.size(); ++Byte) {
      if (Byte)
        File << ',';
      File << static_cast<unsigned>(Output.Bytes[Byte]);
    }
    File << "]}";
  }
  File << "]}\n";
  return static_cast<bool>(File);
}

} // namespace

extern "C" void __formal_register_input(void *Address, size_t Size,
                                        const char *Name) {
  if (!Active || !Address || !Name)
    fail("input registration outside active replay");
  for (Region &Input : Inputs) {
    if (Input.Name != Name)
      continue;
    if (Input.Used || Input.Bytes.size() != Size)
      fail("duplicate input registration or input size mismatch");
    Input.Used = true;
    auto *Destination = static_cast<std::uint8_t *>(Address);
    for (std::size_t Index = 0; Index < Size; ++Index)
      Destination[Index] = Input.Bytes[Index];
    return;
  }
  fail("input name is absent from counterexample");
}

extern "C" void __formal_register_output(void *Address, size_t Size,
                                         const char *Name) {
  if (!Active || Captured || !Address || !Name || !validName(Name) ||
      Size > 65536)
    fail("invalid output registration");
  for (const Region &Output : Outputs)
    if (Output.Name == Name)
      fail("duplicate output registration");
  Region Output;
  Output.Name = Name;
  Output.Address = Address;
  Output.Bytes.resize(Size);
  Outputs.push_back(std::move(Output));
}

extern "C" void __formal_assume(int Condition) {
  if (!Active)
    fail("assume outside active replay");
  if (!Condition) {
    OutsideDomain = true;
    std::longjmp(Escape, 1);
  }
}

extern "C" void formal_replay_capture(void) {
  if (!Active || Captured)
    fail("capture outside active replay or repeated capture");
  for (Region &Output : Outputs) {
    const auto *Source = static_cast<const std::uint8_t *>(Output.Address);
    for (std::size_t Index = 0; Index < Output.Bytes.size(); ++Index)
      Output.Bytes[Index] = Source[Index];
  }
  Captured = true;
}

extern "C" int formal_replay_run(void (*Entry)(void),
                                 const char *CounterexampleJSON,
                                 const char *OutputJSON) {
  if (Active || !Entry || !OutputJSON)
    return 4;
  Inputs.clear();
  Outputs.clear();
  Failure.clear();
  Captured = false;
  OutsideDomain = false;
  if (!loadInputs(CounterexampleJSON))
    return 3;
  Active = true;
  if (setjmp(Escape) == 0)
    Entry();
  Active = false;
  if (OutsideDomain)
    return writeOutputs(OutputJSON, "OUTSIDE_DOMAIN") ? 2 : 4;
  if (!Failure.empty() || !Captured)
    return 4;
  for (const Region &Input : Inputs)
    if (!Input.Used)
      return 4;
  return writeOutputs(OutputJSON, "CAPTURED") ? 0 : 4;
}
