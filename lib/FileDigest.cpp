#include "formal/FormalPass.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"

#include <algorithm>
#include <array>
#include <cerrno>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <string>

#if defined(__linux__)
#include <fcntl.h>
#include <linux/if_alg.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

namespace {

constexpr std::array<std::uint32_t, 64> RoundConstants{
    0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U, 0x3956c25bU,
    0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U, 0xd807aa98U, 0x12835b01U,
    0x243185beU, 0x550c7dc3U, 0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U,
    0xc19bf174U, 0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
    0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU, 0x983e5152U,
    0xa831c66dU, 0xb00327c8U, 0xbf597fc7U, 0xc6e00bf3U, 0xd5a79147U,
    0x06ca6351U, 0x14292967U, 0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU,
    0x53380d13U, 0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
    0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U, 0xd192e819U,
    0xd6990624U, 0xf40e3585U, 0x106aa070U, 0x19a4c116U, 0x1e376c08U,
    0x2748774cU, 0x34b0bcb5U, 0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU,
    0x682e6ff3U, 0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
    0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U};

constexpr std::uint32_t rotateRight(std::uint32_t Value, unsigned Amount) {
  return (Value >> Amount) | (Value << (32U - Amount));
}

class SHA256 final {
public:
  void update(const std::uint8_t *Data, std::size_t Size) {
    TotalBytes += Size;
    while (Size != 0) {
      std::size_t CopySize = std::min(Size, Block.size() - BlockSize);
      std::memcpy(Block.data() + BlockSize, Data, CopySize);
      BlockSize += CopySize;
      Data += CopySize;
      Size -= CopySize;
      if (BlockSize == Block.size()) {
        transform(Block.data());
        BlockSize = 0;
      }
    }
  }

  std::string final() {
    const std::uint64_t BitLength = TotalBytes * 8U;
    Block[BlockSize++] = 0x80U;
    if (BlockSize > 56) {
      std::fill(Block.begin() + BlockSize, Block.end(), 0U);
      transform(Block.data());
      BlockSize = 0;
    }
    std::fill(Block.begin() + BlockSize, Block.begin() + 56, 0U);
    for (unsigned Index = 0; Index != 8; ++Index)
      Block[63U - Index] = static_cast<std::uint8_t>(BitLength >> (Index * 8U));
    transform(Block.data());

    std::ostringstream Result;
    Result << std::hex << std::setfill('0');
    for (std::uint32_t Word : State)
      Result << std::setw(8) << Word;
    return Result.str();
  }

private:
  void transform(const std::uint8_t *Data) {
    std::array<std::uint32_t, 64> Words{};
    for (unsigned Index = 0; Index != 16; ++Index) {
      const unsigned Offset = Index * 4U;
      Words[Index] = (static_cast<std::uint32_t>(Data[Offset]) << 24U) |
                     (static_cast<std::uint32_t>(Data[Offset + 1]) << 16U) |
                     (static_cast<std::uint32_t>(Data[Offset + 2]) << 8U) |
                     static_cast<std::uint32_t>(Data[Offset + 3]);
    }
    for (unsigned Index = 16; Index != 64; ++Index) {
      const std::uint32_t S0 = rotateRight(Words[Index - 15], 7) ^
                               rotateRight(Words[Index - 15], 18) ^
                               (Words[Index - 15] >> 3U);
      const std::uint32_t S1 = rotateRight(Words[Index - 2], 17) ^
                               rotateRight(Words[Index - 2], 19) ^
                               (Words[Index - 2] >> 10U);
      Words[Index] = Words[Index - 16] + S0 + Words[Index - 7] + S1;
    }

    std::uint32_t A = State[0];
    std::uint32_t B = State[1];
    std::uint32_t C = State[2];
    std::uint32_t D = State[3];
    std::uint32_t E = State[4];
    std::uint32_t F = State[5];
    std::uint32_t G = State[6];
    std::uint32_t H = State[7];
    for (unsigned Index = 0; Index != 64; ++Index) {
      const std::uint32_t S1 =
          rotateRight(E, 6) ^ rotateRight(E, 11) ^ rotateRight(E, 25);
      const std::uint32_t Choice = (E & F) ^ (~E & G);
      const std::uint32_t Temporary1 =
          H + S1 + Choice + RoundConstants[Index] + Words[Index];
      const std::uint32_t S0 =
          rotateRight(A, 2) ^ rotateRight(A, 13) ^ rotateRight(A, 22);
      const std::uint32_t Majority = (A & B) ^ (A & C) ^ (B & C);
      const std::uint32_t Temporary2 = S0 + Majority;
      H = G;
      G = F;
      F = E;
      E = D + Temporary1;
      D = C;
      C = B;
      B = A;
      A = Temporary1 + Temporary2;
    }
    State[0] += A;
    State[1] += B;
    State[2] += C;
    State[3] += D;
    State[4] += E;
    State[5] += F;
    State[6] += G;
    State[7] += H;
  }

  std::array<std::uint32_t, 8> State{0x6a09e667U, 0xbb67ae85U, 0x3c6ef372U,
                                     0xa54ff53aU, 0x510e527fU, 0x9b05688cU,
                                     0x1f83d9abU, 0x5be0cd19U};
  std::array<std::uint8_t, 64> Block{};
  std::uint64_t TotalBytes = 0;
  std::size_t BlockSize = 0;
};

llvm::Expected<std::string> portableSHA256File(llvm::StringRef Path) {
  std::ifstream Input(Path.str(), std::ios::binary);
  if (!Input)
    return llvm::createStringError(
        std::error_code(errno, std::generic_category()),
        llvm::Twine("cannot open '") + Path + "' for SHA-256");

  SHA256 Hash;
  std::array<char, 1U << 20> Buffer{};
  while (Input) {
    Input.read(Buffer.data(), static_cast<std::streamsize>(Buffer.size()));
    const std::streamsize Count = Input.gcount();
    if (Count > 0)
      Hash.update(reinterpret_cast<const std::uint8_t *>(Buffer.data()),
                  static_cast<std::size_t>(Count));
  }
  if (!Input.eof())
    return llvm::createStringError(std::make_error_code(std::errc::io_error),
                                   llvm::Twine("failed while hashing '") +
                                       Path + "'");
  return Hash.final();
}

} // namespace

llvm::Expected<std::string> formal::sha256File(llvm::StringRef Path) {
#if defined(__linux__)
  class FileDescriptor final {
  public:
    explicit FileDescriptor(int Value = -1) : Value(Value) {}
    ~FileDescriptor() {
      if (Value >= 0)
        ::close(Value);
    }
    FileDescriptor(const FileDescriptor &) = delete;
    FileDescriptor &operator=(const FileDescriptor &) = delete;
    int get() const noexcept { return Value; }

  private:
    int Value;
  };

  FileDescriptor Input(::open(Path.str().c_str(), O_RDONLY | O_CLOEXEC));
  if (Input.get() < 0)
    return llvm::createStringError(
        std::error_code(errno, std::generic_category()),
        llvm::Twine("cannot open '") + Path + "' for SHA-256");
  struct stat Status{};
  if (::fstat(Input.get(), &Status) != 0)
    return llvm::createStringError(
        std::error_code(errno, std::generic_category()),
        llvm::Twine("cannot stat '") + Path + "' for SHA-256");

  FileDescriptor Algorithm(::socket(AF_ALG, SOCK_SEQPACKET | SOCK_CLOEXEC, 0));
  if (Algorithm.get() < 0)
    return portableSHA256File(Path);
  sockaddr_alg Address{};
  Address.salg_family = AF_ALG;
  std::memcpy(Address.salg_type, "hash", sizeof("hash"));
  std::memcpy(Address.salg_name, "sha256", sizeof("sha256"));
  if (::bind(Algorithm.get(), reinterpret_cast<sockaddr *>(&Address),
             sizeof(Address)) != 0)
    return portableSHA256File(Path);
  FileDescriptor Operation(
      ::accept4(Algorithm.get(), nullptr, nullptr, SOCK_CLOEXEC));
  if (Operation.get() < 0)
    return llvm::createStringError(
        std::error_code(errno, std::generic_category()),
        "cannot accept Linux AF_ALG SHA-256 operation");

  std::array<std::uint8_t, 1U << 20> Buffer{};
  std::uint64_t Total = 0;
  while (true) {
    ssize_t Count = ::read(Input.get(), Buffer.data(), Buffer.size());
    if (Count < 0) {
      if (errno == EINTR)
        continue;
      return llvm::createStringError(
          std::error_code(errno, std::generic_category()),
          llvm::Twine("failed while reading '") + Path + "' for SHA-256");
    }
    if (Count == 0)
      break;
    std::size_t Offset = 0;
    while (Offset != static_cast<std::size_t>(Count)) {
      const std::size_t Remaining = static_cast<std::size_t>(Count) - Offset;
      ssize_t Sent =
          ::send(Operation.get(), Buffer.data() + Offset, Remaining, MSG_MORE);
      if (Sent < 0) {
        if (errno == EINTR)
          continue;
        return llvm::createStringError(
            std::error_code(errno, std::generic_category()),
            "Linux AF_ALG SHA-256 input failed");
      }
      Offset += static_cast<std::size_t>(Sent);
    }
    Total += static_cast<std::uint64_t>(Count);
  }
  if (Total != static_cast<std::uint64_t>(Status.st_size))
    return llvm::createStringError(
        std::make_error_code(std::errc::io_error),
        "executable size changed during SHA-256 calculation");
  if (::send(Operation.get(), nullptr, 0, 0) < 0)
    return llvm::createStringError(
        std::error_code(errno, std::generic_category()),
        "Linux AF_ALG empty SHA-256 input failed");

  std::array<std::uint8_t, 32> Digest{};
  std::size_t Received = 0;
  while (Received != Digest.size()) {
    ssize_t Count = ::read(Operation.get(), Digest.data() + Received,
                           Digest.size() - Received);
    if (Count < 0) {
      if (errno == EINTR)
        continue;
      return llvm::createStringError(
          std::error_code(errno, std::generic_category()),
          "Linux AF_ALG SHA-256 result failed");
    }
    if (Count == 0)
      return llvm::createStringError(
          std::make_error_code(std::errc::io_error),
          "Linux AF_ALG returned a truncated SHA-256 digest");
    Received += static_cast<std::size_t>(Count);
  }
  std::ostringstream Result;
  Result << std::hex << std::setfill('0');
  for (std::uint8_t Byte : Digest)
    Result << std::setw(2) << static_cast<unsigned>(Byte);
  return Result.str();
#else
  return portableSHA256File(Path);
#endif
}

llvm::Expected<formal::HostToolInfo>
formal::validateHostTool(llvm::StringRef ExpectedExecutable,
                         llvm::StringRef ExpectedSHA256,
                         std::uint64_t ExpectedSize) {
  static int MainExecutableAnchor;
  std::string HostPath =
      llvm::sys::fs::getMainExecutable(nullptr, &MainExecutableAnchor);
  if (HostPath.empty())
    return llvm::createStringError(std::errc::no_such_file_or_directory,
                                   "cannot identify the host executable");
#if defined(__linux__)
  constexpr llvm::StringLiteral HostImagePath("/proc/self/exe");
#else
  llvm::StringRef HostImagePath = HostPath;
#endif
  bool IsPinnedExecutable = false;
  if (std::error_code EC = llvm::sys::fs::equivalent(
          HostImagePath, ExpectedExecutable, IsPinnedExecutable))
    return llvm::errorCodeToError(EC);
  llvm::sys::fs::file_status HostStatus;
  if (std::error_code EC = llvm::sys::fs::status(HostImagePath, HostStatus))
    return llvm::errorCodeToError(EC);
  const std::uint64_t HostSize = HostStatus.getSize();
  llvm::Expected<std::string> HostSHA256 = sha256File(HostImagePath);
  if (!HostSHA256)
    return HostSHA256.takeError();
  IsPinnedExecutable = IsPinnedExecutable && HostSize == ExpectedSize &&
                       *HostSHA256 == ExpectedSHA256;
  HostToolInfo Result{HostPath, *HostSHA256, HostSize, IsPinnedExecutable, {}};
  if (!Result.MatchesPinned) {
    std::ostringstream Message;
    Message << "host executable '" << HostPath
            << "' is not the configured pinned opt image '"
            << ExpectedExecutable.str() << "' (size " << ExpectedSize
            << ", SHA-256 " << ExpectedSHA256.str() << "); actual size is "
            << HostSize << ", actual SHA-256 is " << *HostSHA256;
    Result.MismatchMessage = Message.str();
  }
  return Result;
}
