#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringRef.h"

#include <iostream>
#include <string>
#include <string_view>

namespace {

bool wrapPoison(std::string_view Opcode, std::string_view Flag,
                const llvm::APInt &Left, const llvm::APInt &Right) {
  if (Opcode == "shl" && Right.uge(Left.getBitWidth()))
    return true;
  bool Overflow = false;
  if (Opcode == "add") {
    if (Flag == "nsw")
      (void)Left.sadd_ov(Right, Overflow);
    else
      (void)Left.uadd_ov(Right, Overflow);
  } else if (Opcode == "sub") {
    if (Flag == "nsw")
      (void)Left.ssub_ov(Right, Overflow);
    else
      (void)Left.usub_ov(Right, Overflow);
  } else if (Opcode == "mul") {
    if (Flag == "nsw")
      (void)Left.smul_ov(Right, Overflow);
    else
      (void)Left.umul_ov(Right, Overflow);
  } else if (Flag == "nsw") {
    (void)Left.sshl_ov(Right, Overflow);
  } else {
    (void)Left.ushl_ov(Right, Overflow);
  }
  return Overflow;
}

bool exactOrUB(std::string_view Opcode, const llvm::APInt &Left,
               const llvm::APInt &Right) {
  if (Opcode == "udiv" || Opcode == "sdiv") {
    if (Right.isZero())
      return true;
    if (Opcode == "sdiv") {
      if (Left.isMinSignedValue() && Right.isAllOnes())
        return true;
      return !Left.srem(Right).isZero();
    }
    return !Left.urem(Right).isZero();
  }
  if (Right.uge(Left.getBitWidth()))
    return true;
  const unsigned Shift = static_cast<unsigned>(Right.getZExtValue());
  return !(Left & llvm::APInt::getLowBitsSet(Left.getBitWidth(), Shift))
              .isZero();
}

} // namespace

int main() {
  unsigned Width;
  std::string Opcode, Flag, L, R;
  while (std::cin >> Width >> Opcode >> Flag >> L >> R) {
    const llvm::APInt Left(Width, L, 10), Right(Width, R, 10);
    std::cout << (Flag == "exact" ? exactOrUB(Opcode, Left, Right)
                                  : wrapPoison(Opcode, Flag, Left, Right))
              << '\n';
  }
  return std::cin.eof() ? 0 : 1;
}
