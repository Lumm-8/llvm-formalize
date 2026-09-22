#include "formal/FormalPass.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

#include <memory>
#include <string>
#include <vector>

using formal::EntryValidationError;

namespace {

std::unique_ptr<llvm::Module> makeTestModule(llvm::LLVMContext &Context) {
  auto Module = std::make_unique<llvm::Module>("formal-test", Context);
  Module->setTargetTriple("x86_64-unknown-linux-gnu");
  Module->setDataLayout("e-m:e-p:64:64-i64:64-n8:16:32:64-S128");
  return Module;
}

llvm::Function *addVoidFunction(llvm::Module &Module, llvm::StringRef Name) {
  llvm::Function *Function = llvm::Function::Create(
      llvm::FunctionType::get(llvm::Type::getVoidTy(Module.getContext()), false),
      llvm::GlobalValue::ExternalLinkage, Name, Module);
  llvm::IRBuilder<> Builder(
      llvm::BasicBlock::Create(Module.getContext(), "entry", Function));
  Builder.CreateRetVoid();
  return Function;
}

formal::PreflightResult validateTestModule(
    llvm::Module &Module, formal::PreflightLimits Limits = {}) {
  return formal::validateModule(Module, "formal_top", 20, "20.0.0git", "test",
                                true, Limits);
}

} // namespace

TEST(FormalPassEntryProperties, AcceptsExactlyExternalVoidNoArgsDefinition) {
  EXPECT_EQ(formal::validateEntryProperties(true, false, true, 0, false),
            EntryValidationError::None);
  EXPECT_EQ(formal::validateEntryProperties(false, false, true, 0, false),
            EntryValidationError::NotFound);
  EXPECT_EQ(formal::validateEntryProperties(true, true, true, 0, false),
            EntryValidationError::DeclarationOnly);
  EXPECT_EQ(formal::validateEntryProperties(true, false, false, 0, false),
            EntryValidationError::InvalidSignature);
  EXPECT_EQ(formal::validateEntryProperties(true, false, true, 1, false),
            EntryValidationError::InvalidSignature);
  EXPECT_EQ(formal::validateEntryProperties(true, false, true, 0, true),
            EntryValidationError::InvalidSignature);
}

TEST(FormalPassTargetProperties, RejectsMissingAndIncompatibleContracts) {
  using formal::PreflightCode;
  EXPECT_EQ(formal::validateTargetProperties(false, true, true, true, 64),
            PreflightCode::MissingTargetTriple);
  EXPECT_EQ(formal::validateTargetProperties(true, false, true, true, 64),
            PreflightCode::MissingDataLayout);
  EXPECT_EQ(formal::validateTargetProperties(true, true, false, true, 64),
            PreflightCode::UnsupportedTarget);
  EXPECT_EQ(formal::validateTargetProperties(true, true, true, false, 64),
            PreflightCode::IncompatibleDataLayout);
  EXPECT_EQ(formal::validateTargetProperties(true, true, true, true, 32),
            PreflightCode::IncompatibleDataLayout);
  EXPECT_EQ(formal::validateTargetProperties(true, true, true, true, 64),
            PreflightCode::Valid);
}

TEST(FormalPassCallSite, RejectsNonCCallingConventionWithCCallee) {
  llvm::LLVMContext Context;
  std::unique_ptr<llvm::Module> Module = makeTestModule(Context);
  llvm::Function *Callee = addVoidFunction(*Module, "callee");
  llvm::Function *Entry = addVoidFunction(*Module, "formal_top");
  llvm::Instruction *Return = Entry->getEntryBlock().getTerminator();
  llvm::IRBuilder<> Builder(Return);
  llvm::CallInst *Call = Builder.CreateCall(Callee);
  Call->setCallingConv(llvm::CallingConv::Fast);

  formal::PreflightResult Result = validateTestModule(*Module);
  EXPECT_EQ(Result.Code, formal::PreflightCode::UnsupportedCallingConvention);
  EXPECT_TRUE(Result.incomplete());
  EXPECT_NE(Result.Message.find("call site"), std::string::npos);
}

TEST(FormalPassResources, AccountsForConstantAsMetadataValues) {
  llvm::LLVMContext Context;
  std::unique_ptr<llvm::Module> Module = makeTestModule(Context);
  addVoidFunction(*Module, "formal_top");
  llvm::IntegerType *WideType = llvm::IntegerType::get(Context, 257);
  llvm::Metadata *WideConstant = llvm::ConstantAsMetadata::get(
      llvm::ConstantInt::get(WideType, 0));
  Module->getOrInsertNamedMetadata("formal.values")
      ->addOperand(llvm::MDNode::get(Context, WideConstant));

  formal::PreflightLimits Limits;
  Limits.MaxIntegerBitWidth = 256;
  formal::PreflightResult Result = validateTestModule(*Module, Limits);
  EXPECT_EQ(Result.Code, formal::PreflightCode::ResourceLimit);
  EXPECT_NE(Result.Message.find("integer bit width"), std::string::npos);
}

TEST(FormalPassResources, RejectsDeepConstantGraphsBeforeModulePrinting) {
  llvm::LLVMContext Context;
  std::unique_ptr<llvm::Module> Module = makeTestModule(Context);
  addVoidFunction(*Module, "formal_top");
  llvm::Type *I8 = llvm::Type::getInt8Ty(Context);
  llvm::Type *I64 = llvm::Type::getInt64Ty(Context);
  auto *Anchor = new llvm::GlobalVariable(
      *Module, I8, false, llvm::GlobalValue::ExternalLinkage,
      llvm::ConstantInt::get(I8, 0), "anchor");
  llvm::Constant *Value = llvm::ConstantExpr::getPtrToInt(Anchor, I64);
  for (unsigned Index = 0; Index != 32; ++Index) {
    llvm::Constant *Amount = llvm::ConstantInt::get(I64, Index + 1);
    Value = Index % 2 == 0 ? llvm::ConstantExpr::getAdd(Value, Amount)
                           : llvm::ConstantExpr::getXor(Value, Amount);
  }
  new llvm::GlobalVariable(*Module, I64, true,
                           llvm::GlobalValue::ExternalLinkage, Value, "deep");

  formal::PreflightLimits Limits;
  Limits.MaxConstantDepth = 8;
  formal::PreflightResult Result = validateTestModule(*Module, Limits);
  EXPECT_EQ(Result.Code, formal::PreflightCode::ResourceLimit);
  EXPECT_NE(Result.Message.find("constant graph nesting"), std::string::npos);
}

TEST(FormalPassResources, RejectsGreatestDepthThroughSharedMetadataDag) {
  llvm::LLVMContext Context;
  std::unique_ptr<llvm::Module> Module = makeTestModule(Context);
  addVoidFunction(*Module, "formal_top");

  std::vector<llvm::MDNode *> Chain;
  Chain.push_back(llvm::MDNode::getDistinct(Context, {}));
  for (unsigned Depth = 0; Depth != 8; ++Depth)
    Chain.push_back(llvm::MDNode::getDistinct(Context, Chain.back()));

  llvm::NamedMDNode *Roots =
      Module->getOrInsertNamedMetadata("formal.shared-depth");
  for (auto Position = Chain.rbegin(); Position != Chain.rend(); ++Position)
    Roots->addOperand(*Position);

  formal::PreflightLimits Limits;
  Limits.MaxMetadataDepth = 4;
  formal::PreflightResult Result = validateTestModule(*Module, Limits);
  EXPECT_EQ(Result.Code, formal::PreflightCode::ResourceLimit);
  EXPECT_NE(Result.Message.find("metadata nesting depth"), std::string::npos);
}

TEST(FormalPassResources, BoundsRepeatedEdgesInDenseMetadataDag) {
  llvm::LLVMContext Context;
  std::unique_ptr<llvm::Module> Module = makeTestModule(Context);
  addVoidFunction(*Module, "formal_top");

  llvm::MDNode *Shared = llvm::MDNode::getDistinct(Context, {});
  std::vector<llvm::Metadata *> Parents;
  for (unsigned Index = 0; Index != 8; ++Index)
    Parents.push_back(llvm::MDNode::getDistinct(Context, Shared));
  llvm::MDNode *Root = llvm::MDNode::getDistinct(Context, Parents);
  Module->getOrInsertNamedMetadata("formal.dense-dag")->addOperand(Root);

  formal::PreflightLimits Limits;
  Limits.MaxMetadataNodes = 12;
  formal::PreflightResult Result = validateTestModule(*Module, Limits);
  EXPECT_EQ(Result.Code, formal::PreflightCode::ResourceLimit);
  EXPECT_NE(Result.Message.find("metadata traversal work"), std::string::npos);
}

TEST(FormalPassResources, AcceptsSelfAndMultiNodeMetadataCycles) {
  llvm::LLVMContext Context;
  std::unique_ptr<llvm::Module> Module = makeTestModule(Context);
  addVoidFunction(*Module, "formal_top");

  llvm::MDNode *Self = llvm::MDNode::getDistinct(Context, {nullptr});
  Self->replaceOperandWith(0, Self);
  Module->getOrInsertNamedMetadata("formal.self-cycle")->addOperand(Self);

  llvm::MDNode *First = llvm::MDNode::getDistinct(Context, {nullptr});
  llvm::MDNode *Second = llvm::MDNode::getDistinct(Context, First);
  First->replaceOperandWith(0, Second);
  Module->getOrInsertNamedMetadata("formal.multi-cycle")->addOperand(First);

  formal::PreflightLimits Limits;
  Limits.MaxMetadataDepth = 1;
  formal::PreflightResult Result = validateTestModule(*Module, Limits);
  EXPECT_EQ(Result.Code, formal::PreflightCode::Valid);
}

TEST(FormalPassSHA256, UsesContentAndRejectsSameSizeDifferences) {
  llvm::SmallString<128> FirstPath;
  llvm::SmallString<128> SecondPath;
  int FirstDescriptor = -1;
  int SecondDescriptor = -1;
  ASSERT_FALSE(llvm::sys::fs::createTemporaryFile("formal-sha-first", "bin",
                                                  FirstDescriptor, FirstPath));
  ASSERT_FALSE(llvm::sys::fs::createTemporaryFile(
      "formal-sha-second", "bin", SecondDescriptor, SecondPath));
  {
    llvm::raw_fd_ostream First(FirstDescriptor, true);
    llvm::raw_fd_ostream Second(SecondDescriptor, true);
    First << "abc";
    Second << "abd";
  }

  llvm::Expected<std::string> FirstDigest = formal::sha256File(FirstPath);
  llvm::Expected<std::string> SecondDigest = formal::sha256File(SecondPath);
  ASSERT_TRUE(static_cast<bool>(FirstDigest));
  ASSERT_TRUE(static_cast<bool>(SecondDigest));
  EXPECT_EQ(*FirstDigest,
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
  EXPECT_NE(*FirstDigest, *SecondDigest);

  EXPECT_FALSE(llvm::sys::fs::remove(FirstPath));
  EXPECT_FALSE(llvm::sys::fs::remove(SecondPath));
}

TEST(FormalPassHostTool, RejectsWrongDigestForSameExecutableAndSize) {
  static int MainExecutableAnchor;
  const std::string Executable =
      llvm::sys::fs::getMainExecutable(nullptr, &MainExecutableAnchor);
  ASSERT_FALSE(Executable.empty());
  llvm::sys::fs::file_status Status;
  ASSERT_FALSE(llvm::sys::fs::status(Executable, Status));
  llvm::Expected<std::string> Digest = formal::sha256File(Executable);
  ASSERT_TRUE(static_cast<bool>(Digest));

  llvm::Expected<formal::HostToolInfo> Match =
      formal::validateHostTool(Executable, *Digest, Status.getSize());
  ASSERT_TRUE(static_cast<bool>(Match));
  EXPECT_TRUE(Match->MatchesPinned);
  EXPECT_EQ(Match->SHA256, *Digest);

  std::string WrongDigest(Digest->size(), '0');
  if (WrongDigest == *Digest)
    WrongDigest[0] = '1';
  llvm::Expected<formal::HostToolInfo> Mismatch =
      formal::validateHostTool(Executable, WrongDigest, Status.getSize());
  ASSERT_TRUE(static_cast<bool>(Mismatch));
  EXPECT_FALSE(Mismatch->MatchesPinned);
  EXPECT_EQ(Mismatch->SHA256, *Digest);
  EXPECT_NE(Mismatch->MismatchMessage.find("actual SHA-256"),
            std::string::npos);
}
