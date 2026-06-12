//===-- translateToStp.cpp - Example Transformations --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/TranslateToStp.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

// Include std headers
#include <map>
#include <vector>

// Include KLEE headers
#include "klee/Expr/Expr.h"
#include "klee/Expr/ExprBuilder.h"
// FIXME: need to change the include path to the correct one
// #include <stp/c_interface.h>

#include "klee/Solver/STPBuilder.h"
#include <stp/c_interface.h>
using namespace llvm;

// Set of alloca base pointers whose ITE chains are currently being built.
// Loads from these allocas read the root array directly to avoid recursive
// ITE chain explosion (e.g.  a = ashr a, 1 where store value reads same
// alloca that is having its ITE chain constructed).
static thread_local SmallPtrSet<Value *, 4> BuildingITEChains;

BddBranchRecord::BddBranchRecord() {
  bdd_init(100000, 10000);
  bdd_setvarnum(10000);
  bddIndex = 0;
  bddIndexMax = 10000;
}

void BddBranchRecord::collectBranchInfo(Function *F) {
    ReversePostOrderTraversal<Function*> pro(F);
    BasicBlock *entry = &(F->getEntryBlock());

    for (BasicBlock *bb: pro) {
      bdd pc = bddtrue;

      if (bb != entry) {
        pc = bddfalse;
      }

      // get all predecessors
      for (BasicBlock *preBasBlo: predecessors(bb)) {
        // Use bddfalse for predecessors not yet processed (back edges)
        bdd preBdd = bddfalse;
        auto it = basicBlockBdd.find(preBasBlo);
        if (it != basicBlockBdd.end())
          preBdd = it->second;
        bdd brBdd  = getEdgeCondition(preBasBlo, bb);

        pc = pc | (preBdd & brBdd);
      }

      basicBlockBdd[bb] = pc;
    }
  }

bdd BddBranchRecord::getEdgeCondition(BasicBlock *parent, BasicBlock *child) {
  auto *branchInst = dyn_cast<BranchInst>(parent->getTerminator());
  if (!branchInst)
    return bddtrue;

  if (branchInst->isUnconditional()) {
    return bddtrue;
  }

  Value *condition = branchInst->getCondition();
  bool trueBranch = (branchInst->getSuccessor(0) == child);

  if (valueId.count(condition) != 0) {
    int index = valueId[condition];
    bdd ret = (trueBranch ? bdd_ithvar(index) : bdd_nithvar(index));
    // bdd ret = (trueBranch ? bdd_high(index) : bdd_low(index));

    return ret;
  }

  valueId[condition] = bddIndex;
  bddValue[bddIndex] = condition;
  bdd ret;

  if (trueBranch) {
    ret = bdd_ithvar(bddIndex);
  }
  else {
    ret = bdd_nithvar(bddIndex);
  }

  ++bddIndex;
  if (bddIndex >= bddIndexMax) {
     int result = bdd_extvarnum(bddIndexMax);
     
     if (result < 0) {
       assert("bdd alloca failed");
     }

     bddIndexMax *= 2;
  }

  return ret;
}

BddBranchRecord::~BddBranchRecord() {
  // FIXME: bdd_done() crashes due to ABI mismatch with pre-built BuDDy library.
  // Leaking BDD memory on shutdown is acceptable for a short-lived opt process.
  // bdd_done();
}

TranslateToStpPass::TranslateToStpPass() {
  bddBR = std::make_unique<BddBranchRecord>();
  arrayCache = std::make_unique<klee::ArrayCache>();
  exprBuilder = std::unique_ptr<klee::ExprBuilder>(klee::createDefaultExprBuilder());
  vc = vc_createValidityChecker();
  stpBuilder = new klee::STPBuilder(vc);
}

TranslateToStpPass::TranslateToStpPass(TranslateToStpPass&& other) noexcept
  : _F(other._F),
    dataLayout(other.dataLayout),
    output(std::move(other.output)),
    outputKleeExpr(std::move(other.outputKleeExpr)),
    exprBuilder(std::move(other.exprBuilder)),
    bddBR(std::move(other.bddBR)),
    valueToKleeExprCache(std::move(other.valueToKleeExprCache)),
    vc(other.vc),
    stpBuilder(other.stpBuilder),
    arrayCache(std::move(other.arrayCache)),
    memoryArrays(std::move(other.memoryArrays)),
    memoryUpdateLists(std::move(other.memoryUpdateLists)),
    valueToBlock(std::move(other.valueToBlock)),
    argumentExprs(std::move(other.argumentExprs)),
    globalVarExprs(std::move(other.globalVarExprs)),
    bddToKleeCache(std::move(other.bddToKleeCache)),
    symbolicVarIndex(other.symbolicVarIndex) {
  other.stpBuilder = nullptr;
  other.vc = nullptr;
}

TranslateToStpPass&
TranslateToStpPass::operator=(TranslateToStpPass&& other) noexcept {
  if (this != &other) {
    delete stpBuilder;
    if (vc) vc_Destroy(vc);

    _F = other._F;
    dataLayout = other.dataLayout;
    output = std::move(other.output);
    outputKleeExpr = std::move(other.outputKleeExpr);
    exprBuilder = std::move(other.exprBuilder);
    bddBR = std::move(other.bddBR);
    valueToKleeExprCache = std::move(other.valueToKleeExprCache);
    vc = other.vc;
    stpBuilder = other.stpBuilder;
    arrayCache = std::move(other.arrayCache);
    memoryArrays = std::move(other.memoryArrays);
    memoryUpdateLists = std::move(other.memoryUpdateLists);
    valueToBlock = std::move(other.valueToBlock);
    argumentExprs = std::move(other.argumentExprs);
    globalVarExprs = std::move(other.globalVarExprs);
    bddToKleeCache = std::move(other.bddToKleeCache);
    symbolicVarIndex = other.symbolicVarIndex;

    other.stpBuilder = nullptr;
    other.vc = nullptr;
  }
  return *this;
}

TranslateToStpPass::~TranslateToStpPass() {
  // stpBuilder must be deleted BEFORE vc_Destroy because its destructor
  // calls vc_DeleteExpr on expressions created from this VC
  delete stpBuilder;
  // FIXME: vc_Destroy crashes due to ABI incompatibility with pre-built STP.
  // Memory leak acceptable for short-lived opt process.
  // if (vc) vc_Destroy(vc);
}

// Move the helper functions from anonymous namespace to class methods
PreservedAnalyses TranslateToStpPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  errs() << "Processing function: " << F.getName() << "\n";

  _F = &F;
  dataLayout = &(_F->getDataLayout());

  // Build value-to-block mapping for BDD guard lookup
  for (BasicBlock &bb : F)
    for (Instruction &inst : bb)
      valueToBlock[&inst] = &bb;

  bddBR->collectBranchInfo(_F);

  getOutputPort();
  getOutputKleeExpr();

  // Derive output filename from the input module name
  std::string outName = _F->getParent()->getModuleIdentifier();
  size_t slashPos = outName.find_last_of("/\\");
  if (slashPos != std::string::npos) outName = outName.substr(slashPos + 1);
  size_t dotPos = outName.find_last_of('.');
  if (dotPos != std::string::npos) outName = outName.substr(0, dotPos);
  outName += "_output.smt2";

  translateOutputToStp(outName);

  return PreservedAnalyses::all();
}
/**
 * two ways to register output
 * 1. by adding pointer or reference parameter to the top-level function.
 * 2. register the output using the registeration function(may need to process the input) 
 */
void TranslateToStpPass::getOutputPort() {
    BasicBlock &lastBlock = _F->back();
    Instruction &lastInst = lastBlock.back();

    if (_F->arg_size() == 0) {
      
      // user registeration function.
      // Need to find the registration function by keyword
      for (BasicBlock &bb: *_F) {
        for (Instruction &inst: bb) {
          if (auto *ci = dyn_cast<CallInst>(&inst)) {
            Function *calledFunc = ci->getCalledFunction();
            StringRef fName = calledFunc->getName();

            // void registerOutput(const char *name, void *ptr, int bitWith);
            // registerOutput("tmp_a", a, sizeof(a));
            if (fName.find("registerOutput") != StringRef::npos) {
              Value *outputName = ci->getArgOperand(0);
              Value *ptr = ci->getArgOperand(1);
              Value *origin = ptr;
              if (auto *bitCast = dyn_cast<BitCastInst>(ptr))
                origin = bitCast->getOperand(0);

              // Determine the pointed-to type and base for the output load
              Type *loadType = nullptr;
              if (auto *allocaInst = dyn_cast<AllocaInst>(origin)) {
                loadType = allocaInst->getAllocatedType();
              } else if (auto *gepInst = dyn_cast<GetElementPtrInst>(origin)) {
                // Struct field: getelementptr %Point, ptr %p, 0, fieldIdx
                loadType = gepInst->getResultElementType();
              }

              if (!loadType) {
                errs() << "Warning: registerOutput ptr is not an alloca or GEP\n";
                continue;
              }
              Type *type = loadType;

              // Store the user-specified output name for STP output
              std::string name = getStringFromValue(outputName).str();
              if (!name.empty()) {
                for (char &c : name)
                  if (!isalnum(c) && c != '_') c = '_';
                outputNames[origin] = name;
              }

              LoadInst *load = new LoadInst(type, origin, Twine("loadOutput"), InsertPosition(&inst));
              output[origin] = load;
            }
            else if (fName.find("registerInput") != StringRef::npos) {
              // Store the user-specified input name and size
              Value *riName = ci->getArgOperand(0);
              Value *riPtr = ci->getArgOperand(1);
              Value *riOrigin = riPtr;
              if (auto *bc = dyn_cast<BitCastInst>(riPtr))
                riOrigin = bc->getOperand(0);
              std::string iname = getStringFromValue(riName).str();
              if (!iname.empty()) {
                inputNames[riOrigin] = iname;
                if (auto *sizeCI = dyn_cast<ConstantInt>(ci->getArgOperand(2)))
                  inputSizes[riOrigin] = (unsigned)sizeCI->getZExtValue();
              }
            }
          }
        }
      }
    }
    else {
      
      // Need check pointer or referrnce variable.
      for (auto arg = _F->arg_begin(); arg != _F->arg_end(); arg++) {
        if (arg->getType()->isPointerTy()) {
          // find out output port
          auto *st = findStoreInstFromBasicBlock(lastBlock, arg);
          
          if (!st) {
            Type *type = arg->getType();
            LoadInst *load = new LoadInst(type, arg, Twine("loadArg"), InsertPosition(&lastInst));

            output[arg] = load;
          }
          else {
            if (auto *inst = dyn_cast<StoreInst>(st)) {
              output[arg] = inst->getPointerOperand();
            }
            else {
              errs() << "the output value is not store inst, need to check \n";
            }
          }
        }
      }
    }
}
/**
 * @note get StringRef from Value
 */
StringRef llvm::TranslateToStpPass::getStringFromValue(Value *v) {
  if (GlobalVariable *globalVar = dyn_cast<GlobalVariable>(v)) {
    // if (globalVar)
     if (globalVar->isConstant() && globalVar->hasInitializer()) {
        if (ConstantDataArray *constDataArray = dyn_cast<ConstantDataArray>(globalVar->getInitializer())) {
          if (constDataArray->isString()) {
            StringRef s = constDataArray->getAsString();
            while (!s.empty() && s.back() == '\0')
              s = s.drop_back();
            return s;
          }
        }
     }
  }

  return StringRef("");
} 

/**
   * @note find store instruction form 'bb' basic block by 'v' value.
   */
Instruction* TranslateToStpPass::findStoreInstFromBasicBlock(BasicBlock &bb, Value *v) {
  Instruction *ret = nullptr;

  for (Instruction &i : bb) {
    if (isa<StoreInst>(i)) {
      StoreInst *st = dyn_cast<StoreInst>(&i);
      Value *ptr = st->getPointerOperand();
      // Value *val = st->getOperand(0);
      if (ptr == v) {
        ret = &i;
      }
    }
  }

  return ret;
}

/**
 * @note Convert the value variable of the output port into a Klee expression
 */
void TranslateToStpPass::getOutputKleeExpr() {
  // Pre-pass: translate all registerInput calls so symbolic arrays are
  // created BEFORE any BDD conversion or output ITE chain building.
  // Without this, the first BDD conversion happens before arrays are
  // set up and caches wrong values (e.g. constant 0 instead of symbolic a).
  for (BasicBlock &bb : *_F) {
    for (Instruction &inst : bb) {
      if (auto *ci = dyn_cast<CallInst>(&inst)) {
        Function *cf = ci->getCalledFunction();
        if (cf && cf->getName().find("registerInput") != StringRef::npos) {
          translateRecursion(ci, exprBuilder->True(),
                            exprBuilder->Constant(0, klee::Expr::Int32));
        }
      }
    }
  }

  // First pass: translate all output loads to build ITE chains for all
  // output allocas. This ensures later outputs that read from earlier
  // outputs get the correct ITE-expressed values.
  for (auto &it: output) {
    translateInst(it.second);
  }

  // Clear the cache so the second pass recomputes now that ITE chains
  // are available for all output allocas.
  valueToKleeExprCache.clear();
  bddToKleeCache.clear();

  // Second pass: translate again and store the final expressions.
  for (auto &it: output) {
    outputKleeExpr[it.first] = translateInst(it.second);
  }
}

/**
 * @note 
 */
kleeExpr TranslateToStpPass::translateInst(Value *v) {
  kleeExpr guard = getGuardForValue(v);
  kleeExpr offset = exprBuilder->Constant(0, klee::Expr::Int32);

  kleeExpr ret = translateRecursion(v, guard, offset);

  return ret;
}

kleeExpr TranslateToStpPass::translateRecursion(Value *v, kleeExpr guard, kleeExpr offset) {
  static thread_local unsigned depth = 0;
  if (++depth > 1000) { --depth; return exprBuilder->Constant(0, 32); }
  if (valueToKleeExprCache.count(v)) { --depth; return valueToKleeExprCache[v]; }

  kleeExpr ret = nullptr;

  if (auto *constantInst = dyn_cast<ConstantInt>(v)) {
    unsigned bw = constantInst->getType()->getPrimitiveSizeInBits();
    // Use getZExtValue to avoid the APInt signed-constructor assertion
    // on negative values (e.g., i32 -1 as int64_t → uint64_t overflow).
    ret = exprBuilder->Constant(
        static_cast<uint64_t>(constantInst->getZExtValue()), bw);
  }
  else if (auto *callInst = dyn_cast<CallInst>(v)) {
    auto *calledFunc = callInst->getCalledFunction();
    if (!calledFunc) {
      errs() << "Warning: indirect call, returning 0\n";
      ret = exprBuilder->Constant(0, klee::Expr::Int32);
    } else {
      auto funcName = calledFunc->getName();
      if (funcName.find("registerInput") != StringRef::npos) {
        // registerInput(name, ptr, size) -- create symbolic array for the input
        Value *ptr = callInst->getArgOperand(1);
        auto *sizeCI = dyn_cast<ConstantInt>(callInst->getArgOperand(2));
        unsigned size = sizeCI ? sizeCI->getZExtValue() : 4;

        std::string inputName = getStringFromValue(callInst->getArgOperand(0)).str();
        if (inputName.empty())
          inputName = "input_" + std::to_string(symbolicVarIndex++);

        // Create a single wide element array (e.g. 32-bit for int) instead
        // of a byte array. This way a single Read returns the full value,
        // and STP output uses the clean variable name directly.
        unsigned bitWidth = size * 8;
        const klee::Array *array = arrayCache->CreateArray(inputName, 1,
            nullptr, nullptr, klee::Expr::Int32, bitWidth);
        memoryArrays[ptr] = array;
        memoryUpdateLists.insert_or_assign(ptr, std::make_unique<klee::UpdateList>(array, nullptr));

        // Track the underlying alloca/GEP to redirect later loads/stores
        if (auto *bitCast = dyn_cast<BitCastInst>(ptr)) {
          Value *origin = bitCast->getOperand(0);
          memoryArrays[origin] = array;
          memoryUpdateLists.insert_or_assign(origin, std::make_unique<klee::UpdateList>(array, nullptr));
          // If origin is an alloca (struct base), find GEPs targeting it
          // at offset 0 and map them to this array (struct field access).
          if (isa<AllocaInst>(origin)) {
            for (BasicBlock &sbb : *_F)
              for (Instruction &si : sbb)
                if (auto *gep = dyn_cast<GetElementPtrInst>(&si))
                  if (gep->getPointerOperand() == origin && gep->hasAllZeroIndices()) {
                    memoryArrays[gep] = array;
                    memoryUpdateLists.insert_or_assign(gep, std::make_unique<klee::UpdateList>(array, nullptr));
                  }
          }
          // If origin is a GEP (struct field like p.x), find other GEPs
          // targeting the same struct field so that loads via different
          // GEP instructions still find the registerInput array.
          if (auto *originGEP = dyn_cast<GetElementPtrInst>(origin)) {
            Value *base = originGEP->getPointerOperand();
            SmallVector<Value *, 4> idxList;
            for (auto &idx : originGEP->indices())
              idxList.push_back(idx);
            for (BasicBlock &sbb : *_F)
              for (Instruction &si : sbb)
                if (auto *gep = dyn_cast<GetElementPtrInst>(&si))
                  if (gep != originGEP &&
                      gep->getPointerOperand() == base &&
                      gep->getNumIndices() == idxList.size()) {
                    bool same = true;
                    for (unsigned i = 0; i < idxList.size(); i++)
                      if (gep->getOperand(1 + i) != idxList[i]) {
                        same = false; break;
                      }
                    if (same) {
                      memoryArrays[gep] = array;
                      memoryUpdateLists.insert_or_assign(gep, std::make_unique<klee::UpdateList>(array, nullptr));
                    }
                  }
          }
        }

        ret = exprBuilder->Constant(0, klee::Expr::Int32);
      } else if (funcName.find("registerOutput") != StringRef::npos) {
        // registerOutput is handled in getOutputPort(); here it's a no-op
        ret = exprBuilder->Constant(0, klee::Expr::Int32);
      } else if (funcName.find("llvm.dbg.") != StringRef::npos ||
                 funcName.find("llvm.lifetime.") != StringRef::npos) {
        // Ignore debug and lifetime intrinsics
        ret = exprBuilder->Constant(0, klee::Expr::Int32);
      } else {
        // For unknown function calls, return a fresh symbolic variable
        errs() << "Warning: unhandled function call: " << funcName << "\n";
        Type *retType = callInst->getType();
        if (retType->isVoidTy()) {
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
        } else {
          unsigned width = retType->getPrimitiveSizeInBits();
          if (width == 0) width = 32;
          std::string symName = "call_" + funcName.str() + "_" +
                                std::to_string(symbolicVarIndex++);
          const klee::Array *array = arrayCache->CreateArray(symName,
              (width + 7) / 8, nullptr, nullptr, klee::Expr::Int32, klee::Expr::Int8);
          klee::UpdateList ul(array, nullptr);
          if (width <= 8) {
            ret = exprBuilder->Read(ul, exprBuilder->Constant(0, klee::Expr::Int32));
          } else {
            ret = nullptr;
            for (unsigned i = 0; i < (width / 8); i++) {
              kleeExpr byteExpr = exprBuilder->Read(ul,
                  exprBuilder->Constant(i, klee::Expr::Int32));
              ret = (i == 0) ? byteExpr : exprBuilder->Concat(ret, byteExpr);
            }
          }
        }
      }
    }
  }
  else if (auto *arg = dyn_cast<Argument>(v)) {
    if (argumentExprs.count(arg)) {
      ret = argumentExprs[arg];
    } else {
      Type *argType = arg->getType();
      if (argType->isPointerTy()) {
        // Pointer arguments are treated as base address 0
        ret = exprBuilder->Constant(0, klee::Expr::Int32);
      } else {
        unsigned width = argType->getPrimitiveSizeInBits();
        if (width == 0) width = 32;
        std::string varName = arg->getName().str();
        if (varName.empty())
          varName = "arg_" + std::to_string(arg->getArgNo());
        const klee::Array *array = arrayCache->CreateArray(varName,
            (width + 7) / 8, nullptr, nullptr, klee::Expr::Int32, klee::Expr::Int8);
        klee::UpdateList ul(array, nullptr);
        unsigned numBytes = (width + 7) / 8;
        ret = nullptr;
        for (unsigned i = 0; i < numBytes; i++) {
          kleeExpr byteExpr = exprBuilder->Read(ul,
              exprBuilder->Constant(i, klee::Expr::Int32));
          ret = (i == 0) ? byteExpr : exprBuilder->Concat(ret, byteExpr);
        }
        if (!ret)
          ret = exprBuilder->Constant(0, width);
        argumentExprs[arg] = ret;
      }
    }
  }
  else if (auto *globalVar = dyn_cast<GlobalVariable>(v)) {
    if (globalVarExprs.count(globalVar)) {
      ret = globalVarExprs[globalVar];
    } else {
      Type *globalType = globalVar->getValueType();
      unsigned size = dataLayout->getTypeAllocSize(globalType);
      std::string name = globalVar->getName().str();
      if (name.empty())
        name = "global_" + std::to_string(symbolicVarIndex++);

      const klee::Array *array;
      if (globalVar->hasInitializer()) {
        // Extract raw bytes from constant initializer
        auto *init = globalVar->getInitializer();
        std::vector<unsigned char> rawBytes(size, 0);
        if (auto *ca = dyn_cast<ConstantArray>(init)) {
          unsigned byteIdx = 0;
          for (unsigned i = 0; i < ca->getNumOperands() && byteIdx < size; i++) {
            if (auto *ci = dyn_cast<ConstantInt>(ca->getOperand(i))) {
              uint64_t val = ci->getZExtValue();
              unsigned elemSize = dataLayout->getTypeAllocSize(ci->getType());
              for (unsigned b = 0; b < elemSize && byteIdx < size; b++, byteIdx++)
                rawBytes[byteIdx] = (unsigned char)((val >> (b * 8)) & 0xFF);
            }
          }
        } else if (auto *cd = dyn_cast<ConstantDataSequential>(init)) {
          for (unsigned i = 0; i < cd->getNumElements() && i < size; i++)
            rawBytes[i] = (unsigned char)(cd->getElementAsInteger(i) & 0xFF);
        }
        // Create KLEE ConstantExpr for each byte
        std::vector<klee::ref<klee::ConstantExpr>> constVals;
        for (unsigned i = 0; i < size; i++)
          constVals.push_back(klee::ConstantExpr::alloc(rawBytes[i], klee::Expr::Int8));
        array = arrayCache->CreateArray(name, size,
            constVals.data(), constVals.data() + constVals.size(),
            klee::Expr::Int32, klee::Expr::Int8);
        // Write each byte as a concrete update so reads find constant values
        auto ul = std::make_unique<klee::UpdateList>(array, nullptr);
        for (unsigned i = 0; i < size; i++)
          ul->extend(klee::ConstantExpr::alloc(i, klee::Expr::Int32),
                     klee::ConstantExpr::alloc(rawBytes[i], klee::Expr::Int8));
        memoryArrays[globalVar] = array;
        memoryUpdateLists.insert_or_assign(globalVar, std::move(ul));
      } else {
        array = arrayCache->CreateArray(name, size,
            nullptr, nullptr, klee::Expr::Int32, klee::Expr::Int8);
        memoryArrays[globalVar] = array;
        memoryUpdateLists.insert_or_assign(globalVar, std::make_unique<klee::UpdateList>(array, nullptr));
      }

      // Return the base address (0) for the global pointer
      ret = exprBuilder->Constant(0, klee::Expr::Int32);
      globalVarExprs[globalVar] = ret;
    }
  }
  else if (auto *inst = dyn_cast<Instruction>(v)) {
    switch (inst->getOpcode()) {
      case Instruction::Add:
      case Instruction::Sub:
      case Instruction::Mul:
      case Instruction::UDiv:
      case Instruction::SDiv:
      case Instruction::URem:
      case Instruction::SRem: {
        kleeExpr left = translateRecursion(inst->getOperand(0), guard, offset);
        kleeExpr right = translateRecursion(inst->getOperand(1), guard, offset);
        switch (inst->getOpcode()) {
          case Instruction::Add:
            ret = exprBuilder->Add(left, right);
            break;
          case Instruction::Sub:
            ret = exprBuilder->Sub(left, right);
            break;
          case Instruction::Mul:
            ret = exprBuilder->Mul(left, right);
            break;
          case Instruction::UDiv:
            ret = exprBuilder->UDiv(left, right);
            break;
          case Instruction::SDiv:
            ret = exprBuilder->SDiv(left, right);
            break;
          case Instruction::URem:
            ret = exprBuilder->URem(left, right);
            break;
          case Instruction::SRem:
            ret = exprBuilder->SRem(left, right);
            break;
          default:
            assert(false && "Unsupported arithmetic operation");
        }
        break;
      }
      case Instruction::ICmp: {
        auto *icmpInst = dyn_cast<ICmpInst>(inst);
        kleeExpr left = translateRecursion(icmpInst->getOperand(0), guard, offset);
        kleeExpr right = translateRecursion(icmpInst->getOperand(1), guard, offset);
        // Use KLEE canonical forms: STPBuilder only handles Eq/Slt/Sle/Ult/Ule.
        // Non-canonical predicates are rewritten:
        //   Ne(a,b)→Not(Eq(a,b))  Sgt(a,b)→Slt(b,a)  Sge(a,b)→Sle(b,a)
        //   Ugt(a,b)→Ult(b,a)    Uge(a,b)→Ule(b,a)
        switch (icmpInst->getPredicate()) {
          case ICmpInst::ICMP_EQ:
            ret = exprBuilder->Eq(left, right);
            break;
          case ICmpInst::ICMP_NE:
            ret = exprBuilder->Not(exprBuilder->Eq(left, right));
            break;
          case ICmpInst::ICMP_SLT:
            ret = exprBuilder->Slt(left, right);
            break;
          case ICmpInst::ICMP_SLE:
            ret = exprBuilder->Sle(left, right);
            break;
          case ICmpInst::ICMP_SGT:
            ret = exprBuilder->Slt(right, left);
            break;
          case ICmpInst::ICMP_SGE:
            ret = exprBuilder->Sle(right, left);
            break;
          case ICmpInst::ICMP_ULT:
            ret = exprBuilder->Ult(left, right);
            break;
          case ICmpInst::ICMP_ULE:
            ret = exprBuilder->Ule(left, right);
            break;
          case ICmpInst::ICMP_UGT:
            ret = exprBuilder->Ult(right, left);
            break;
          case ICmpInst::ICMP_UGE:
            ret = exprBuilder->Ule(right, left);
            break;
          default:
            assert(false && "Unsupported ICmp predicate");
        }
        break;
      }
      case Instruction::And:
      case Instruction::Or:
      case Instruction::Xor:
      case Instruction::Shl:
      case Instruction::LShr:
      case Instruction::AShr: {
        kleeExpr left = translateRecursion(inst->getOperand(0), guard, offset);
        kleeExpr right = translateRecursion(inst->getOperand(1), guard, offset);
        switch (inst->getOpcode()) {
          case Instruction::And:
            ret = exprBuilder->And(left, right);
            break;
          case Instruction::Or:
            ret = exprBuilder->Or(left, right);
            break;
          case Instruction::Xor:
            ret = exprBuilder->Xor(left, right);
            break;
          case Instruction::Shl:
            ret = exprBuilder->Shl(left, right);
            break;
          case Instruction::LShr:
            ret = exprBuilder->LShr(left, right);
            break;
          case Instruction::AShr:
            ret = exprBuilder->AShr(left, right);
            break;
          default:
            assert(false && "Unsupported bitwise operation");
        }
        break;
      }
      case Instruction::ZExt:
      case Instruction::Trunc:
      case Instruction::SExt: {
        auto *castInst = dyn_cast<CastInst>(inst);
        kleeExpr operand = translateRecursion(castInst->getOperand(0), guard, offset);
        unsigned toWidth = castInst->getType()->getPrimitiveSizeInBits();
        switch (inst->getOpcode()) {
          case Instruction::Trunc:
            // Extract low bits for truncation
            ret = exprBuilder->Extract(operand, 0, toWidth);
            break;
          case Instruction::ZExt:
            ret = exprBuilder->ZExt(operand, toWidth);
            break;
          case Instruction::SExt:
            ret = exprBuilder->SExt(operand, toWidth);
            break;
          default:
            assert(false && "Unsupported cast operation");
        }
        break;
      }
      case Instruction::Select: {
        auto *selectInst = dyn_cast<SelectInst>(inst);
        kleeExpr cond = translateRecursion(selectInst->getCondition(), guard, offset);
        kleeExpr trueVal = translateRecursion(selectInst->getTrueValue(), guard, offset);
        kleeExpr falseVal = translateRecursion(selectInst->getFalseValue(), guard, offset);
        // Ensure cond is boolean
        if (cond->getWidth() != klee::Expr::Bool)
          cond = exprBuilder->Ne(cond, exprBuilder->Constant(0, cond->getWidth()));
        ret = exprBuilder->Select(cond, trueVal, falseVal);
        break;
      }
      case Instruction::Alloca: {
        auto *allocaInst = dyn_cast<AllocaInst>(inst);
        // If already registered (e.g., via registerInput for struct field),
        // don't overwrite with a generic byte array.
        if (memoryUpdateLists.count(allocaInst)) {
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
          break;
        }
        Type *allocatedType = allocaInst->getAllocatedType();
        unsigned allocSize = dataLayout->getTypeAllocSize(allocatedType);
        if (allocSize == 0) allocSize = 1;

        // Use a stable counter-based name so the SMT2 output is
        // deterministic and auto-declare works correctly.
        std::string arrayName = "alloca_" + std::to_string(symbolicVarIndex++);

        const klee::Array *array = arrayCache->CreateArray(arrayName, allocSize,
            nullptr, nullptr, klee::Expr::Int32, klee::Expr::Int8);
        memoryArrays[allocaInst] = array;
        memoryUpdateLists.insert_or_assign(allocaInst, std::make_unique<klee::UpdateList>(array, nullptr));

        ret = exprBuilder->Constant(0, klee::Expr::Int32);
        break;
      }
      case Instruction::Store: {
        auto *storeInst = dyn_cast<StoreInst>(inst);
        Value *ptr = storeInst->getPointerOperand();
        Value *val = storeInst->getValueOperand();

        // Resolve pointer to the base allocation
        Value *basePtr = ptr;
        kleeExpr byteOffset = exprBuilder->Constant(0, klee::Expr::Int32);

        // Extract base pointer from GEP (instruction or constant expression)
        if (auto *gepInst = dyn_cast<GetElementPtrInst>(ptr)) {
          basePtr = gepInst->getPointerOperand();
          byteOffset = translateRecursion(gepInst, guard, offset);
        } else if (auto *ce = dyn_cast<ConstantExpr>(ptr)) {
          if (ce->getOpcode() == Instruction::GetElementPtr) {
            basePtr = ce->getOperand(0);
            // Compute byte offset via DataLayout
            Type *srcTy = cast<GEPOperator>(ce)->getSourceElementType();
            SmallVector<Value *, 4> indices;
            for (unsigned idx = 1; idx < ce->getNumOperands(); idx++)
              indices.push_back(ce->getOperand(idx));
            uint64_t off = dataLayout->getIndexedOffsetInType(srcTy, indices);
            byteOffset = exprBuilder->Constant(off, klee::Expr::Int32);
          }
        }

        // Track pointer assignments: when storing an alloca address to
        // a pointer alloca, record the mapping so later *p loads can
        // resolve to the correct target.
        if (isa<AllocaInst>(val) || isa<GetElementPtrInst>(val) ||
            isa<Argument>(val) || isa<GlobalVariable>(val)) {
          pointerTargets[basePtr] = val;
        }

        if (memoryUpdateLists.count(basePtr)) {
          kleeExpr valExpr = translateRecursion(val, guard, offset);
          unsigned arrayRange = memoryUpdateLists.at(basePtr)->root->getRange();

          if (arrayRange > 8) {
            // Wide array: write at the GEP byte offset so different
            // struct fields (offset 0 vs 4) use separate update slots.
            memoryUpdateLists.at(basePtr)->extend(byteOffset, valExpr);
          } else {
            unsigned storeSize = dataLayout->getTypeStoreSize(val->getType());
            if (storeSize == 0) storeSize = (val->getType()->getPrimitiveSizeInBits() + 7) / 8;

            // Decompose into byte writes
            for (unsigned i = 0; i < storeSize; i++) {
              kleeExpr byteIndex;
              if (byteOffset->getKind() == klee::Expr::Constant) {
                unsigned off = static_cast<const klee::ConstantExpr *>(
                    byteOffset.get())->getAPValue().getZExtValue();
                byteIndex = exprBuilder->Constant(off + i, klee::Expr::Int32);
              } else {
                byteIndex = exprBuilder->Add(byteOffset,
                    exprBuilder->Constant(i, klee::Expr::Int32));
              }
              kleeExpr byteValue;
              if (storeSize == 1 && valExpr->getWidth() <= 8) {
                byteValue = valExpr;
              } else {
                byteValue = exprBuilder->Extract(valExpr, i * 8, klee::Expr::Int8);
              }
              memoryUpdateLists.at(basePtr)->extend(byteIndex, byteValue);
            }
          }
          ret = valExpr;
        } else {
          errs() << "Warning: Store to unknown pointer\n";
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
        }
        break;
      }
      case Instruction::Load: {
        auto *loadInst = dyn_cast<LoadInst>(inst);
        Value *ptr = loadInst->getPointerOperand();

        // Resolve pointer dereference: if ptr is a loaded pointer value
        // (e.g. %p_loaded = load ptr, ptr %p_alloca), look up its
        // target alloca from the pointerTargets map.
        if (!isa<AllocaInst>(ptr) && !isa<GetElementPtrInst>(ptr) &&
            !isa<GlobalVariable>(ptr) && !isa<ConstantExpr>(ptr)) {
          if (pointerTargets.count(ptr)) {
            ptr = pointerTargets[ptr];
            // Propagate the mapping through this load: the load result
            // also points to the same target.
            pointerTargets[inst] = ptr;
          }
        }

        Value *basePtr = ptr;
        kleeExpr byteOffset = exprBuilder->Constant(0, klee::Expr::Int32);

        // Extract base pointer from GEP (instruction or constant expression)
        if (auto *gepInst = dyn_cast<GetElementPtrInst>(ptr)) {
          basePtr = gepInst->getPointerOperand();
          byteOffset = translateRecursion(gepInst, guard, offset);
        } else if (auto *ce = dyn_cast<ConstantExpr>(ptr)) {
          if (ce->getOpcode() == Instruction::GetElementPtr) {
            basePtr = ce->getOperand(0);
            // Compute byte offset via DataLayout
            Type *srcTy = cast<GEPOperator>(ce)->getSourceElementType();
            SmallVector<Value *, 4> indices;
            for (unsigned idx = 1; idx < ce->getNumOperands(); idx++)
              indices.push_back(ce->getOperand(idx));
            uint64_t off = dataLayout->getIndexedOffsetInType(srcTy, indices);
            byteOffset = exprBuilder->Constant(off, klee::Expr::Int32);
          }
        }

        // Fast path: constant global variable access — return value directly
        if (auto *gv = dyn_cast<GlobalVariable>(basePtr)) {
          if (gv->hasInitializer() && byteOffset->getKind() == klee::Expr::Constant) {
            uint64_t off = static_cast<const klee::ConstantExpr *>(
                byteOffset.get())->getAPValue().getZExtValue();
            unsigned bitWidth = loadInst->getType()->getPrimitiveSizeInBits();
            if (bitWidth > 0 && off + (bitWidth / 8) <= dataLayout->getTypeAllocSize(gv->getValueType())) {
              // Read bytes directly from the global initializer
              uint64_t result = 0;
              auto *init = gv->getInitializer();
              // Handle constant arrays of simple integers (ConstantDataArray)
              if (auto *cds = dyn_cast<ConstantDataSequential>(init)) {
                Type *elemTy = cds->getElementType();
                unsigned elemSize = dataLayout->getTypeAllocSize(elemTy);
                unsigned elemIdx = off / elemSize;
                unsigned byteInElem = off % elemSize;
                if (elemIdx < cds->getNumElements() && byteInElem == 0)
                  result = cds->getElementAsInteger(elemIdx);
              } else if (auto *ca = dyn_cast<ConstantArray>(init)) {
                // More complex array types
                Type *elemTy = ca->getType()->getElementType();
                unsigned elemSize = dataLayout->getTypeAllocSize(elemTy);
                unsigned elemIdx = off / elemSize;
                unsigned byteInElem = off % elemSize;
                if (elemIdx < ca->getNumOperands() && byteInElem == 0) {
                  if (auto *ci = dyn_cast<ConstantInt>(ca->getOperand(elemIdx)))
                    result = ci->getZExtValue();
                }
              }
              ret = exprBuilder->Constant(result, bitWidth);
              break;
            }
          }
        }

        // Lazy initialization: create arrays for allocas/globals and
        // process stores/registerInput calls. If the GEP handler already
        // created a byte array, check if a registerInput should override it.
        if (isa<AllocaInst>(basePtr) || isa<GlobalVariable>(basePtr)) {
          bool alreadyExists = memoryUpdateLists.count(basePtr) != 0;
          // First, check for a registerInput call targeting this alloca.
          // If found, use the user-specified name for the symbolic array.
          bool hasSpecialArray = alreadyExists;
          for (BasicBlock &bb : *_F) {
            for (Instruction &bbInst : bb) {
              if (auto *ci = dyn_cast<CallInst>(&bbInst)) {
                Function *cf = ci->getCalledFunction();
                if (cf && cf->getName().find("registerInput") != StringRef::npos) {
                  Value *riPtr = ci->getArgOperand(1);
                  Value *riBase = riPtr;
                  if (auto *bc = dyn_cast<BitCastInst>(riPtr))
                    riBase = bc->getOperand(0);
                  // Match: direct alloca, or GEP whose base is the alloca
                  bool matches = (riBase == basePtr);
                  if (!matches)
                    if (auto *riGEP = dyn_cast<GetElementPtrInst>(riBase))
                      matches = (riGEP->getPointerOperand() == basePtr);
                  if (matches) {
                    translateRecursion(ci, guard, offset);
                    hasSpecialArray = true;
                  }
                }
              }
            }
          }
          // Find output name — may be direct or via GEP (struct field).
          std::string outName;
          Value *outKey = basePtr;
          unsigned outBitW = 0;
          if (outputNames.count(basePtr)) {
            outName = outputNames[basePtr];
            if (auto *ai = dyn_cast<AllocaInst>(basePtr))
              outBitW = dataLayout->getTypeAllocSize(ai->getAllocatedType()) * 8;
          } else {
            // Check if any GEP targeting this alloca is an output
            for (auto &on : outputNames) {
              if (auto *gep = dyn_cast<GetElementPtrInst>(on.first)) {
                if (gep->getPointerOperand() == basePtr) {
                  outName = on.second;
                        outBitW = gep->getResultElementType()->getPrimitiveSizeInBits();
                  break;
                }
              }
            }
          }
          // If this alloca/GEP is an output, create a wide array.
          if (!hasSpecialArray && !outName.empty()) {
            const klee::Array *array = arrayCache->CreateArray(
                outName, 1, nullptr, nullptr, klee::Expr::Int32, outBitW);
            memoryArrays[outKey] = array;
            memoryUpdateLists.insert_or_assign(
                outKey, std::make_unique<klee::UpdateList>(array, nullptr));
            hasSpecialArray = true;

            // Build guarded ITE chain for output allocas with stores from
            // multiple basic blocks, using BDD path conditions.
            // Process sequentially so later stores that READ from this
            // alloca see the accumulated ITE from earlier stores.
            kleeExpr result = exprBuilder->Constant(0, outBitW);
            for (BasicBlock &bb : *_F) {
              for (Instruction &bbInst : bb) {
                if (auto *si = dyn_cast<StoreInst>(&bbInst)) {
                  Value *siPtr = si->getPointerOperand();
                  Value *siBase = siPtr;
                  if (auto *gep = dyn_cast<GetElementPtrInst>(siPtr))
                    siBase = gep->getPointerOperand();
                  if (siBase == basePtr) {
                    // Write current ITE so loads from this alloca see it
                    memoryUpdateLists.at(basePtr)->extend(
                        exprBuilder->Constant(0, klee::Expr::Int32), result);
                    // Get guard and translate value (loads from this alloca
                    // will read the current ITE via the UpdateList)
                    kleeExpr storeGuard = exprBuilder->True();
                    if (bddBR->basicBlockBdd.count(&bb)) {
                      bdd blockBdd = bddBR->basicBlockBdd[&bb];
                      storeGuard = convertBddToKleeExpr(blockBdd);
                    }
                    kleeExpr valExpr = translateRecursion(
                        si->getValueOperand(), guard, offset);
                    // Later stores override: ite(g_i, v_i, result)
                    // Skip unnecessary ITE when guard is trivially true
                    if (storeGuard->getKind() == klee::Expr::Constant) {
                      auto &apv = static_cast<const klee::ConstantExpr *>(
                          storeGuard.get())->getAPValue();
                      if (apv == 1)
                        result = valExpr;
                      else
                        result = exprBuilder->Select(storeGuard, valExpr, result);
                    } else {
                      result = exprBuilder->Select(storeGuard, valExpr, result);
                    }
                  }
                }
              }
            }
            // Write the final accumulated ITE expression
            memoryUpdateLists.at(basePtr)->extend(
                exprBuilder->Constant(0, klee::Expr::Int32), result);
          }
          // Otherwise create a default byte-level array from the alloca
          if (!hasSpecialArray)
            translateRecursion(basePtr, guard, offset);
          // For non-output allocas: build a guarded ITE chain so that
          // stores from different control-flow paths are selected
          // correctly based on BDD path conditions.  The old flat-store
          // approach (extending the UpdateList unconditionally) caused
          // stores from unreachable paths to shadow the initial symbolic
          // value, producing constant 0 instead of the symbolic variable.
          // Skip if this alloca is already having its ITE chain built
          // (recursive call from a store's value operand that reads the
          // same alloca — see BuildingITEChains guard above).
          // For registerInput allocas where the input is the alloca
          // ITSELF (not a GEP field of a struct), skip the ITE chain
          // entirely to avoid circular BDD dependencies.  Struct fields
          // via GEP (hasSpecialArray set by GEP matching) still need
          // ITE chains for correct multi-block store tracking.
          bool isDirectRegisterInput = false;
          if (hasSpecialArray) {
            for (BasicBlock &bb : *_F)
              for (Instruction &bbInst : bb)
                if (auto *ci = dyn_cast<CallInst>(&bbInst))
                  if (ci->getCalledFunction() &&
                      ci->getCalledFunction()->getName().find("registerInput") != StringRef::npos)
                    if (auto *bc = dyn_cast<BitCastInst>(ci->getArgOperand(1)))
                      if (bc->getOperand(0) == basePtr &&
                          isa<AllocaInst>(bc->getOperand(0)))
                        { isDirectRegisterInput = true; break; }
          }
          if (!outputNames.count(basePtr) &&
              !BuildingITEChains.count(basePtr) &&
              !isDirectRegisterInput) {
            // Collect all blocks that store to this alloca
            SmallVector<std::pair<BasicBlock *, StoreInst *>, 8> storesToAlloca;
            for (BasicBlock &bb : *_F) {
              for (Instruction &bbInst : bb) {
                if (auto *si = dyn_cast<StoreInst>(&bbInst)) {
                  Value *siPtr = si->getPointerOperand();
                  Value *siBase = siPtr;
                  if (auto *gep = dyn_cast<GetElementPtrInst>(siPtr))
                    siBase = gep->getPointerOperand();
                  if (siBase == basePtr) {
                    storesToAlloca.push_back({&bb, si});
                  }
                }
              }
            }
            // If there are stores from multiple *different* blocks, build
            // an ITE chain with BDD guards.  Single-block stores (e.g.
            // struct initializer stores in the entry block) use the old
            // flat behavior which is correct for non-path-dependent values.
            SmallPtrSet<BasicBlock *, 4> storeBlocks;
            for (auto &[bb, si] : storesToAlloca)
              storeBlocks.insert(bb);
            bool multiBlockStores = storeBlocks.size() > 1;
            bool isWideArray = memoryUpdateLists.count(basePtr) &&
                memoryUpdateLists.at(basePtr)->root->getRange() > 8;

            if (multiBlockStores) {
              unsigned bitWidth = 32;
              if (auto *ai = dyn_cast<AllocaInst>(basePtr))
                bitWidth = dataLayout->getTypeAllocSize(ai->getAllocatedType()) * 8;

              // Mark this alloca as "under ITE construction" so that
              // recursive loads (store values that read from the same
              // alloca) use the root array instead of triggering another
              // ITE chain build, preventing exponential formula blowup.
              BuildingITEChains.insert(basePtr);

              // Build the guarded ITE chain over all stores.
              kleeExpr result = exprBuilder->Constant(0, bitWidth);
              for (auto &[bb, si] : storesToAlloca) {
                kleeExpr storeGuard = exprBuilder->True();
                if (bddBR->basicBlockBdd.count(bb)) {
                  bdd blockBdd = bddBR->basicBlockBdd[bb];
                  storeGuard = convertBddToKleeExpr(blockBdd);
                }
                kleeExpr valExpr = translateRecursion(
                    si->getValueOperand(), guard, offset);
                if (valExpr->getWidth() != bitWidth) {
                  if (valExpr->getWidth() < bitWidth)
                    valExpr = exprBuilder->ZExt(valExpr, bitWidth);
                  else
                    valExpr = exprBuilder->Extract(valExpr, 0, bitWidth);
                }
                // ite(guard, val, previous_result)
                if (storeGuard->getKind() == klee::Expr::Constant) {
                  auto &apv = static_cast<const klee::ConstantExpr *>(
                      storeGuard.get())->getAPValue();
                  if (apv == 1)
                    result = valExpr;
                  else
                    result = exprBuilder->Select(storeGuard, valExpr, result);
                } else {
                  result = exprBuilder->Select(storeGuard, valExpr, result);
                }
              }

              if (isWideArray) {
                // Wide-element array: write the ITE expression as a single
                // update covering the whole value.
                memoryUpdateLists.at(basePtr)->extend(
                    exprBuilder->Constant(0, klee::Expr::Int32), result);
              } else {
                // Byte-level array: decompose the ITE result into
                // individual bytes and extend each byte index separately.
                unsigned allocSize = bitWidth / 8;
                if (allocSize == 0) allocSize = 1;
                for (unsigned i = 0; i < allocSize; i++) {
                  kleeExpr byteVal = exprBuilder->Extract(
                      result, i * 8, klee::Expr::Int8);
                  memoryUpdateLists.at(basePtr)->extend(
                      exprBuilder->Constant(i, klee::Expr::Int32), byteVal);
                }
              }

              BuildingITEChains.erase(basePtr);
            } else {
              // Single-block stores: flat translation is safe
              for (auto &[bb, si] : storesToAlloca)
                translateInst(si);
            }
          }
        }

        // For struct field loads via GEP, prefer the GEP's own array
        // if registered (e.g., registerInput for p.y maps to the GEP %y).
        Value *memKey = basePtr;
        if (auto *gepLd = dyn_cast<GetElementPtrInst>(loadInst->getPointerOperand()))
          if (memoryUpdateLists.count(gepLd))
            memKey = gepLd;

        if (memoryUpdateLists.count(memKey)) {
          klee::UpdateList &updates = *memoryUpdateLists.at(memKey);
          unsigned loadBitWidth = loadInst->getType()->getPrimitiveSizeInBits();
          if (loadBitWidth == 0) loadBitWidth = 32;

          // If this alloca is currently having its ITE chain built, read
          // directly from the root array to avoid recursive ITE explosion
          // (e.g. a = ashr a, 1 where store value reads the same alloca).
          if (BuildingITEChains.count(basePtr) ||
              BuildingITEChains.count(memKey)) {
            ret = exprBuilder->Read(
                klee::UpdateList(updates.root, nullptr),
                exprBuilder->Constant(0, klee::Expr::Int32));
            if (ret->getWidth() != loadBitWidth) {
              if (ret->getWidth() < loadBitWidth)
                ret = exprBuilder->ZExt(ret, loadBitWidth);
              else
                ret = exprBuilder->Extract(ret, 0, loadBitWidth);
            }
            valueToKleeExprCache[v] = ret;
            --depth;
            return ret;
          }

          // If the array has a wide range (e.g. Int32 from registerInput),
          // do a single read. Otherwise, do byte-by-byte reads.
          unsigned arrayRange = updates.root->getRange();
          if (arrayRange > 8) {
            ret = exprBuilder->Read(updates, byteOffset);
            if (ret->getWidth() != loadBitWidth) {
              if (ret->getWidth() < loadBitWidth)
                ret = exprBuilder->ZExt(ret, loadBitWidth);
              else
                ret = exprBuilder->Extract(ret, 0, loadBitWidth);
            }
          } else {
            unsigned loadSize = dataLayout->getTypeStoreSize(loadInst->getType());
            if (loadSize == 0)
              loadSize = (loadBitWidth + 7) / 8;
            if (loadSize == 0) loadSize = 1;

            // Read bytes and concatenate (little-endian: first byte is LSB)
            ret = nullptr;
            for (unsigned i = 0; i < loadSize; i++) {
              // Compute byte index with constant folding for matching updates
              kleeExpr byteIndex;
              if (byteOffset->getKind() == klee::Expr::Constant) {
                unsigned off = static_cast<const klee::ConstantExpr *>(
                    byteOffset.get())->getAPValue().getZExtValue();
                byteIndex = exprBuilder->Constant(off + i, klee::Expr::Int32);
              } else {
                byteIndex = exprBuilder->Add(byteOffset,
                    exprBuilder->Constant(i, klee::Expr::Int32));
              }
              kleeExpr byteVal = exprBuilder->Read(updates, byteIndex);
              if (i == 0)
                ret = byteVal;
              else
                ret = exprBuilder->Concat(ret, byteVal);
            }
          }
          if (!ret)
            ret = exprBuilder->Constant(0, loadBitWidth);
        } else {
          errs() << "Warning: Load from unknown pointer\n";
          unsigned width = loadInst->getType()->getPrimitiveSizeInBits();
          ret = exprBuilder->Constant(0, width > 0 ? width : 32);
        }
        break;
      }
      case Instruction::GetElementPtr: {
        auto *gepInst = dyn_cast<GetElementPtrInst>(inst);
        Value *ptrOperand = gepInst->getPointerOperand();

        // Translate the base pointer address
        kleeExpr baseAddr = translateRecursion(ptrOperand, guard, offset);

        // Compute cumulative byte offset from GEP indices
        kleeExpr cumOffset = exprBuilder->Constant(0, klee::Expr::Int32);

        for (auto it = gep_type_begin(gepInst), et = gep_type_end(gepInst);
             it != et; ++it) {
          Type *indexedType = it.getIndexedType();
          Value *indexVal = it.getOperand();

          if (it.isStruct()) {
            auto *structType = it.getStructType();
            auto *constIdx = dyn_cast<ConstantInt>(indexVal);
            if (constIdx) {
              unsigned structIdx = constIdx->getZExtValue();
              unsigned elemOffset =
                  dataLayout->getStructLayout(structType)->getElementOffset(structIdx);
              cumOffset = exprBuilder->Add(cumOffset,
                  exprBuilder->Constant(elemOffset, klee::Expr::Int32));
            } else {
              errs() << "Warning: non-constant struct GEP index\n";
            }
          } else {
            // Sequential (array/pointer/vector) index
            TypeSize stride = it.getSequentialElementStride(*dataLayout);
            unsigned elemSize = stride.getFixedValue();

            kleeExpr idxExpr = translateRecursion(indexVal, guard, offset);
            if (idxExpr->getWidth() < klee::Expr::Int32)
              idxExpr = exprBuilder->ZExt(idxExpr, klee::Expr::Int32);
            else if (idxExpr->getWidth() > klee::Expr::Int32)
              idxExpr = exprBuilder->Extract(idxExpr, 0, klee::Expr::Int32);

            if (elemSize > 1)
              idxExpr = exprBuilder->Mul(idxExpr,
                  exprBuilder->Constant(elemSize, klee::Expr::Int32));
            cumOffset = exprBuilder->Add(cumOffset, idxExpr);
          }
        }

        ret = exprBuilder->Add(baseAddr, cumOffset);
        break;
      }
      case Instruction::PHI: {
        // Loop-unroll pass runs before us, so all loops are already
        // unrolled.  Every PHI node is a regular multi-predecessor PHI
        // (e.g. from mem2reg after force-unroll).  We translate it by
        // accumulating BDD-guarded Select expressions for each incoming
        // edge — mutually exclusive guards ensure only one value is active.
        auto *phiInst = dyn_cast<PHINode>(inst);
        unsigned width = phiInst->getType()->getPrimitiveSizeInBits();
        if (width == 0) width = 32;
        BasicBlock *currentBB = phiInst->getParent();

        kleeExpr result = exprBuilder->Constant(0, width);
        for (unsigned i = 0; i < phiInst->getNumIncomingValues(); i++) {
          BasicBlock *incomingBB = phiInst->getIncomingBlock(i);
          Value *incomingVal = phiInst->getIncomingValue(i);

          bdd edgeCond = bddBR->getEdgeCondition(incomingBB, currentBB);
          kleeExpr condExpr = convertBddToKleeExpr(edgeCond);

          kleeExpr incomingExpr = translateRecursion(incomingVal, guard, offset);

          if (incomingExpr->getWidth() != width) {
            if (incomingExpr->getWidth() < width)
              incomingExpr = exprBuilder->ZExt(incomingExpr, width);
            else
              incomingExpr = exprBuilder->Extract(incomingExpr, 0, width);
          }

          kleeExpr guardedVal = exprBuilder->Select(condExpr, incomingExpr,
              exprBuilder->Constant(0, width));
          result = exprBuilder->Add(result, guardedVal);
        }

        ret = result;
        break;
      }
      default:
        errs() << "Unsupported instruction: " << *inst << "\n";
        assert(false && "Unsupported instruction type");
    }
  }

  if (!ret) {
    errs() << "Warning: translateRecursion returned null for: ";
    v->dump();
    errs() << "\n";
    unsigned width = 32;
    if (v->getType()->isSized())
      width = v->getType()->getPrimitiveSizeInBits();
    if (width == 0) width = 32;
    ret = exprBuilder->Constant(0, width);
  }

  valueToKleeExprCache[v] = ret;
  --depth;
  return ret;
}


kleeExpr TranslateToStpPass::convertBddToKleeExpr(bdd node) {
  if (node == bddtrue)
    return exprBuilder->True();
  if (node == bddfalse)
    return exprBuilder->False();

  // Cache by unique BDD node id
  int nodeKey = node.id();
  if (bddToKleeCache.count(nodeKey))
    return bddToKleeCache[nodeKey];

  int var = bdd_var(node);
  bdd low = bdd_low(node);
  bdd high = bdd_high(node);

  // Get the LLVM Value for this BDD variable
  kleeExpr varExpr;
  if (bddBR->bddValue.count(var)) {
    Value *condVal = bddBR->bddValue[var];
    varExpr = translateRecursion(condVal, exprBuilder->True(),
        exprBuilder->Constant(0, klee::Expr::Int32));
    if (varExpr->getWidth() != klee::Expr::Bool)
      varExpr = exprBuilder->Ne(varExpr, exprBuilder->Constant(0, varExpr->getWidth()));
  } else {
    varExpr = exprBuilder->False();
  }

  kleeExpr lowExpr = convertBddToKleeExpr(low);
  kleeExpr highExpr = convertBddToKleeExpr(high);

  // ITE(var, high, low)
  kleeExpr result = exprBuilder->Select(varExpr, highExpr, lowExpr);
  bddToKleeCache[nodeKey] = result;
  return result;
}

kleeExpr TranslateToStpPass::getGuardForValue(Value *v) {
  if (valueToBlock.count(v)) {
    BasicBlock *bb = valueToBlock[v];
    if (bddBR->basicBlockBdd.count(bb)) {
      bdd blockBdd = bddBR->basicBlockBdd[bb];
      return convertBddToKleeExpr(blockBdd);
    }
  }
  return exprBuilder->True();
}

void TranslateToStpPass::printSMTExpr(kleeExpr e, raw_ostream &os,
    const std::unordered_map<std::string, unsigned> &varWidths) {
  using namespace klee;
  Expr::Kind kind = e->getKind();

  // Constant
  if (kind == Expr::Constant) {
    const llvm::APInt &val =
        static_cast<const klee::ConstantExpr *>(e.get())->getAPValue();
    // 1-bit values are Booleans in KLEE; print true / false for SMT2.
    if (val.getBitWidth() == 1) {
      os << (val.isZero() ? "false" : "true");
      return;
    }
    llvm::SmallString<40> hexStr;
    val.toString(hexStr, 16, false);
    unsigned expectedChars = (val.getBitWidth() + 3) / 4;
    os << "#x";
    for (unsigned i = hexStr.size(); i < expectedChars; i++)
      os << '0';
    os << hexStr;
    return;
  }

  // ReadExpr → follow update chain, or print symbolic variable name
  if (kind == Expr::Read) {
    auto *re = static_cast<const klee::ReadExpr *>(e.get());
    // Walk the update chain: find the most recent write at this index
    const klee::UpdateNode *un = re->updates.head.get();
    while (un) {
      // Compare indices by APInt value if both are constants
      bool sameIndex = false;
      if (re->index->getKind() == Expr::Constant &&
          un->index->getKind() == Expr::Constant) {
        auto &v1 = static_cast<const klee::ConstantExpr *>(re->index.get())->getAPValue();
        auto &v2 = static_cast<const klee::ConstantExpr *>(un->index.get())->getAPValue();
        sameIndex = (v1 == v2);
      } else {
        sameIndex = (re->index == un->index); // pointer equality fallback
      }
      if (sameIndex) {
        printSMTExpr(un->value, os, varWidths);
        return;
      }
      un = un->next.get();
    }
    // No matching write: symbolic read.
    // For single-element arrays (registerInput): just print the name.
    // For multi-byte arrays: include byte offset for uniqueness.
    std::string fullName = re->updates.root->name;
    if (re->updates.root->getSize() > 1) {
      fullName += "_b";
      if (re->index->getKind() == Expr::Constant)
        fullName += std::to_string(
            static_cast<const klee::ConstantExpr *>(re->index.get())->getAPValue().getZExtValue());
      else
        fullName += std::to_string(re->index->hash());
    }
    os << fullName;
    undeclaredSmtArrays.insert(fullName);
    return;
  }

  // Select / ITE
  if (kind == Expr::Select) {
    auto *se = static_cast<const klee::SelectExpr *>(e.get());
    os << "(ite ";
    printSMTExpr(se->cond, os, varWidths); os << " ";
    printSMTExpr(se->trueExpr, os, varWidths); os << " ";
    printSMTExpr(se->falseExpr, os, varWidths); os << ")";
    return;
  }

  // Extract
  if (kind == Expr::Extract) {
    auto *ee = static_cast<const klee::ExtractExpr *>(e.get());
    unsigned top = ee->offset + ee->width - 1;
    os << "((_ extract " << top << " " << ee->offset << ") ";
    printSMTExpr(ee->expr, os, varWidths); os << ")";
    return;
  }

  // Concat — encode as (bvor (bvshl (zext byte) offset) ...) instead of
  // nested (concat ...) because STP's SMT2 parser mishandles concat when the
  // left/right operands have different bit-widths (e.g. concat(16bit, 8bit)).
  if (kind == Expr::Concat) {
    // Flatten the concat tree into a list of byte-width leaves
    SmallVector<kleeExpr, 8> leaves;
    std::function<void(kleeExpr)> flatten = [&](kleeExpr x) {
      if (x->getKind() == Expr::Concat) {
        auto *cc = static_cast<const klee::ConcatExpr *>(x.get());
        flatten(cc->getLeft());
        flatten(cc->getRight());
      } else {
        leaves.push_back(x);
      }
    };
    flatten(e);
    // Encode: bvor(bvshl(zext(byte0, W), 0), bvshl(zext(byte1, W), 8), ...)
    // where W = total bit-width of the concat result.
    unsigned totalW = e->getWidth();
    os << "(bvor ";
    for (unsigned i = 0; i < leaves.size(); i++) {
      if (i > 0) os << " ";
      unsigned shift = i * leaves[i]->getWidth();
      os << "(bvshl ((_ zero_extend " << (totalW - leaves[i]->getWidth()) << ") ";
      // For 8-bit operands, print directly; for wider operands the shift
      // from their position in the concat tree is already correct.
      printSMTExpr(leaves[i], os, varWidths);
      os << ") #x" << llvm::format_hex_no_prefix(shift, totalW / 4) << ")";
    }
    os << ")";
    return;
  }

  // ZExt
  if (kind == Expr::ZExt) {
    auto *ze = static_cast<const klee::ZExtExpr *>(e.get());
    unsigned ext = ze->width - ze->src->getWidth();
    os << "((_ zero_extend " << ext << ") ";
    printSMTExpr(ze->src, os, varWidths); os << ")";
    return;
  }

  // SExt
  if (kind == Expr::SExt) {
    auto *se = static_cast<const klee::SExtExpr *>(e.get());
    unsigned ext = se->width - se->src->getWidth();
    os << "((_ sign_extend " << ext << ") ";
    printSMTExpr(se->src, os, varWidths); os << ")";
    return;
  }

  // Boolean NOT
  if (kind == Expr::Not) {
    auto *ne = static_cast<const klee::NotExpr *>(e.get());
    os << "(not ";
    printSMTExpr(ne->expr, os, varWidths); os << ")";
    return;
  }

  // Binary expressions
  auto *be = static_cast<const klee::BinaryExpr *>(e.get());
  const char *op = nullptr;
  switch (kind) {
  case Expr::Add: op = "bvadd"; break;
  case Expr::Sub: op = "bvsub"; break;
  case Expr::Mul: op = "bvmul"; break;
  case Expr::UDiv: op = "bvudiv"; break;
  case Expr::SDiv: op = "bvsdiv"; break;
  case Expr::URem: op = "bvurem"; break;
  case Expr::SRem: op = "bvsrem"; break;
  case Expr::And: op = "bvand"; break;
  case Expr::Or:  op = "bvor";  break;
  case Expr::Xor: op = "bvxor"; break;
  case Expr::Shl: op = "bvshl"; break;
  case Expr::LShr: op = "bvlshr"; break;
  case Expr::AShr: op = "bvashr"; break;
  case Expr::Ne:  op = "distinct"; break;
  case Expr::Eq:  op = "=";     break;
  case Expr::Ult: op = "bvult"; break;
  case Expr::Ule: op = "bvule"; break;
  case Expr::Slt: op = "bvslt"; break;
  case Expr::Sle: op = "bvsle"; break;
  default: break;
  }
  if (op) {
    os << "(" << op << " ";
    printSMTExpr(be->left, os, varWidths);
    os << " ";
    printSMTExpr(be->right, os, varWidths);
    os << ")";
    return;
  }

  // Fallback
  os << "#x0 ;; unhandled kind: " << kind;
}

void TranslateToStpPass::translateOutputToStp(const std::string &outFileName) {
  errs() << "Writing SMT-LIB2 to " << outFileName << "...\n";

  // Build a map of variable name → bit-width for SMT-LIB2 declarations.
  std::unordered_map<std::string, unsigned> varWidths;
  for (auto &kv : inputNames) {
    // Use the stored size from registerInput(name, ptr, SIZE) if available
    if (inputSizes.count(kv.first))
      varWidths[kv.second] = inputSizes[kv.first] * 8;
    else if (auto *ai = dyn_cast<AllocaInst>(kv.first))
      varWidths[kv.second] = dataLayout->getTypeAllocSize(ai->getAllocatedType()) * 8;
  }
  for (auto &kv : outputNames) {
    if (output.count(kv.first) && output[kv.first])
      varWidths[kv.second] = output[kv.first]->getType()->getPrimitiveSizeInBits();
  }

  // Write SMT-LIB2 output
  std::error_code EC;
  llvm::raw_fd_ostream ofs(outFileName, EC);
  if (EC) {
    errs() << "Cannot open " << outFileName << ": " << EC.message() << "\n";
    return;
  }

  // First pass: generate assertions into a temp buffer so we can
  // discover auto-generated array names that need declare-fun entries.
  undeclaredSmtArrays.clear();
  std::string assertBuf;
  llvm::raw_string_ostream assertOS(assertBuf);
  for (auto &it: outputKleeExpr) {
    Value *v = it.first;
    kleeExpr e = it.second;
    if (!e) continue;

    std::string varName;
    if (outputNames.count(v))
      varName = outputNames[v];
    if (varName.empty())
      varName = v->getName().str();
    if (varName.empty()) continue;

    for (char &c : varName)
      if (!isalnum(c) && c != '_') c = '_';

    assertOS << "(assert (= " << varName << " ";
    printSMTExpr(e, assertOS, varWidths);
    assertOS << "))\n";
  }
  assertOS.flush();

  // Add bit-widths for undeclared arrays.
  // Byte-level arrays (with _b suffix) are 8-bit; others default to 32-bit.
  for (auto &name : undeclaredSmtArrays) {
    if (!varWidths.count(name)) {
      unsigned bw = (name.find("_b") != std::string::npos) ? 8 : 32;
      varWidths[name] = bw;
    }
  }

  // Header
  ofs << "(set-logic QF_BV)\n";
  ofs << "(set-info :source |generated by translateToStp pass|)\n\n";

  // Declare variables
  for (auto &vw : varWidths) {
    ofs << "(declare-fun " << vw.first << " () (_ BitVec " << vw.second << "))\n";
  }
  ofs << "\n";

  // Write the pre-built assertions
  ofs << assertBuf;

  // Print output variable names for logging
  for (auto &it: outputKleeExpr) {
    Value *v = it.first;
    if (!it.second) continue;
    std::string varName;
    if (outputNames.count(v))
      varName = outputNames[v];
    if (varName.empty())
      varName = v->getName().str();
    if (varName.empty()) continue;
    errs() << "SMT Variable: " << varName << "\n";
  }

  ofs << "\n(check-sat)\n";
  ofs.close();
}