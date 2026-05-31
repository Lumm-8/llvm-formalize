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
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

// Include std headers
#include <fcntl.h>
#include <fstream>
#include <map>
#include <regex>
#include <unistd.h>
#include <vector>

// Include KLEE headers
#include "klee/Expr/Expr.h"
#include "klee/Expr/ExprBuilder.h"
#include <llvm/Support/Debug.h>

// FIXME: need to change the include path to the correct one
// #include <stp/c_interface.h>

#include "klee/Solver/STPBuilder.h"
#include <stp/c_interface.h>
using namespace llvm;


void printValue(Value *v, StringRef s) {
  errs() << s ;
  v->dump();
  errs() << "\n";
}

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
        bdd preBdd = basicBlockBdd[preBasBlo];
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
  translateOutputToStp();

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

              // TODO: Need to change to find the store instruction related to it.
              // Because the current processing method may cause the origin to not be the allocaInst instruction.
              auto *allocaInst = dyn_cast<AllocaInst>(origin);
              if (!allocaInst) {
                errs() << "Warning: registerOutput ptr is not an alloca\n";
                continue;
              }
              Type *type = allocaInst->getAllocatedType();

              // Store the user-specified output name for STP output
              std::string name = getStringFromValue(outputName).str();
              if (!name.empty())
                outputNames[origin] = name;

              LoadInst *load = new LoadInst(type, origin, Twine("loadOutput"), InsertPosition(&inst));
              output[origin] = load;
            }
            else if (fName.find("registerInput") != StringRef::npos) {
              // Store the user-specified input name for STP output post-processing
              Value *riName = ci->getArgOperand(0);
              Value *riPtr = ci->getArgOperand(1);
              Value *riOrigin = riPtr;
              if (auto *bc = dyn_cast<BitCastInst>(riPtr))
                riOrigin = bc->getOperand(0);
              std::string iname = getStringFromValue(riName).str();
              if (!iname.empty())
                inputNames[riOrigin] = iname;
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
  if (valueToKleeExprCache.count(v)) {
    return valueToKleeExprCache[v];
  }

  kleeExpr ret = nullptr;

  if (auto *constantInst = dyn_cast<ConstantInt>(v)) {
    ret = exprBuilder->Constant(constantInst->getSExtValue(), 
                            constantInst->getType()->getPrimitiveSizeInBits());
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

        // Track the underlying alloca to redirect later loads/stores
        if (auto *bitCast = dyn_cast<BitCastInst>(ptr)) {
          Value *origin = bitCast->getOperand(0);
          memoryArrays[origin] = array;
          memoryUpdateLists.insert_or_assign(origin, std::make_unique<klee::UpdateList>(array, nullptr));
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
      const klee::Array *array = arrayCache->CreateArray(name, size,
          nullptr, nullptr, klee::Expr::Int32, klee::Expr::Int8);
      memoryArrays[globalVar] = array;
      memoryUpdateLists.insert_or_assign(globalVar, std::make_unique<klee::UpdateList>(array, nullptr));

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
      case Instruction::SDiv: {
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
        Type *allocatedType = allocaInst->getAllocatedType();
        unsigned allocSize = dataLayout->getTypeAllocSize(allocatedType);
        if (allocSize == 0) allocSize = 1;

        std::string arrayName = allocaInst->getName().str();
        if (arrayName.empty())
          arrayName = "alloca_" + std::to_string(symbolicVarIndex++);

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

        if (auto *gepInst = dyn_cast<GetElementPtrInst>(ptr)) {
          basePtr = gepInst->getPointerOperand();
          byteOffset = translateRecursion(gepInst, guard, offset);
        }

        if (memoryUpdateLists.count(basePtr)) {
          kleeExpr valExpr = translateRecursion(val, guard, offset);
          unsigned arrayRange = memoryUpdateLists.at(basePtr)->root->getRange();

          if (arrayRange > 8) {
            // Wide array: single-element write
            memoryUpdateLists.at(basePtr)->extend(
                exprBuilder->Constant(0, klee::Expr::Int32), valExpr);
          } else {
            unsigned storeSize = dataLayout->getTypeStoreSize(val->getType());
            if (storeSize == 0) storeSize = (val->getType()->getPrimitiveSizeInBits() + 7) / 8;

            // Decompose into byte writes
            for (unsigned i = 0; i < storeSize; i++) {
              kleeExpr byteIndex = exprBuilder->Add(byteOffset,
                  exprBuilder->Constant(i, klee::Expr::Int32));
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

        Value *basePtr = ptr;
        kleeExpr byteOffset = exprBuilder->Constant(0, klee::Expr::Int32);

        if (auto *gepInst = dyn_cast<GetElementPtrInst>(ptr)) {
          basePtr = gepInst->getPointerOperand();
          byteOffset = translateRecursion(gepInst, guard, offset);
        }

        // Lazy initialization: if the alloca hasn't been processed yet,
        // create its array and translate all preceding stores / registerInput
        // calls to it. This builds the memory model on demand.
        if (!memoryUpdateLists.count(basePtr) &&
            (isa<AllocaInst>(basePtr) || isa<GlobalVariable>(basePtr))) {
          // First, check for a registerInput call targeting this alloca.
          // If found, use the user-specified name for the symbolic array.
          bool hasSpecialArray = false;
          for (BasicBlock &bb : *_F) {
            for (Instruction &bbInst : bb) {
              if (auto *ci = dyn_cast<CallInst>(&bbInst)) {
                Function *cf = ci->getCalledFunction();
                if (cf && cf->getName().find("registerInput") != StringRef::npos) {
                  Value *riPtr = ci->getArgOperand(1);
                  Value *riBase = riPtr;
                  if (auto *bc = dyn_cast<BitCastInst>(riPtr))
                    riBase = bc->getOperand(0);
                  if (riBase == basePtr) {
                    translateRecursion(ci, guard, offset);
                    hasSpecialArray = true;
                  }
                }
              }
            }
          }
          // If this alloca is an output (from registerOutput), create a wide
          // array like registerInput does, to avoid byte-level concatenation.
          if (!hasSpecialArray && outputNames.count(basePtr)) {
            auto *ai = cast<AllocaInst>(basePtr);
            std::string arrName = outputNames[basePtr];
            unsigned bitW = dataLayout->getTypeAllocSize(ai->getAllocatedType()) * 8;
            const klee::Array *array = arrayCache->CreateArray(
                arrName, 1, nullptr, nullptr, klee::Expr::Int32, bitW);
            memoryArrays[basePtr] = array;
            memoryUpdateLists.insert_or_assign(
                basePtr, std::make_unique<klee::UpdateList>(array, nullptr));
            hasSpecialArray = true;
          }
          // Otherwise create a default byte-level array from the alloca
          if (!hasSpecialArray)
            translateRecursion(basePtr, guard, offset);
          // Walk the function and translate all stores that write to this
          // alloca, in program order.
          for (BasicBlock &bb : *_F)
            for (Instruction &bbInst : bb) {
              if (auto *si = dyn_cast<StoreInst>(&bbInst)) {
                Value *siPtr = si->getPointerOperand();
                Value *siBase = siPtr;
                if (auto *gep = dyn_cast<GetElementPtrInst>(siPtr))
                  siBase = gep->getPointerOperand();
                if (siBase == basePtr)
                  translateInst(&bbInst);
              }
            }
        }

        if (memoryUpdateLists.count(basePtr)) {
          klee::UpdateList &updates = *memoryUpdateLists.at(basePtr);
          unsigned loadBitWidth = loadInst->getType()->getPrimitiveSizeInBits();
          if (loadBitWidth == 0) loadBitWidth = 32;

          // If the array has a wide range (e.g. Int32 from registerInput),
          // do a single read. Otherwise, do byte-by-byte reads.
          unsigned arrayRange = updates.root->getRange();
          if (arrayRange > 8) {
            ret = exprBuilder->Read(updates, exprBuilder->Constant(0, klee::Expr::Int32));
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
              kleeExpr byteIndex = exprBuilder->Add(byteOffset,
                  exprBuilder->Constant(i, klee::Expr::Int32));
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
            auto *structType = cast<StructType>(indexedType);
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
        auto *phiInst = dyn_cast<PHINode>(inst);
        unsigned width = phiInst->getType()->getPrimitiveSizeInBits();
        if (width == 0) width = 32;

        // Accumulate: result = sum over incoming edges of Select(edgeGuard, val, 0)
        kleeExpr result = exprBuilder->Constant(0, width);
        BasicBlock *currentBB = phiInst->getParent();

        for (unsigned i = 0; i < phiInst->getNumIncomingValues(); i++) {
          BasicBlock *incomingBB = phiInst->getIncomingBlock(i);
          Value *incomingVal = phiInst->getIncomingValue(i);

          bdd edgeCond = bddBR->getEdgeCondition(incomingBB, currentBB);
          kleeExpr condExpr = convertBddToKleeExpr(edgeCond);

          kleeExpr incomingExpr = translateRecursion(incomingVal, guard, offset);

          // Ensure matching widths for Select
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
  return ret;
}


kleeExpr TranslateToStpPass::convertBddToKleeExpr(bdd node) {
  if (node == bddtrue)
    return exprBuilder->True();
  if (node == bddfalse)
    return exprBuilder->False();

  int var = bdd_var(node);
  if (bddToKleeCache.count(var))
    return bddToKleeCache[var];

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
  bddToKleeCache[var] = result;
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
    // No matching write: symbolic read, print the array name
    os << re->updates.root->name;
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

  // Concat
  if (kind == Expr::Concat) {
    auto *cc = static_cast<const klee::ConcatExpr *>(e.get());
    os << "(concat ";
    printSMTExpr(cc->getLeft(), os, varWidths); os << " ";
    printSMTExpr(cc->getRight(), os, varWidths); os << ")";
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

void TranslateToStpPass::translateOutputToStp() {
  errs() << "Translating output expressions to SMT-LIB2 format...\n";

  // Build a map of variable name → bit-width for SMT-LIB2 declarations.
  // Inputs come from registerInput, outputs from registerOutput.
  std::unordered_map<std::string, unsigned> varWidths;
  for (auto &kv : inputNames) {
    // Get bit-width from the alloca's allocated type
    if (auto *ai = dyn_cast<AllocaInst>(kv.first))
      varWidths[kv.second] = dataLayout->getTypeAllocSize(ai->getAllocatedType()) * 8;
  }
  for (auto &kv : outputNames) {
    if (output.count(kv.first) && output[kv.first])
      varWidths[kv.second] = output[kv.first]->getType()->getPrimitiveSizeInBits();
  }

  // Write SMT-LIB2 output
  std::error_code EC;
  llvm::raw_fd_ostream ofs("outputStpExpr.txt", EC);
  if (EC) {
    errs() << "Cannot open outputStpExpr.txt: " << EC.message() << "\n";
    return;
  }

  // Header
  ofs << "(set-logic QF_BV)\n";
  ofs << "(set-info :source |generated by translateToStp pass|)\n\n";

  // Declare variables
  for (auto &vw : varWidths) {
    ofs << "(declare-fun " << vw.first << " () (_ BitVec " << vw.second << "))\n";
  }
  ofs << "\n";

  // Assert each output
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

    // Sanitize name
    for (char &c : varName)
      if (!isalnum(c) && c != '_') c = '_';

    ofs << "(assert (= " << varName << " ";
    printSMTExpr(e, ofs, varWidths);
    ofs << "))\n";

    errs() << "SMT Variable: " << varName << "\n";
  }

  ofs << "\n(check-sat)\n";
  ofs.close();
}