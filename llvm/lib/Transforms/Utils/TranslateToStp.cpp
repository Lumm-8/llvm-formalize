//===-- translateToStp.cpp - Example Transformations --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//#include "TranslateToStp.h"
#include "llvm/Transforms/Utils/TranslateToStp.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/ADT/PostOrderIterator.h"

// Include std headers
#include <map>
#include <vector>

// Include KLEE headers
#include "klee/Expr/Expr.h"
#include "klee/Expr/ExprBuilder.h"
#include <llvm/Support/Debug.h>
#include <iostream>


using namespace llvm;

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
  bdd_done();
}

TranslateToStpPass::TranslateToStpPass() {
  bddBR = new BddBranchRecord();
  exprBuilder = std::make_unique<klee::ExprBuilder>(klee::createDefaultExprBuilder());
  vc = vc_createValidityChecker();
  stpBuilder = new klee::StpBuilder(vc);
}

TranslateToStpPass::~TranslateToStpPass() {
  delete bddBR;
  vc_Destroy(vc);
  delete stpBuilder;
}

// Move the helper functions from anonymous namespace to class methods
PreservedAnalyses TranslateToStpPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  errs() << "Processing function: " << F.getName() << "\n";
  std::cout << "TranslateToStp pass is running \n";

  _F = &F;
  dataLayout = &(_F->getDataLayout());

  bddBR->collectBranchInfo(_F);

  getOutputPort();


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
      errs() << "function is empty \n";
      // user registeration function.
      // Need to find the registration function by keyword
      for (BasicBlock &bb: *_F) {
        for (Instruction &inst: bb) {
          if (auto *ci = dyn_cast<CallInst>(&inst)) {
            Function *calledFunc = ci->getCalledFunction();
            StringRef fName = calledFunc->getName();

            // void registerOutput(const char *name, void *ptr, int bitWith);
            // registerOutput("tmp_a", a, sizeof(a));
            errs() << "function name is " << fName << "\n";
            ci->dump();
            errs() << "\n";
            calledFunc->dump();
            errs() << "\n";
            
            if (fName.find("registerOutput") != StringRef::npos) {
              Value *outputName = ci->getArgOperand(0);
              Value *ptr = ci->getArgOperand(1);
              BitCastInst *bitCast = dyn_cast<BitCastInst>(ptr);
              Value *origin = bitCast->getOperand(0);

              // TODO: Need to change to find the store instruction related to it.
              // Because the current processing method may cause the origin to not be the allocaInst instruction.
              AllocaInst *allocaInst = dyn_cast<AllocaInst>(origin);
              Type *type = allocaInst->getAllocatedType();

              StringRef name = getStringFromValue(outputName);
              errs() << "the output port name is " << name << " \n";

              errs() << "origin value is \n";
              origin->dump();
              errs() << "\n";
              
              type->dump();
              errs() << "\n";

              outputName->dump();
              outputName->getType()->dump();
              errs() << "\n";

              LoadInst *load = new LoadInst(type, origin, Twine("loadOutput"), InsertPosition(&inst));
              output[origin] = load;
            }
            else if (fName.find("registerInput") != StringRef::npos) {
              // processing input potr.
              /**
               * void registerInput(char *name, void *ptr, int len);
               * registerInput("a", &a, sizeof(a));
               * Because the ptr variable passed into the registerInput function needs to be turned into an input port, 
               * turning it into an input port means that the logic using this function no longer exists. 
               * At this time, we can create a new variable. If the user name is inconsistent with the original name,
               * use the user-provided name. If they are the same, add the prefix new. 
               * Then construct a store instruction and assign the newly created variable to the ptr variable.
               * 
               * @Time 2025.08.07 update:
               *  It may not be necessary to be so troublesome. you only need to terminate the translation 
               *  when you encounter the CallInst instruction during the instruction translation process, 
               *  so no processing is done here.
               */
            }
          }
        }
      }
    }
    else {
      errs() << "the function is not empty \n";
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
            return constDataArray->getAsString();
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
  kleeExpr guard = exprBuilder->Constant(1, klee::Expr::Bool);
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
    // TODO:
    errs() << "need to process CallInst: \n";
    callInst->dump();
    errs() << "\n";
  }
  else if (isa<Argument>(v)) {
    // TODO:
    errs() << "need to process Argument inst: \n";
    v->dump();
    errs() << "\n";
  }
  else if (isa<GlobalVariable>(v)) {
    // TODO:
    errs() << "need to process GlobalVariable inst: \n";
    v->dump();
    errs() << "\n";
  }
  else if (auto *inst = dyn_cast<Instruction>(v)) {
    // process instruction
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
            // klee::SubExpr::create(left, right);
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
        switch (icmpInst->getPredicate()) {
          case ICmpInst::ICMP_EQ:
            ret = exprBuilder->Eq(left, right);
            break;
          case ICmpInst::ICMP_NE:
            ret = exprBuilder->Ne(left, right);
            break;
          case ICmpInst::ICMP_SLT:
            ret = exprBuilder->Slt(left, right);
            break;
          case ICmpInst::ICMP_SLE:
            ret = exprBuilder->Sle(left, right);
            break;
          case ICmpInst::ICMP_SGT:
            ret = exprBuilder->Sgt(left, right);
            break;
          case ICmpInst::ICMP_SGE:
            ret = exprBuilder->Sge(left, right);
            break;
          case ICmpInst::ICMP_ULT:
            ret = exprBuilder->Ult(left, right);
            break;
          case ICmpInst::ICMP_ULE:
            ret = exprBuilder->Ule(left, right);
            break;
          case ICmpInst::ICMP_UGT:
            ret = exprBuilder->Ugt(left, right);
            break;
          case ICmpInst::ICMP_UGE:
            ret = exprBuilder->Uge(left, right);
            break;
          default:
            assert(false && "Unsupported ICmp predicate");
        }
        break;
      }
      // case Instruction::Not: {
      //   kleeExpr operand = translateRecursion(inst->getOperand(0), guard, offset);
      //   ret = exprBuilder->Not(operand);
      //   break;
      // }
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
            // FIXME: is this correct about the trunc operation ?
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
    case Instruction::Alloca: {
      auto allocaInst = dyn_cast<AllocaInst>(inst);
      break; 
    }
    case Instruction::Store: {
      // Store instruction, we can ignore it for now
      // because we are not interested in the store value.
      // But we can process the pointer operand if needed.  -- ai
      auto *storeInst = dyn_cast<StoreInst>(inst);
      Value *ptr = storeInst->getPointerOperand();
      ret = translateRecursion(ptr, guard, offset);
      break;
    }
    case Instruction::Load: {

    }
  }
}


void TranslateToStpPass::translateOutputToStp() {
  FILE fd = fopen("outputStpExpr.txt", "w");

  if (!fd) {
    errs() << "file doesnot open\n";
    return ;
  }

  for (auto &it: outputKleeExpr) {
    Value *v = it.first;
    kleeExpr e = it.second;

    // Get the bit-width of the value
    unsigned bitWidth = 0;
    if (auto *inst = dyn_cast<Instruction>(v)) {
      bitWidth = inst->getType()->getPrimitiveSizeInBits();
    } else if (auto *arg = dyn_cast<Argument>(v)) {
      bitWidth = arg->getType()->getPrimitiveSizeInBits();
    } else if (auto *globalVar = dyn_cast<GlobalVariable>(v)) {
      bitWidth = globalVar->getType()->getPrimitiveSizeInBits();
    } else {
      errs() << "Unsupported Value type for STP translation\n";
      continue;
    }

    // Convert Klee expression to STP expression
    ExprHandle stpExpr = stpBuilder->construct(e);

    // Declare a new STP variable for the output
    std::string varName = v->getName().str();
    ExprHandle stpVar = vc_varExpr(vc, varName.c_str(), vc_bvType(vc, bitWidth));

    // Assert equality between the STP variable and the translated expression
    ExprHandle equality = vc_eqExpr(vc, stpVar, stpExpr);
    vc_assertFormula(vc, equality);

    vc_printExprFile(vc, fd, equality);

    // For debugging: print the STP variable and its corresponding expression
    errs() << "STP Variable: " << varName << "\n";
    errs() << "Corresponding STP Expression: ";
    vc_printExpr(vc, stpExpr);
    errs() << "\n";
  }

  fd->close();
}