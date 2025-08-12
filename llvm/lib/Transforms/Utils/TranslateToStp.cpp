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
}

TranslateToStpPass::~TranslateToStpPass() {
  delete bddBR;
}

// Move the helper functions from anonymous namespace to class methods
PreservedAnalyses TranslateToStpPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  errs() << "Processing function: " << F.getName() << "\n";
  std::cout << "TranslateToStp pass is running \n";

  _F = &F;
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
    
  }

}

/**
 * @note 
 */
void TranslateToStpPass::translateInst(Value *v) {
  
}