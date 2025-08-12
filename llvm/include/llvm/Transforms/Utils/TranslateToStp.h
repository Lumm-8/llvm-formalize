//===-- TranslateToStp.h - Example Transformations ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_TRANSLATETOSTP_H
#define LLVM_TRANSFORMS_UTILS_TRANSLATETOSTP_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "klee/Expr/Expr.h"
#include "klee/Expr/ExprBuilder.h"

#include "bdd.h"

#include <map>
#include <vector>

namespace llvm {
    /**
     * Use bdd to record the path conditions of basic blocks.
     */
    class BddBranchRecord {
    public:
      BddBranchRecord();
      ~BddBranchRecord();
      void collectBranchInfo(Function *F);
      bdd getEdgeCondition(BasicBlock *parent, BasicBlock *child);

    private:
      int bddIndex;
      int bddIndexMax;
      std::unordered_map<int, Value*> bddValue;
      std::unordered_map<Value*, int> valueId;
      std::unordered_map<BasicBlock*, bdd> basicBlockBdd;
    };

    class TranslateToStpPass : public PassInfoMixin<TranslateToStpPass> {
    public:
    	PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
      TranslateToStpPass();
      ~TranslateToStpPass();
      
      void getOutputPort();
      Instruction* findStoreInstFromBasicBlock(BasicBlock &bb, Value *v);
      StringRef getStringFromValue(Value *v);
      void getOutputKleeExpr();
      void translateInst(Value *v);

    private:
      Function *_F;
      // key is PO, value is the logic of PO
      std::map<Value*, Value*> output;
      BddBranchRecord *bddBR;

    };

} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_TRANSLATETOSTP_H
