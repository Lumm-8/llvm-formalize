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
#include "klee/Expr/Expr.h"
#include "klee/Solver/StpBuilder.h"
// FIXME: need to change the include path to the correct one
#include <stp/c_interface.h>

#include <map>
#include <vector>

namespace llvm {
    typedef klee::ref<klee::Expr> kleeExpr;
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
      void translateOutputToStp();
      Instruction* findStoreInstFromBasicBlock(BasicBlock &bb, Value *v);
      StringRef getStringFromValue(Value *v);
      void getOutputKleeExpr();
      kleeExpr translateInst(Value *v);
      kleeExpr translateRecursion(Value *v, kleeExpr guard, kleeExpr offset);
    private:
      Function *_F;
      const DataLayout *dataLayout;
      // key is PO, value is the logic of PO
      std::map<Value*, Value*> output;
      std::unordered_map<Value*, kleeExpr> outputKleeExpr;

      std::unique_ptr<klee::ExprBuilder> exprBuilder;
      // Cache for translated Klee expressions  

      BddBranchRecord *bddBR;
      std::unordered_map<Value*, kleeExpr> valueToKleeExprCache;

      VC vc;
      klee::StpBuilder *stpBuilder;
      
    };

} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_TRANSLATETOSTP_H
