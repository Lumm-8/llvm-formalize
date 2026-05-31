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
#include "klee/Expr/ArrayCache.h"
#include "klee/Solver/STPBuilder.h"

#include <map>
#include <vector>
#include <unordered_map>

namespace llvm {
    void printValue(Value *v, StringRef s);
    typedef klee::ref<klee::Expr> kleeExpr;
    /**
     * Use bdd to record the path conditions of basic blocks.
     */
    class BddBranchRecord {
      friend class TranslateToStpPass;
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
      TranslateToStpPass(TranslateToStpPass&& other) noexcept;
      TranslateToStpPass& operator=(TranslateToStpPass&& other) noexcept;

      void getOutputPort();
      void translateOutputToStp();
      Instruction* findStoreInstFromBasicBlock(BasicBlock &bb, Value *v);
      StringRef getStringFromValue(Value *v);
      void getOutputKleeExpr();
      kleeExpr translateInst(Value *v);
      kleeExpr translateRecursion(Value *v, kleeExpr guard, kleeExpr offset);
      kleeExpr convertBddToKleeExpr(bdd node);
      kleeExpr getGuardForValue(Value *v);
      void printSMTExpr(kleeExpr e, raw_ostream &os,
                        const std::unordered_map<std::string, unsigned> &varWidths);
    private:
      Function *_F;
      const DataLayout *dataLayout;
      // key is PO, value is the logic of PO
      std::map<Value*, Value*> output;
      std::unordered_map<Value*, kleeExpr> outputKleeExpr;

      std::unique_ptr<klee::ExprBuilder> exprBuilder;
      // Cache for translated Klee expressions

      std::unique_ptr<BddBranchRecord> bddBR;
      std::unordered_map<Value*, kleeExpr> valueToKleeExprCache;

      VC vc;
      klee::STPBuilder *stpBuilder;

      // Memory model
      std::unique_ptr<klee::ArrayCache> arrayCache;
      std::unordered_map<Value*, const klee::Array*> memoryArrays;
      std::unordered_map<Value*, std::unique_ptr<klee::UpdateList>> memoryUpdateLists;

      // Value to basic block mapping for BDD guard lookup
      std::unordered_map<Value*, BasicBlock*> valueToBlock;

      // Output names from registerOutput(name, ptr, size)
      std::unordered_map<Value*, std::string> outputNames;
      // Input names from registerInput(name, ptr, size): maps alloca → name
      std::unordered_map<Value*, std::string> inputNames;

      // Symbolic variables for arguments and globals
      std::unordered_map<Argument*, kleeExpr> argumentExprs;
      std::unordered_map<GlobalVariable*, kleeExpr> globalVarExprs;

      // BDD-to-KLEE memoization cache
      std::unordered_map<int, kleeExpr> bddToKleeCache;

      // Symbolic variable index for unnamed values
      unsigned symbolicVarIndex = 0;
    };

} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_TRANSLATETOSTP_H
