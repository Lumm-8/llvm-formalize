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
#include <set>
#include <string>
#include <vector>
#include <unordered_map>

namespace llvm {
    class CallInst;
    class StoreInst;
    class AllocaInst;
    class GEPOperator;

    typedef klee::ref<klee::Expr> kleeExpr;
    /**
     * Use bdd to record the path conditions of basic blocks.
     */
    class BddBranchRecord {
      friend class TranslateToStpPass;
    public:
      BddBranchRecord();
      ~BddBranchRecord();
      void reset();
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

      void resetFunctionState();
      void collectFunctionMemoryIndex();
      void getOutputPort();
      void translateOutputToStp(const std::string &outFileName);
      Instruction* findStoreInstFromBasicBlock(BasicBlock &bb, Value *v);
      StringRef getStringFromValue(Value *v);
      void getOutputKleeExpr();
      void buildMemorySideEffects();
      kleeExpr translateInst(Value *v);
      kleeExpr translateRecursion(Value *v, kleeExpr guard, kleeExpr offset);
      kleeExpr guardedValue(kleeExpr guard, kleeExpr newValue, kleeExpr oldValue);
      bool decomposePointer(Value *ptr, Value *&basePtr, kleeExpr &byteOffset,
                            kleeExpr guard, kleeExpr offset);
      kleeExpr convertBddToKleeExpr(bdd node);
      kleeExpr getGuardForValue(Value *v);
      klee::ExprHandle convertKleeToStpExpr(kleeExpr e);
      void printSMTExpr(kleeExpr e, raw_ostream &os,
                        const std::unordered_map<std::string, unsigned> &varWidths);
      std::string sanitizeSymbolName(std::string name) const;
      std::string registerSymbolName(Value *v, std::string proposedName,
                                     bool exactName = false);
      std::string getSymbolName(Value *v, StringRef fallbackPrefix);
      std::string getPointerSourceName(Value *ptr, StringRef fallbackPrefix);
      std::string getGEPSourceName(const GEPOperator *gep);
      bool getConstantPointerOffset(Value *ptr, Value *&basePtr,
                                    int64_t &byteOffset) const;
      void recordRegisteredPointerOffset(Value *ptr);
      kleeExpr normalizeRegisteredOffset(Value *memKey, kleeExpr byteOffset);
      Value *selectRegisteredMemoryKey(Value *ptr, Value *basePtr,
                                       kleeExpr &byteOffset);
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

      // One-pass function index used by output translation.  The old
      // translation path repeatedly walked the whole function when resolving
      // loads/stores.  These tables let the recursive translator start from
      // output assignments and look up only the relevant memory operations.
      std::vector<CallInst*> inputRegisterCalls;
      std::vector<CallInst*> memoryIntrinsicCalls;
      std::vector<AllocaInst*> allocaInsts;
      std::unordered_map<Value*, std::vector<StoreInst*>> storesByBase;
      std::unordered_map<Value*, unsigned> loadCountByBase;
      std::set<Value*> storedBases;

      // Value to basic block mapping for BDD guard lookup
      std::unordered_map<Value*, BasicBlock*> valueToBlock;

      // Output names from registerOutput(name, ptr, size)
      std::unordered_map<Value*, std::string> outputNames;
      // Input names from registerInput(name, ptr, size): maps alloca → name
      std::unordered_map<Value*, std::string> inputNames;
      // Input sizes in bytes from registerInput's third argument
      std::unordered_map<Value*, unsigned> inputSizes;
      // Output sizes in bytes from registerOutput's third argument
      std::unordered_map<Value*, unsigned> outputSizes;

      // Symbolic variables for arguments and globals
      std::unordered_map<Argument*, kleeExpr> argumentExprs;
      std::unordered_map<GlobalVariable*, kleeExpr> globalVarExprs;

      // BDD-to-KLEE memoization cache
      std::unordered_map<int, kleeExpr> bddToKleeCache;

      // Named wide arrays for local int allocas (two-phase ITE optimization).
      // Maps alloca → pre-created named array.  Loads from these allocas
      // return Read(array, 0) references instead of inlined ITE chains,
      // preventing O(2^n) formula explosion.
      std::unordered_map<Value*, const klee::Array*> namedLocalArrays;

      // ITE expressions built for named local arrays (populated after
      // output ITE processing, emitted as separate SMT2 assertions).
      std::unordered_map<Value*, kleeExpr> namedLocalITEs;

      // Symbolic variable index for unnamed values
      unsigned symbolicVarIndex = 0;

      // Unified SMT symbol naming.  Exact names come from
      // registerInput/registerOutput and are never uniquified; inferred names
      // use LLVM/source provenance where available and are made unique here
      // instead of relying on STP/KLEE suffixing.
      std::unordered_map<Value*, std::string> symbolNames;
      std::unordered_map<std::string, unsigned> symbolNameUseCount;
      std::unordered_map<Value*, int64_t> registeredBaseOffsets;

      // Pointer analysis: maps loaded pointer SSA values to the alloca
      // they point to.  Filled by stores to pointer allocas and
      // propagated through pointer loads.  Used to resolve *p loads.
      std::unordered_map<Value*, Value*> pointerTargets;

      // Auto-declared array names encountered during SMT2 printing.
      // These are auto-generated alloca names that appear in the formula
      // but don't have explicit declare-fun entries.  Collected during
      // printSMTExpr and written in translateOutputToStp.
      mutable std::set<std::string> undeclaredSmtArrays;

      bool explicitMemoryMode = false;
      bool memorySideEffectsBuilt = false;
    };

} // namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_TRANSLATETOSTP_H
