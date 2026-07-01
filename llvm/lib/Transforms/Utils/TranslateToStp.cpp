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
#include <algorithm>
#include <cctype>
#include <map>
#include <sstream>
#include <vector>

// Include KLEE headers
#include "klee/Expr/Expr.h"
#include "klee/Expr/ExprBuilder.h"
// FIXME: need to change the include path to the correct one
// #include <stp/c_interface.h>

#include "klee/Solver/STPBuilder.h"
#include <stp/c_interface.h>
#include <stp/AST/ASTNode.h>
using namespace llvm;

namespace printer {
void SMTLIB2_PrintBack(std::ostream &os, const stp::ASTNode &n, stp::STPMgr *stp,
                       bool definately_bv = false);
}

// Set of alloca base pointers whose ITE chains are currently being built.
// Loads from these allocas read the root array directly to avoid recursive
// ITE chain explosion (e.g.  a = ashr a, 1 where store value reads same
// alloca that is having its ITE chain constructed).
static thread_local SmallPtrSet<Value *, 4> BuildingITEChains;
static bool BddPackageInitialized = false;

static Value *stripPointerCasts(Value *v) {
  while (true) {
    if (auto *bc = dyn_cast<BitCastInst>(v)) {
      v = bc->getOperand(0);
      continue;
    }
    if (auto *asc = dyn_cast<AddrSpaceCastInst>(v)) {
      v = asc->getOperand(0);
      continue;
    }
    if (auto *ce = dyn_cast<ConstantExpr>(v)) {
      if (ce->getOpcode() == Instruction::BitCast ||
          ce->getOpcode() == Instruction::AddrSpaceCast) {
        v = ce->getOperand(0);
        continue;
      }
    }
    return v;
  }
}

BddBranchRecord::BddBranchRecord() {
  if (!BddPackageInitialized) {
    bdd_init(100000, 10000);
    bdd_setvarnum(10000);
    BddPackageInitialized = true;
  }
  reset();
}

void BddBranchRecord::reset() {
  bddIndex = 0;
  bddIndexMax = 10000;
  bddValue.clear();
  valueId.clear();
  basicBlockBdd.clear();
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
  stpBuilder = new klee::STPBuilder(vc, false);
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
    inputRegisterCalls(std::move(other.inputRegisterCalls)),
    memoryIntrinsicCalls(std::move(other.memoryIntrinsicCalls)),
    allocaInsts(std::move(other.allocaInsts)),
    storesByBase(std::move(other.storesByBase)),
    loadCountByBase(std::move(other.loadCountByBase)),
    storedBases(std::move(other.storedBases)),
    valueToBlock(std::move(other.valueToBlock)),
    outputNames(std::move(other.outputNames)),
    inputNames(std::move(other.inputNames)),
    inputSizes(std::move(other.inputSizes)),
    outputSizes(std::move(other.outputSizes)),
    argumentExprs(std::move(other.argumentExprs)),
    globalVarExprs(std::move(other.globalVarExprs)),
    bddToKleeCache(std::move(other.bddToKleeCache)),
    namedLocalArrays(std::move(other.namedLocalArrays)),
    namedLocalITEs(std::move(other.namedLocalITEs)),
    symbolicVarIndex(other.symbolicVarIndex),
    pointerTargets(std::move(other.pointerTargets)),
    undeclaredSmtArrays(std::move(other.undeclaredSmtArrays)),
    explicitMemoryMode(other.explicitMemoryMode),
    memorySideEffectsBuilt(other.memorySideEffectsBuilt) {
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
    inputRegisterCalls = std::move(other.inputRegisterCalls);
    memoryIntrinsicCalls = std::move(other.memoryIntrinsicCalls);
    allocaInsts = std::move(other.allocaInsts);
    storesByBase = std::move(other.storesByBase);
    loadCountByBase = std::move(other.loadCountByBase);
    storedBases = std::move(other.storedBases);
    valueToBlock = std::move(other.valueToBlock);
    outputNames = std::move(other.outputNames);
    inputNames = std::move(other.inputNames);
    inputSizes = std::move(other.inputSizes);
    outputSizes = std::move(other.outputSizes);
    argumentExprs = std::move(other.argumentExprs);
    globalVarExprs = std::move(other.globalVarExprs);
    bddToKleeCache = std::move(other.bddToKleeCache);
    namedLocalArrays = std::move(other.namedLocalArrays);
    namedLocalITEs = std::move(other.namedLocalITEs);
    symbolicVarIndex = other.symbolicVarIndex;
    pointerTargets = std::move(other.pointerTargets);
    undeclaredSmtArrays = std::move(other.undeclaredSmtArrays);
    explicitMemoryMode = other.explicitMemoryMode;
    memorySideEffectsBuilt = other.memorySideEffectsBuilt;

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

void TranslateToStpPass::resetFunctionState() {
  _F = nullptr;
  dataLayout = nullptr;
  output.clear();
  outputKleeExpr.clear();
  valueToKleeExprCache.clear();
  bddToKleeCache.clear();
  memoryArrays.clear();
  memoryUpdateLists.clear();
  inputRegisterCalls.clear();
  memoryIntrinsicCalls.clear();
  allocaInsts.clear();
  storesByBase.clear();
  loadCountByBase.clear();
  storedBases.clear();
  valueToBlock.clear();
  outputNames.clear();
  inputNames.clear();
  inputSizes.clear();
  outputSizes.clear();
  argumentExprs.clear();
  globalVarExprs.clear();
  namedLocalArrays.clear();
  namedLocalITEs.clear();
  pointerTargets.clear();
  undeclaredSmtArrays.clear();
  explicitMemoryMode = false;
  memorySideEffectsBuilt = false;
  symbolicVarIndex = 0;
  arrayCache = std::make_unique<klee::ArrayCache>();
  if (bddBR)
    bddBR->reset();
}

void TranslateToStpPass::collectFunctionMemoryIndex() {
  inputRegisterCalls.clear();
  memoryIntrinsicCalls.clear();
  allocaInsts.clear();
  storesByBase.clear();
  loadCountByBase.clear();
  storedBases.clear();

  auto canonicalBase = [](Value *ptr) -> Value * {
    ptr = stripPointerCasts(ptr);
    if (auto *gep = dyn_cast<GetElementPtrInst>(ptr))
      return stripPointerCasts(gep->getPointerOperand());
    if (auto *ce = dyn_cast<ConstantExpr>(ptr))
      if (ce->getOpcode() == Instruction::GetElementPtr)
        return stripPointerCasts(cast<GEPOperator>(ce)->getPointerOperand());
    return ptr;
  };

  for (BasicBlock &bb : *_F) {
    for (Instruction &inst : bb) {
      if (auto *ai = dyn_cast<AllocaInst>(&inst)) {
        allocaInsts.push_back(ai);
      } else if (auto *ci = dyn_cast<CallInst>(&inst)) {
        Function *cf = ci->getCalledFunction();
        if (!cf)
          continue;
        StringRef name = cf->getName();
        if (name.find("registerInput") != StringRef::npos) {
          inputRegisterCalls.push_back(ci);
        } else if (name.find("llvm.memcpy") != StringRef::npos ||
                   name.find("llvm.memmove") != StringRef::npos ||
                   name.find("llvm.memset") != StringRef::npos) {
          memoryIntrinsicCalls.push_back(ci);
        }
      } else if (auto *li = dyn_cast<LoadInst>(&inst)) {
        loadCountByBase[canonicalBase(li->getPointerOperand())]++;
      } else if (auto *si = dyn_cast<StoreInst>(&inst)) {
        Value *base = canonicalBase(si->getPointerOperand());
        storesByBase[base].push_back(si);
        storedBases.insert(base);
      }
    }
  }
}

// Move the helper functions from anonymous namespace to class methods
PreservedAnalyses TranslateToStpPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  errs() << "Processing function: " << F.getName() << "\n";

  resetFunctionState();
  _F = &F;
  dataLayout = &(_F->getDataLayout());

  // Build value-to-block mapping for BDD guard lookup
  for (BasicBlock &bb : F)
    for (Instruction &inst : bb)
      valueToBlock[&inst] = &bb;

  bddBR->collectBranchInfo(_F);

  getOutputPort();
  if (output.empty()) {
    errs() << "No output ports found for function: " << F.getName()
           << ", skipping translateToStp\n";
    return PreservedAnalyses::all();
  }
  collectFunctionMemoryIndex();
  getOutputKleeExpr();

  // Derive output filename from the input module name
  std::string outName = _F->getParent()->getModuleIdentifier();
  size_t slashPos = outName.find_last_of("/\\");
  if (slashPos != std::string::npos) outName = outName.substr(slashPos + 1);
  size_t dotPos = outName.find_last_of('.');
  if (dotPos != std::string::npos) outName = outName.substr(0, dotPos);
  unsigned definedFunctionCount = 0;
  for (const Function &MF : *_F->getParent())
    if (!MF.isDeclaration())
      ++definedFunctionCount;
  if (definedFunctionCount > 1) {
    std::string functionName = F.getName().str();
    if (functionName.empty())
      functionName = "anon";
    for (char &c : functionName)
      if (!isalnum(c) && c != '_') c = '_';
    outName += "_" + functionName;
  }
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
            if (!calledFunc)
              continue;
            StringRef fName = calledFunc->getName();

            // void registerOutput(const char *name, void *ptr, int bitWith);
            // registerOutput("tmp_a", a, sizeof(a));
            if (fName.find("registerOutput") != StringRef::npos) {
              Value *outputName = ci->getArgOperand(0);
              Value *ptr = ci->getArgOperand(1);
              Value *origin = ptr;
              if (auto *bitCast = dyn_cast<BitCastInst>(ptr))
                origin = bitCast->getOperand(0);
              origin = stripPointerCasts(origin);

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
              if (auto *sizeCI = dyn_cast<ConstantInt>(ci->getArgOperand(2)))
                outputSizes[origin] = (unsigned)sizeCI->getZExtValue();

              Value *outputValue = nullptr;
              for (auto rit = inst.getIterator(); rit != bb.begin();) {
                --rit;
                if (auto *st = dyn_cast<StoreInst>(&*rit)) {
                  Value *storePtr = stripPointerCasts(st->getPointerOperand());
                  bool matches = storePtr == origin;
                  if (!matches) {
                    Value *storeBase = nullptr;
                    kleeExpr unusedOffset;
                    decomposePointer(storePtr, storeBase, unusedOffset,
                                     exprBuilder->True(),
                                     exprBuilder->Constant(0, klee::Expr::Int32));
                    matches = storeBase == origin;
                  }
                  if (matches) {
                    outputValue = st->getValueOperand();
                    break;
                  }
                }
              }

              if (outputValue) {
                output[origin] = outputValue;
              } else {
                LoadInst *load =
                    new LoadInst(type, origin, Twine("loadOutput"), inst.getIterator());
                output[origin] = load;
              }
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
            LoadInst *load =
                new LoadInst(type, arg, Twine("loadArg"), lastInst.getIterator());

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
  namedLocalArrays.clear();
  namedLocalITEs.clear();

  // Pre-scan: create named wide arrays for local integer allocas that
  // are read AND written (have both load and store instructions).
  // These get Read references instead of inlined ITE, preventing O(2^n).
  {
    unsigned varIdx = 0;
    for (AllocaInst *ai : allocaInsts) {
        if (inputNames.count(ai) || outputNames.count(ai)) continue;
        Type *allocTy = ai->getAllocatedType();
        if (!allocTy->isIntegerTy()) continue;
        unsigned bw = allocTy->getPrimitiveSizeInBits();
        if (bw > 64) continue;
        // Only name variables that are BOTH stored AND loaded from
        // multiple places (load count > 1).  Single-use temporaries
        // can be inlined safely.
        if (!storedBases.count(ai) || loadCountByBase[ai] <= 1) continue;
        // Skip if any GEP targeting this alloca is input/output
        bool hasGEPUse = false;
        for (auto &kv : outputNames)
          if (auto *g = dyn_cast<GetElementPtrInst>(kv.first))
            if (g->getPointerOperand() == ai) { hasGEPUse = true; break; }
        if (!hasGEPUse)
          for (auto &kv : inputNames)
            if (auto *g = dyn_cast<GetElementPtrInst>(kv.first))
              if (g->getPointerOperand() == ai) { hasGEPUse = true; break; }
        if (hasGEPUse) continue;

        std::string varName = "var_" + std::to_string(varIdx++);
        const klee::Array *array = arrayCache->CreateArray(
            varName, 1, nullptr, nullptr, klee::Expr::Int32, bw);
        memoryArrays[ai] = array;
        memoryUpdateLists.insert_or_assign(
            ai, std::make_unique<klee::UpdateList>(array, nullptr));
        namedLocalArrays[ai] = array;
    }
  }

  buildMemorySideEffects();
  memorySideEffectsBuilt = true;

  // Build ITE expressions for named local arrays.  These were skipped
  // during memory writes; emit them separately as definitions so output
  // expressions can refer to stable var_N symbols without inlining large
  // store chains.
  for (auto &kv : namedLocalArrays) {
    Value *basePtr = kv.first;
    unsigned bitWidth = 32;
    if (auto *ai = dyn_cast<AllocaInst>(basePtr))
      bitWidth = dataLayout->getTypeAllocSize(ai->getAllocatedType()) * 8;

    auto storesIt = storesByBase.find(basePtr);
    if (storesIt == storesByBase.end()) continue;

    kleeExpr result = exprBuilder->Constant(0, bitWidth);
    for (StoreInst *si : storesIt->second) {
      BasicBlock *bb = si->getParent();
      kleeExpr storeGuard = exprBuilder->True();
      if (bddBR->basicBlockBdd.count(bb)) {
        bdd blockBdd = bddBR->basicBlockBdd[bb];
        storeGuard = convertBddToKleeExpr(blockBdd);
      }
      kleeExpr valExpr = translateRecursion(
          si->getValueOperand(), exprBuilder->True(),
          exprBuilder->Constant(0, klee::Expr::Int32));
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
    namedLocalITEs[basePtr] = result;
  }

  // Output translation is the only expression translation pass.  Caches from
  // memory side-effect construction are not reusable because loads must see the
  // fully materialized UpdateLists/namedLocalITEs.
  valueToKleeExprCache.clear();
  bddToKleeCache.clear();

  for (auto &it: output) {
    outputKleeExpr[it.first] = translateInst(it.second);
  }
}

void TranslateToStpPass::buildMemorySideEffects() {
  explicitMemoryMode = true;
  memorySideEffectsBuilt = false;

  // Create memory objects and input arrays before processing writes.  Store
  // values may load from those objects while we build guarded updates.
  for (BasicBlock &bb : *_F) {
    for (Instruction &inst : bb) {
      if (isa<AllocaInst>(&inst)) {
        translateInst(&inst);
      } else if (auto *ci = dyn_cast<CallInst>(&inst)) {
        Function *cf = ci->getCalledFunction();
        if (cf && cf->getName().find("registerInput") != StringRef::npos)
          translateInst(ci);
      }
    }
  }

  valueToKleeExprCache.clear();
  bddToKleeCache.clear();

  for (BasicBlock &bb : *_F) {
    for (Instruction &inst : bb) {
      if (isa<StoreInst>(&inst)) {
        translateInst(&inst);
      } else if (auto *ci = dyn_cast<CallInst>(&inst)) {
        Function *cf = ci->getCalledFunction();
        if (!cf)
          continue;
        StringRef name = cf->getName();
        if (name.find("llvm.memcpy") != StringRef::npos ||
            name.find("llvm.memmove") != StringRef::npos ||
            name.find("llvm.memset") != StringRef::npos)
          translateInst(ci);
      }
    }
  }

  explicitMemoryMode = false;
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

kleeExpr TranslateToStpPass::guardedValue(kleeExpr guard, kleeExpr newValue,
                                          kleeExpr oldValue) {
  if (!guard)
    return newValue;
  if (newValue->getWidth() != oldValue->getWidth()) {
    unsigned width = oldValue->getWidth();
    if (newValue->getWidth() < width)
      newValue = exprBuilder->ZExt(newValue, width);
    else
      newValue = exprBuilder->Extract(newValue, 0, width);
  }
  if (guard->getKind() == klee::Expr::Constant) {
    auto &apv = static_cast<const klee::ConstantExpr *>(
        guard.get())->getAPValue();
    if (apv == 1)
      return newValue;
    return oldValue;
  }
  return exprBuilder->Select(guard, newValue, oldValue);
}

bool TranslateToStpPass::decomposePointer(Value *ptr, Value *&basePtr,
                                          kleeExpr &byteOffset,
                                          kleeExpr guard, kleeExpr offset) {
  ptr = stripPointerCasts(ptr);
  basePtr = ptr;
  byteOffset = exprBuilder->Constant(0, klee::Expr::Int32);

  if (auto *gepInst = dyn_cast<GetElementPtrInst>(ptr)) {
    basePtr = stripPointerCasts(gepInst->getPointerOperand());
    kleeExpr cumOffset = exprBuilder->Constant(0, klee::Expr::Int32);
    bool allConstant = true;
    uint64_t constantOffset = 0;
    for (auto it = gep_type_begin(gepInst), et = gep_type_end(gepInst);
         it != et; ++it) {
      Value *indexVal = it.getOperand();
      if (it.isStruct()) {
        auto *structType = it.getStructType();
        if (auto *constIdx = dyn_cast<ConstantInt>(indexVal)) {
          unsigned structIdx = constIdx->getZExtValue();
          unsigned elemOffset =
              dataLayout->getStructLayout(structType)->getElementOffset(structIdx);
          constantOffset += elemOffset;
          cumOffset = exprBuilder->Add(
              cumOffset, exprBuilder->Constant(elemOffset, klee::Expr::Int32));
        } else {
          allConstant = false;
        }
      } else {
        TypeSize stride = it.getSequentialElementStride(*dataLayout);
        unsigned elemSize = stride.getFixedValue();
        if (auto *constIdx = dyn_cast<ConstantInt>(indexVal)) {
          constantOffset += constIdx->getSExtValue() * elemSize;
        } else {
          allConstant = false;
        }
        kleeExpr idxExpr = translateRecursion(indexVal, guard, offset);
        if (idxExpr->getWidth() < klee::Expr::Int32)
          idxExpr = exprBuilder->ZExt(idxExpr, klee::Expr::Int32);
        else if (idxExpr->getWidth() > klee::Expr::Int32)
          idxExpr = exprBuilder->Extract(idxExpr, 0, klee::Expr::Int32);
        if (elemSize > 1)
          idxExpr = exprBuilder->Mul(
              idxExpr, exprBuilder->Constant(elemSize, klee::Expr::Int32));
        cumOffset = exprBuilder->Add(cumOffset, idxExpr);
      }
    }
    byteOffset = allConstant
                     ? exprBuilder->Constant(constantOffset, klee::Expr::Int32)
                     : cumOffset;
    return true;
  }

  if (auto *ce = dyn_cast<ConstantExpr>(ptr)) {
    if (ce->getOpcode() == Instruction::GetElementPtr) {
      auto *gep = cast<GEPOperator>(ce);
      basePtr = stripPointerCasts(gep->getPointerOperand());
      SmallVector<Value *, 4> indices;
      for (auto &idx : gep->indices())
        indices.push_back(idx.get());
      uint64_t off =
          dataLayout->getIndexedOffsetInType(gep->getSourceElementType(),
                                             indices);
      byteOffset = exprBuilder->Constant(off, klee::Expr::Int32);
      return true;
    }
  }

  return true;
}

kleeExpr TranslateToStpPass::translateRecursion(Value *v, kleeExpr guard, kleeExpr offset) {
  static thread_local unsigned depth = 0;
  if (++depth > 1000) { --depth; return exprBuilder->Constant(0, 32); }
  bool sideEffectingValue = false;
  if (isa<StoreInst>(v)) {
    sideEffectingValue = true;
  } else if (auto *ci = dyn_cast<CallInst>(v)) {
    if (Function *cf = ci->getCalledFunction()) {
      StringRef name = cf->getName();
      sideEffectingValue =
          name.find("registerInput") != StringRef::npos ||
          name.find("llvm.memcpy") != StringRef::npos ||
          name.find("llvm.memmove") != StringRef::npos ||
          name.find("llvm.memset") != StringRef::npos;
    }
  }
  if (!sideEffectingValue && valueToKleeExprCache.count(v)) {
    --depth;
    return valueToKleeExprCache[v];
  }

  kleeExpr ret = nullptr;

  if (auto *constantInst = dyn_cast<ConstantInt>(v)) {
    unsigned bw = constantInst->getType()->getPrimitiveSizeInBits();
    // Use getZExtValue to avoid the APInt signed-constructor assertion
    // on negative values (e.g., i32 -1 as int64_t → uint64_t overflow).
    ret = exprBuilder->Constant(
        static_cast<uint64_t>(constantInst->getZExtValue()), bw);
  }
  else if (auto *constantFP = dyn_cast<ConstantFP>(v)) {
    const APInt &bits = constantFP->getValueAPF().bitcastToAPInt();
    ret = exprBuilder->Constant(bits.getZExtValue(), bits.getBitWidth());
  }
  else if (isa<ConstantPointerNull>(v)) {
    ret = exprBuilder->Constant(0, klee::Expr::Int32);
  }
  else if (isa<UndefValue>(v) || isa<PoisonValue>(v)) {
    unsigned width = 32;
    if (v->getType()->isIntegerTy())
      width = v->getType()->getPrimitiveSizeInBits();
    ret = exprBuilder->Constant(0, width);
  }
  else if (isa<ConstantAggregateZero>(v)) {
    unsigned width = 32;
    if (v->getType()->isSized())
      width = dataLayout->getTypeStoreSize(v->getType()) * 8;
    if (width == 0) width = 32;
    ret = exprBuilder->Constant(0, width);
  }
  else if (auto *ce = dyn_cast<ConstantExpr>(v)) {
    switch (ce->getOpcode()) {
    case Instruction::GetElementPtr: {
      Value *basePtr = nullptr;
      kleeExpr byteOffset;
      decomposePointer(ce, basePtr, byteOffset, guard, offset);
      kleeExpr baseAddr = translateRecursion(basePtr, guard, offset);
      ret = exprBuilder->Add(baseAddr, byteOffset);
      break;
    }
    case Instruction::BitCast:
    case Instruction::AddrSpaceCast:
    case Instruction::IntToPtr:
      ret = translateRecursion(ce->getOperand(0), guard, offset);
      break;
    case Instruction::PtrToInt: {
      ret = translateRecursion(ce->getOperand(0), guard, offset);
      unsigned toWidth = ce->getType()->getPrimitiveSizeInBits();
      if (toWidth == 0) toWidth = klee::Expr::Int32;
      if (ret->getWidth() < toWidth)
        ret = exprBuilder->ZExt(ret, toWidth);
      else if (ret->getWidth() > toWidth)
        ret = exprBuilder->Extract(ret, 0, toWidth);
      break;
    }
    case Instruction::Trunc:
    case Instruction::ZExt:
    case Instruction::SExt: {
      kleeExpr operand = translateRecursion(ce->getOperand(0), guard, offset);
      unsigned toWidth = ce->getType()->getPrimitiveSizeInBits();
      if (ce->getOpcode() == Instruction::Trunc)
        ret = exprBuilder->Extract(operand, 0, toWidth);
      else if (ce->getOpcode() == Instruction::ZExt)
        ret = exprBuilder->ZExt(operand, toWidth);
      else
        ret = exprBuilder->SExt(operand, toWidth);
      break;
    }
    default:
      errs() << "Warning: unsupported constant expression: " << *ce << "\n";
      ret = exprBuilder->Constant(0, klee::Expr::Int32);
      break;
    }
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

        Value *originPtr = stripPointerCasts(ptr);
        bool scalarInput = false;
        if (auto *ai = dyn_cast<AllocaInst>(originPtr)) {
          Type *allocTy = ai->getAllocatedType();
          scalarInput = allocTy->isIntegerTy() &&
                        dataLayout->getTypeStoreSize(allocTy) == size;
        } else if (auto *gep = dyn_cast<GEPOperator>(originPtr)) {
          Type *elemTy = gep->getResultElementType();
          scalarInput = elemTy->isIntegerTy() &&
                        dataLayout->getTypeStoreSize(elemTy) == size;
        }

        unsigned bitWidth = size * 8;
        const klee::Array *array = nullptr;
        if (scalarInput && bitWidth <= 64) {
          // Scalar input: keep the clean single-symbol representation.
          array = arrayCache->CreateArray(inputName, 1, nullptr, nullptr,
                                          klee::Expr::Int32, bitWidth);
        } else {
          // Aggregate/array input: model bytes so GEP field/index loads work.
          array = arrayCache->CreateArray(inputName, size ? size : 1,
                                          nullptr, nullptr, klee::Expr::Int32,
                                          klee::Expr::Int8);
        }
        memoryArrays[ptr] = array;
        memoryUpdateLists.insert_or_assign(ptr, std::make_unique<klee::UpdateList>(array, nullptr));

        // Track the underlying alloca/GEP to redirect later loads/stores
        if (originPtr != ptr) {
          Value *origin = originPtr;
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
      } else if (funcName.find("llvm.ctlz.") != StringRef::npos) {
        kleeExpr x = translateRecursion(callInst->getArgOperand(0), guard, offset);
        unsigned xWidth = x->getWidth();
        unsigned retWidth = callInst->getType()->getPrimitiveSizeInBits();
        if (retWidth == 0) retWidth = klee::Expr::Int32;

        ret = exprBuilder->Constant(xWidth, retWidth);
        for (unsigned i = 0; i < xWidth; ++i) {
          unsigned bit = xWidth - 1 - i;
          kleeExpr bitExpr = exprBuilder->Extract(x, bit, klee::Expr::Bool);
          kleeExpr bitIsOne =
              exprBuilder->Eq(bitExpr, exprBuilder->Constant(1, klee::Expr::Bool));
          ret = exprBuilder->Select(bitIsOne,
                                    exprBuilder->Constant(i, retWidth),
                                    ret);
        }
      } else if (funcName.find("llvm.memcpy") != StringRef::npos ||
                 funcName.find("llvm.memmove") != StringRef::npos) {
        auto *lenCI = dyn_cast<ConstantInt>(callInst->getArgOperand(2));
        if (!lenCI) {
          errs() << "Warning: symbolic memcpy length is not supported\n";
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
        } else {
          uint64_t len = lenCI->getZExtValue();
          Value *dstBase = nullptr;
          Value *srcBase = nullptr;
          kleeExpr dstOff, srcOff;
          decomposePointer(callInst->getArgOperand(0), dstBase, dstOff,
                           guard, offset);
          decomposePointer(callInst->getArgOperand(1), srcBase, srcOff,
                           guard, offset);
          translateRecursion(dstBase, guard, offset);
          translateRecursion(srcBase, guard, offset);

          if (!memoryUpdateLists.count(dstBase) ||
              !memoryUpdateLists.count(srcBase)) {
            errs() << "Warning: memcpy with unknown memory object\n";
          } else {
            auto &dstUpdates = *memoryUpdateLists.at(dstBase);
            auto &srcUpdates = *memoryUpdateLists.at(srcBase);
            SmallVector<kleeExpr, 16> bytes;
            for (uint64_t i = 0; i < len; ++i) {
              kleeExpr srcIndex = srcOff;
              if (srcOff->getKind() == klee::Expr::Constant) {
                uint64_t off = static_cast<const klee::ConstantExpr *>(
                    srcOff.get())->getAPValue().getZExtValue();
                srcIndex = exprBuilder->Constant(off + i, klee::Expr::Int32);
              } else {
                srcIndex = exprBuilder->Add(srcOff,
                    exprBuilder->Constant(i, klee::Expr::Int32));
              }

              kleeExpr byteVal;
              if (srcUpdates.root->getRange() > 8 &&
                  srcOff->getKind() == klee::Expr::Constant) {
                uint64_t off = static_cast<const klee::ConstantExpr *>(
                    srcOff.get())->getAPValue().getZExtValue();
                kleeExpr wide = exprBuilder->Read(
                    srcUpdates,
                    exprBuilder->Constant(off, klee::Expr::Int32));
                byteVal = exprBuilder->Extract(wide, i * 8,
                                               klee::Expr::Int8);
              } else {
                byteVal = exprBuilder->Read(srcUpdates, srcIndex);
              }
              bytes.push_back(byteVal);
            }

            if (dstUpdates.root->getRange() > 8 &&
                dstOff->getKind() == klee::Expr::Constant &&
                len * 8 == dstUpdates.root->getRange()) {
              kleeExpr wide = nullptr;
              for (kleeExpr byteVal : bytes)
                wide = wide ? exprBuilder->Concat(wide, byteVal) : byteVal;
              kleeExpr oldWide = exprBuilder->Read(dstUpdates, dstOff);
              dstUpdates.extend(dstOff, guardedValue(guard, wide, oldWide));
            } else {
              for (uint64_t i = 0; i < len; ++i) {
                kleeExpr dstIndex = dstOff;
                if (dstOff->getKind() == klee::Expr::Constant) {
                  uint64_t off = static_cast<const klee::ConstantExpr *>(
                      dstOff.get())->getAPValue().getZExtValue();
                  dstIndex = exprBuilder->Constant(off + i, klee::Expr::Int32);
                } else {
                  dstIndex = exprBuilder->Add(dstOff,
                      exprBuilder->Constant(i, klee::Expr::Int32));
                }
                kleeExpr oldByte = exprBuilder->Read(dstUpdates, dstIndex);
                dstUpdates.extend(dstIndex,
                                  guardedValue(guard, bytes[i], oldByte));
              }
            }
          }
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
        }
      } else if (funcName.find("llvm.memset") != StringRef::npos) {
        auto *lenCI = dyn_cast<ConstantInt>(callInst->getArgOperand(2));
        if (!lenCI) {
          errs() << "Warning: symbolic memset length is not supported\n";
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
        } else {
          uint64_t len = lenCI->getZExtValue();
          Value *dstBase = nullptr;
          kleeExpr dstOff;
          decomposePointer(callInst->getArgOperand(0), dstBase, dstOff,
                           guard, offset);
          translateRecursion(dstBase, guard, offset);
          kleeExpr byteVal = translateRecursion(callInst->getArgOperand(1),
                                                guard, offset);
          if (byteVal->getWidth() > klee::Expr::Int8)
            byteVal = exprBuilder->Extract(byteVal, 0, klee::Expr::Int8);
          else if (byteVal->getWidth() < klee::Expr::Int8)
            byteVal = exprBuilder->ZExt(byteVal, klee::Expr::Int8);

          if (!memoryUpdateLists.count(dstBase)) {
            errs() << "Warning: memset with unknown memory object\n";
          } else {
            auto &dstUpdates = *memoryUpdateLists.at(dstBase);
            if (dstUpdates.root->getRange() > 8 &&
                dstOff->getKind() == klee::Expr::Constant &&
                len * 8 == dstUpdates.root->getRange()) {
              kleeExpr wide = nullptr;
              for (uint64_t i = 0; i < len; ++i)
                wide = wide ? exprBuilder->Concat(wide, byteVal) : byteVal;
              kleeExpr oldWide = exprBuilder->Read(dstUpdates, dstOff);
              dstUpdates.extend(dstOff, guardedValue(guard, wide, oldWide));
            } else {
              for (uint64_t i = 0; i < len; ++i) {
                kleeExpr dstIndex = dstOff;
                if (dstOff->getKind() == klee::Expr::Constant) {
                  uint64_t off = static_cast<const klee::ConstantExpr *>(
                      dstOff.get())->getAPValue().getZExtValue();
                  dstIndex = exprBuilder->Constant(off + i, klee::Expr::Int32);
                } else {
                  dstIndex = exprBuilder->Add(dstOff,
                      exprBuilder->Constant(i, klee::Expr::Int32));
                }
                kleeExpr oldByte = exprBuilder->Read(dstUpdates, dstIndex);
                dstUpdates.extend(dstIndex,
                                  guardedValue(guard, byteVal, oldByte));
              }
            }
          }
          ret = exprBuilder->Constant(0, klee::Expr::Int32);
        }
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
        if (left->getWidth() != right->getWidth()) {
          unsigned width = std::max(left->getWidth(), right->getWidth());
          if (left->getWidth() < width)
            left = exprBuilder->ZExt(left, width);
          else if (left->getWidth() > width)
            left = exprBuilder->Extract(left, 0, width);
          if (right->getWidth() < width)
            right = exprBuilder->ZExt(right, width);
          else if (right->getWidth() > width)
            right = exprBuilder->Extract(right, 0, width);
        }
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
      case Instruction::BitCast:
      case Instruction::AddrSpaceCast:
      case Instruction::IntToPtr: {
        ret = translateRecursion(inst->getOperand(0), guard, offset);
        break;
      }
      case Instruction::PtrToInt: {
        ret = translateRecursion(inst->getOperand(0), guard, offset);
        unsigned toWidth = inst->getType()->getPrimitiveSizeInBits();
        if (toWidth == 0) toWidth = klee::Expr::Int32;
        if (ret->getWidth() < toWidth)
          ret = exprBuilder->ZExt(ret, toWidth);
        else if (ret->getWidth() > toWidth)
          ret = exprBuilder->Extract(ret, 0, toWidth);
        break;
      }
      case Instruction::Freeze: {
        ret = translateRecursion(inst->getOperand(0), guard, offset);
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
        Value *basePtr = nullptr;
        kleeExpr byteOffset;
        decomposePointer(ptr, basePtr, byteOffset, guard, offset);

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
            kleeExpr oldValue = exprBuilder->Read(
                *memoryUpdateLists.at(basePtr), byteOffset);
            memoryUpdateLists.at(basePtr)->extend(
                byteOffset, guardedValue(guard, valExpr, oldValue));
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
              kleeExpr oldByte = exprBuilder->Read(
                  *memoryUpdateLists.at(basePtr), byteIndex);
              memoryUpdateLists.at(basePtr)->extend(
                  byteIndex, guardedValue(guard, byteValue, oldByte));
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
        ptr = stripPointerCasts(ptr);
        if (!isa<AllocaInst>(ptr) && !isa<GetElementPtrInst>(ptr) &&
            !isa<GlobalVariable>(ptr) && !isa<ConstantExpr>(ptr)) {
          if (pointerTargets.count(ptr)) {
            ptr = pointerTargets[ptr];
            // Propagate the mapping through this load: the load result
            // also points to the same target.
            pointerTargets[inst] = ptr;
          }
        }

        // Named local array shortcut: if this load targets a simple int
        // alloca that is NOT a registerInput/output target and NOT accessed
        // via GEP, lazily create a named wide array and return a Read
        // reference.  The ITE expression is built after the first pass and
        // emitted as a separate assert, preventing O(2^n) inlining.
        if (!isa<GetElementPtrInst>(ptr) && isa<AllocaInst>(ptr) &&
            !inputNames.count(ptr) && !outputNames.count(ptr)) {
          auto *ai = cast<AllocaInst>(ptr);
          Type *allocTy = ai->getAllocatedType();
          if (allocTy->isIntegerTy() && allocTy->getPrimitiveSizeInBits() <= 64) {
            if (!namedLocalArrays.count(ptr)) {
              std::string varName = "var_" + std::to_string(namedLocalArrays.size());
              unsigned bw = allocTy->getPrimitiveSizeInBits();
              const klee::Array *array = arrayCache->CreateArray(
                  varName, 1, nullptr, nullptr, klee::Expr::Int32, bw);
              memoryArrays[ptr] = array;
              memoryUpdateLists.insert_or_assign(
                  ptr, std::make_unique<klee::UpdateList>(array, nullptr));
              namedLocalArrays[ptr] = array;
            }
            unsigned lw = loadInst->getType()->getPrimitiveSizeInBits();
            if (lw == 0) lw = 32;
            ret = exprBuilder->Read(
                *memoryUpdateLists.at(ptr),
                exprBuilder->Constant(0, klee::Expr::Int32));
            if (ret->getWidth() != lw) {
              if (ret->getWidth() < lw) ret = exprBuilder->ZExt(ret, lw);
              else ret = exprBuilder->Extract(ret, 0, lw);
            }
            valueToKleeExprCache[v] = ret;
            --depth;
            return ret;
          }
        }

        Value *basePtr = nullptr;
        kleeExpr byteOffset;
        decomposePointer(ptr, basePtr, byteOffset, guard, offset);

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
        if (!explicitMemoryMode && !memorySideEffectsBuilt &&
            (isa<AllocaInst>(basePtr) || isa<GlobalVariable>(basePtr))) {
          bool alreadyExists = memoryUpdateLists.count(basePtr) != 0;
          // First, check for a registerInput call targeting this alloca.
          // If found, use the user-specified name for the symbolic array.
          bool hasSpecialArray = alreadyExists;
          auto registerInputMatchesBase = [&](CallInst *ci) {
            Value *riBase = stripPointerCasts(ci->getArgOperand(1));
            if (riBase == basePtr)
              return true;
            if (auto *riGEP = dyn_cast<GetElementPtrInst>(riBase))
              return stripPointerCasts(riGEP->getPointerOperand()) == basePtr;
            if (auto *riCE = dyn_cast<ConstantExpr>(riBase))
              if (riCE->getOpcode() == Instruction::GetElementPtr)
                return stripPointerCasts(
                           cast<GEPOperator>(riCE)->getPointerOperand()) ==
                       basePtr;
            return false;
          };
          for (CallInst *ci : inputRegisterCalls) {
            if (registerInputMatchesBase(ci)) {
              translateRecursion(ci, guard, offset);
              hasSpecialArray = true;
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
          // Always build ITE chain for output allocas, even if they are
          // also registerInput targets (e.g., test 21: c is both input
          // and output, body stores must be captured).
          if (!outName.empty()) {
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
            auto outStoresIt = storesByBase.find(basePtr);
            if (outStoresIt != storesByBase.end()) {
              for (StoreInst *si : outStoresIt->second) {
                BasicBlock *bb = si->getParent();
                // Write current ITE so loads from this alloca see it
                memoryUpdateLists.at(basePtr)->extend(
                    exprBuilder->Constant(0, klee::Expr::Int32), result);
                // Get guard and translate value (loads from this alloca
                // will read the current ITE via the UpdateList)
                kleeExpr storeGuard = exprBuilder->True();
                if (bddBR->basicBlockBdd.count(bb)) {
                  bdd blockBdd = bddBR->basicBlockBdd[bb];
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
          // For NON-OUTPUT registerInput allocas where the input is the
          // alloca ITSELF (not a GEP field), skip the ITE chain to avoid
          // circular BDD dependencies (e.g. a = a >> 1).
          // Output allocas ALWAYS build ITE chains regardless — the
          // output value must reflect all body stores.
          bool isDirectRegisterInput = false;
          bool isOutput = outputNames.count(basePtr) != 0;
          if (!isOutput && hasSpecialArray) {
            for (CallInst *ci : inputRegisterCalls) {
              Value *riPtr = stripPointerCasts(ci->getArgOperand(1));
              if (riPtr == basePtr && isa<AllocaInst>(riPtr)) {
                isDirectRegisterInput = true;
                break;
              }
            }
          }
          if (!isOutput &&
              !BuildingITEChains.count(basePtr) &&
              !isDirectRegisterInput) {
            auto storesIt = storesByBase.find(basePtr);
            ArrayRef<StoreInst *> storesToAlloca =
                storesIt == storesByBase.end()
                    ? ArrayRef<StoreInst *>()
                    : ArrayRef<StoreInst *>(storesIt->second);
            // If there are stores from multiple *different* blocks, build
            // an ITE chain with BDD guards.  Single-block stores (e.g.
            // struct initializer stores in the entry block) use the old
            // flat behavior which is correct for non-path-dependent values.
            SmallPtrSet<BasicBlock *, 4> storeBlocks;
            for (StoreInst *si : storesToAlloca)
              storeBlocks.insert(si->getParent());
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
              for (StoreInst *si : storesToAlloca) {
                BasicBlock *bb = si->getParent();
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
              for (StoreInst *si : storesToAlloca)
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

          bdd incomingCond = bddBR->getEdgeCondition(incomingBB, currentBB);
          auto predPathIt = bddBR->basicBlockBdd.find(incomingBB);
          if (predPathIt != bddBR->basicBlockBdd.end())
            incomingCond = predPathIt->second & incomingCond;
          kleeExpr condExpr = convertBddToKleeExpr(incomingCond);

          kleeExpr incomingExpr = translateRecursion(incomingVal, guard, offset);

          if (incomingExpr->getWidth() != width) {
            if (incomingExpr->getWidth() < width)
              incomingExpr = exprBuilder->ZExt(incomingExpr, width);
            else
              incomingExpr = exprBuilder->Extract(incomingExpr, 0, width);
          }

          result = exprBuilder->Select(condExpr, incomingExpr, result);
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

  if (!sideEffectingValue)
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

klee::ExprHandle TranslateToStpPass::convertKleeToStpExpr(kleeExpr e) {
  if (!e)
    return klee::ExprHandle();

  std::unordered_map<const klee::Expr *, kleeExpr> normalizedCache;
  std::function<kleeExpr(kleeExpr)> normalize = [&](kleeExpr x) -> kleeExpr {
    if (!x)
      return x;

    auto cacheIt = normalizedCache.find(x.get());
    if (cacheIt != normalizedCache.end())
      return cacheIt->second;

    using namespace klee;
    kleeExpr ret = x;
    Expr::Kind kind = x->getKind();

    if (auto *re = dyn_cast<ReadExpr>(x.get())) {
      std::vector<const UpdateNode *> nodes;
      for (const UpdateNode *un = re->updates.head.get(); un; un = un->next.get())
        nodes.push_back(un);
      std::reverse(nodes.begin(), nodes.end());

      UpdateList updates(re->updates.root, nullptr);
      bool changed = false;
      for (const UpdateNode *un : nodes) {
        kleeExpr idx = normalize(un->index);
        kleeExpr val = normalize(un->value);
        changed |= idx.get() != un->index.get() || val.get() != un->value.get();
        updates.extend(idx, val);
      }

      kleeExpr idx = normalize(re->index);
      changed |= idx.get() != re->index.get();
      ret = changed ? ReadExpr::create(updates, idx) : x;
      normalizedCache[x.get()] = ret;
      return ret;
    }

    if (kind >= Expr::BinaryKindFirst && kind <= Expr::BinaryKindLast) {
      auto *be = static_cast<const BinaryExpr *>(x.get());
      kleeExpr left = normalize(be->left);
      kleeExpr right = normalize(be->right);

      switch (kind) {
      case Expr::Ne:
        ret = exprBuilder->Not(exprBuilder->Eq(left, right));
        break;
      case Expr::Ugt:
        ret = exprBuilder->Ult(right, left);
        break;
      case Expr::Uge:
        ret = exprBuilder->Ule(right, left);
        break;
      case Expr::Sgt:
        ret = exprBuilder->Slt(right, left);
        break;
      case Expr::Sge:
        ret = exprBuilder->Sle(right, left);
        break;
      default:
        if (left.get() != be->left.get() || right.get() != be->right.get()) {
          klee::ref<Expr> kids[] = {left, right};
          ret = x->rebuild(kids);
        }
        break;
      }

      normalizedCache[x.get()] = ret;
      return ret;
    }

    unsigned numKids = x->getNumKids();
    if (numKids != 0) {
      SmallVector<klee::ref<Expr>, 4> kids;
      kids.reserve(numKids);
      bool changed = false;
      for (unsigned i = 0; i < numKids; ++i) {
        kleeExpr oldKid = x->getKid(i);
        kleeExpr newKid = normalize(oldKid);
        changed |= oldKid.get() != newKid.get();
        kids.push_back(newKid);
      }
      if (changed)
        ret = x->rebuild(kids.data());
    }

    normalizedCache[x.get()] = ret;
    return ret;
  };

  return stpBuilder->construct(normalize(e));
}

void TranslateToStpPass::printSMTExpr(kleeExpr e, raw_ostream &os,
    const std::unordered_map<std::string, unsigned> &varWidths) {
  using namespace klee;
  auto isBoolExpr = [](kleeExpr x) {
    Expr::Kind k = x->getKind();
    return k == Expr::Eq || k == Expr::Ne ||
           k == Expr::Ult || k == Expr::Ule ||
           k == Expr::Ugt || k == Expr::Uge ||
           k == Expr::Slt || k == Expr::Sle ||
           k == Expr::Sgt || k == Expr::Sge ||
           (k == Expr::Select && x->getWidth() == Expr::Bool) ||
           (k == Expr::Not && x->getWidth() == Expr::Bool);
  };

  auto printBVConst = [](const llvm::APInt &val, raw_ostream &out) {
    if (val.getBitWidth() == 1) {
      out << (val.isZero() ? "#b0" : "#b1");
      return;
    }
    llvm::SmallString<40> hexStr;
    val.toString(hexStr, 16, false);
    unsigned expectedChars = (val.getBitWidth() + 3) / 4;
    out << "#x";
    for (unsigned i = hexStr.size(); i < expectedChars; i++)
      out << '0';
    out << hexStr;
  };

  auto getConst = [](kleeExpr x, llvm::APInt &out) {
    if (!x || x->getKind() != Expr::Constant)
      return false;
    out = static_cast<const klee::ConstantExpr *>(x.get())->getAPValue();
    return true;
  };

  std::function<bool(kleeExpr, bool &)> evalBoolConst;
  evalBoolConst = [&](kleeExpr x, bool &out) {
    if (!x)
      return false;
    Expr::Kind k = x->getKind();
    if (k == Expr::Constant && x->getWidth() == Expr::Bool) {
      llvm::APInt val;
      getConst(x, val);
      out = !val.isZero();
      return true;
    }
    if (k == Expr::Not && x->getWidth() == Expr::Bool) {
      auto *ne = static_cast<const klee::NotExpr *>(x.get());
      bool inner = false;
      if (!evalBoolConst(ne->expr, inner))
        return false;
      out = !inner;
      return true;
    }

    if (k != Expr::Eq && k != Expr::Ne &&
        k != Expr::Ult && k != Expr::Ule &&
        k != Expr::Ugt && k != Expr::Uge &&
        k != Expr::Slt && k != Expr::Sle &&
        k != Expr::Sgt && k != Expr::Sge)
      return false;

    auto *be = static_cast<const klee::BinaryExpr *>(x.get());
    if (be->left == be->right) {
      out = k == Expr::Eq || k == Expr::Ule || k == Expr::Uge ||
            k == Expr::Sle || k == Expr::Sge;
      return k == Expr::Eq || k == Expr::Ne ||
             k == Expr::Ule || k == Expr::Uge ||
             k == Expr::Sle || k == Expr::Sge ||
             k == Expr::Ult || k == Expr::Ugt ||
             k == Expr::Slt || k == Expr::Sgt;
    }

    llvm::APInt lhs, rhs;
    if (!getConst(be->left, lhs) || !getConst(be->right, rhs) ||
        lhs.getBitWidth() != rhs.getBitWidth())
      return false;

    switch (k) {
    case Expr::Eq:  out = lhs == rhs; break;
    case Expr::Ne:  out = lhs != rhs; break;
    case Expr::Ult: out = lhs.ult(rhs); break;
    case Expr::Ule: out = lhs.ule(rhs); break;
    case Expr::Ugt: out = lhs.ugt(rhs); break;
    case Expr::Uge: out = lhs.uge(rhs); break;
    case Expr::Slt: out = lhs.slt(rhs); break;
    case Expr::Sle: out = lhs.sle(rhs); break;
    case Expr::Sgt: out = lhs.sgt(rhs); break;
    case Expr::Sge: out = lhs.sge(rhs); break;
    default: return false;
    }
    return true;
  };

  std::function<void(kleeExpr, raw_ostream &, bool)> printExpr;
  printExpr = [&](kleeExpr x, raw_ostream &out, bool asBool) {
    Expr::Kind kind = x->getKind();
    bool boolValue = false;

    if (evalBoolConst(x, boolValue)) {
      if (asBool)
        out << (boolValue ? "true" : "false");
      else
        out << (boolValue ? "#b1" : "#b0");
      return;
    }

    if (asBool && !isBoolExpr(x)) {
      out << "(= ";
      printExpr(x, out, false);
      out << " #b1)";
      return;
    }

    if (!asBool && isBoolExpr(x)) {
      out << "(ite ";
      printExpr(x, out, true);
      out << " #b1 #b0)";
      return;
    }

    if (kind == Expr::Constant) {
      const llvm::APInt &val =
          static_cast<const klee::ConstantExpr *>(x.get())->getAPValue();
      if (asBool) {
        out << (val.isZero() ? "false" : "true");
        return;
      }
      printBVConst(val, out);
      return;
    }

    if (kind == Expr::Read) {
      auto *re = static_cast<const klee::ReadExpr *>(x.get());
      const klee::UpdateNode *un = re->updates.head.get();
      while (un) {
        bool sameIndex = false;
        if (re->index->getKind() == Expr::Constant &&
            un->index->getKind() == Expr::Constant) {
          auto &v1 = static_cast<const klee::ConstantExpr *>(
              re->index.get())->getAPValue();
          auto &v2 = static_cast<const klee::ConstantExpr *>(
              un->index.get())->getAPValue();
          sameIndex = (v1 == v2);
        } else {
          sameIndex = (re->index == un->index);
        }
        if (sameIndex) {
          printExpr(un->value, out, asBool);
          return;
        }
        un = un->next.get();
      }

      std::string fullName = re->updates.root->name;
      if (re->updates.root->getSize() > 1) {
        fullName += "_b";
        if (re->index->getKind() == Expr::Constant)
          fullName += std::to_string(static_cast<const klee::ConstantExpr *>(
              re->index.get())->getAPValue().getZExtValue());
        else
          fullName += std::to_string(re->index->hash());
      }
      out << fullName;
      undeclaredSmtArrays.insert(fullName);
      return;
    }

    if (kind == Expr::Select) {
      auto *se = static_cast<const klee::SelectExpr *>(x.get());
      bool condValue = false;
      if (evalBoolConst(se->cond, condValue)) {
        printExpr(condValue ? se->trueExpr : se->falseExpr, out, asBool);
        return;
      }
      if (se->trueExpr == se->falseExpr) {
        printExpr(se->trueExpr, out, asBool);
        return;
      }
      if (asBool && x->getWidth() == Expr::Bool) {
        bool trueValue = false, falseValue = false;
        bool trueConst = evalBoolConst(se->trueExpr, trueValue);
        bool falseConst = evalBoolConst(se->falseExpr, falseValue);
        if (trueConst && falseConst) {
          if (trueValue == falseValue) {
            out << (trueValue ? "true" : "false");
            return;
          }
          if (trueValue && !falseValue) {
            printExpr(se->cond, out, true);
            return;
          }
          out << "(not ";
          printExpr(se->cond, out, true);
          out << ")";
          return;
        }
      }
      llvm::APInt trueConstVal, falseConstVal;
      if (getConst(se->trueExpr, trueConstVal) &&
          getConst(se->falseExpr, falseConstVal) &&
          trueConstVal == falseConstVal) {
        printBVConst(trueConstVal, out);
        return;
      }
      out << "(ite ";
      printExpr(se->cond, out, true);
      out << " ";
      printExpr(se->trueExpr, out, asBool);
      out << " ";
      printExpr(se->falseExpr, out, asBool);
      out << ")";
      return;
    }

    if (kind == Expr::Extract) {
      auto *ee = static_cast<const klee::ExtractExpr *>(x.get());
      llvm::APInt src;
      if (getConst(ee->expr, src) &&
          ee->offset < src.getBitWidth() &&
          ee->width <= src.getBitWidth() - ee->offset) {
        printBVConst(src.extractBits(ee->width, ee->offset), out);
        return;
      }
      unsigned top = ee->offset + ee->width - 1;
      out << "((_ extract " << top << " " << ee->offset << ") ";
      printExpr(ee->expr, out, false);
      out << ")";
      return;
    }

    if (kind == Expr::Concat) {
      SmallVector<kleeExpr, 8> leaves;
      std::function<void(kleeExpr)> flatten = [&](kleeExpr y) {
        if (y->getKind() == Expr::Concat) {
          auto *cc = static_cast<const klee::ConcatExpr *>(y.get());
          flatten(cc->getLeft());
          flatten(cc->getRight());
        } else {
          leaves.push_back(y);
        }
      };
      flatten(x);
      unsigned totalW = x->getWidth();
      out << "(bvor ";
      for (unsigned i = 0; i < leaves.size(); i++) {
        if (i > 0) out << " ";
        unsigned shift = i * leaves[i]->getWidth();
        out << "(bvshl ((_ zero_extend "
            << (totalW - leaves[i]->getWidth()) << ") ";
        printExpr(leaves[i], out, false);
        out << ") #x" << llvm::format_hex_no_prefix(shift, totalW / 4) << ")";
      }
      out << ")";
      return;
    }

    if (kind == Expr::ZExt) {
      auto *ze = static_cast<const klee::ZExtExpr *>(x.get());
      llvm::APInt src;
      if (getConst(ze->src, src) && ze->width >= src.getBitWidth()) {
        printBVConst(src.zext(ze->width), out);
        return;
      }
      unsigned ext = ze->width - ze->src->getWidth();
      out << "((_ zero_extend " << ext << ") ";
      printExpr(ze->src, out, false);
      out << ")";
      return;
    }

    if (kind == Expr::SExt) {
      auto *se = static_cast<const klee::SExtExpr *>(x.get());
      llvm::APInt src;
      if (getConst(se->src, src) && se->width >= src.getBitWidth()) {
        printBVConst(src.sext(se->width), out);
        return;
      }
      unsigned ext = se->width - se->src->getWidth();
      out << "((_ sign_extend " << ext << ") ";
      printExpr(se->src, out, false);
      out << ")";
      return;
    }

    if (kind == Expr::Not) {
      auto *ne = static_cast<const klee::NotExpr *>(x.get());
      llvm::APInt src;
      if (getConst(ne->expr, src)) {
        printBVConst(~src, out);
        return;
      }
      out << (x->getWidth() == Expr::Bool ? "(not " : "(bvnot ");
      printExpr(ne->expr, out, x->getWidth() == Expr::Bool);
      out << ")";
      return;
    }

    auto *be = static_cast<const klee::BinaryExpr *>(x.get());
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
    case Expr::Eq:  op = "="; break;
    case Expr::Ult: op = "bvult"; break;
    case Expr::Ule: op = "bvule"; break;
    case Expr::Ugt: op = "bvugt"; break;
    case Expr::Uge: op = "bvuge"; break;
    case Expr::Slt: op = "bvslt"; break;
    case Expr::Sle: op = "bvsle"; break;
    case Expr::Sgt: op = "bvsgt"; break;
    case Expr::Sge: op = "bvsge"; break;
    default: break;
    }
    if (op) {
      llvm::APInt lhs, rhs;
      bool lhsConst = getConst(be->left, lhs);
      bool rhsConst = getConst(be->right, rhs);

      if (lhsConst && rhsConst && lhs.getBitWidth() == rhs.getBitWidth()) {
        bool folded = true;
        llvm::APInt result(lhs.getBitWidth(), 0);
        switch (kind) {
        case Expr::Add: result = lhs + rhs; break;
        case Expr::Sub: result = lhs - rhs; break;
        case Expr::Mul: result = lhs * rhs; break;
        case Expr::UDiv:
          if (rhs.isZero()) folded = false;
          else result = lhs.udiv(rhs);
          break;
        case Expr::URem:
          if (rhs.isZero()) folded = false;
          else result = lhs.urem(rhs);
          break;
        case Expr::And: result = lhs & rhs; break;
        case Expr::Or:  result = lhs | rhs; break;
        case Expr::Xor: result = lhs ^ rhs; break;
        case Expr::Shl: {
          uint64_t shift = rhs.getLimitedValue(lhs.getBitWidth());
          if (shift >= lhs.getBitWidth()) folded = false;
          else result = lhs.shl(static_cast<unsigned>(shift));
          break;
        }
        case Expr::LShr: {
          uint64_t shift = rhs.getLimitedValue(lhs.getBitWidth());
          if (shift >= lhs.getBitWidth()) folded = false;
          else result = lhs.lshr(static_cast<unsigned>(shift));
          break;
        }
        case Expr::AShr: {
          uint64_t shift = rhs.getLimitedValue(lhs.getBitWidth());
          if (shift >= lhs.getBitWidth()) folded = false;
          else result = lhs.ashr(static_cast<unsigned>(shift));
          break;
        }
        default:
          folded = false;
          break;
        }
        if (folded) {
          printBVConst(result, out);
          return;
        }
      }

      if (rhsConst) {
        if ((kind == Expr::Add || kind == Expr::Sub ||
             kind == Expr::Or || kind == Expr::Xor ||
             kind == Expr::Shl || kind == Expr::LShr ||
             kind == Expr::AShr) && rhs.isZero()) {
          printExpr(be->left, out, false);
          return;
        }
        if (kind == Expr::Mul &&
            rhs == llvm::APInt(rhs.getBitWidth(), 1)) {
          printExpr(be->left, out, false);
          return;
        }
        if ((kind == Expr::Mul || kind == Expr::And) && rhs.isZero()) {
          printBVConst(llvm::APInt::getZero(rhs.getBitWidth()), out);
          return;
        }
        if (kind == Expr::And && rhs.isAllOnes()) {
          printExpr(be->left, out, false);
          return;
        }
      }

      if (lhsConst) {
        if ((kind == Expr::Add || kind == Expr::Or ||
             kind == Expr::Xor) && lhs.isZero()) {
          printExpr(be->right, out, false);
          return;
        }
        if (kind == Expr::Mul &&
            lhs == llvm::APInt(lhs.getBitWidth(), 1)) {
          printExpr(be->right, out, false);
          return;
        }
        if ((kind == Expr::Mul || kind == Expr::And) && lhs.isZero()) {
          printBVConst(llvm::APInt::getZero(lhs.getBitWidth()), out);
          return;
        }
        if (kind == Expr::And && lhs.isAllOnes()) {
          printExpr(be->right, out, false);
          return;
        }
      }

      out << "(" << op << " ";
      printExpr(be->left, out, false);
      out << " ";
      printExpr(be->right, out, false);
      out << ")";
      return;
    }

    out << "#x0 ;; unhandled kind: " << kind;
  };

  printExpr(e, os, false);
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
    if (outputSizes.count(kv.first)) {
      varWidths[kv.second] = outputSizes[kv.first] * 8;
    } else if (output.count(kv.first) && output[kv.first]) {
      Type *outTy = output[kv.first]->getType();
      unsigned bw = outTy->getPrimitiveSizeInBits();
      if (bw == 0 && outTy->isSized())
        bw = dataLayout->getTypeStoreSize(outTy) * 8;
      if (bw == 0) bw = 32;
      varWidths[kv.second] = bw;
    }
  }

  auto emitWithStpPrinter = [&]() -> bool {
    std::vector<klee::ExprHandle> liveExprs;
    std::vector<::VCExpr> assertions;

    auto sanitizeName = [](std::string name) {
      for (char &c : name)
        if (!isalnum(c) && c != '_') c = '_';
      return name;
    };

    auto makeSymbol = [&](const std::string &name, unsigned bitWidth)
        -> klee::ExprHandle {
      ::VCType type = (bitWidth == klee::Expr::Bool)
                          ? vc_boolType(vc)
                          : vc_bvType(vc, bitWidth);
      klee::ExprHandle symbol(vc_varExpr(vc, name.c_str(), type));
      vc_DeleteExpr(type);
      return symbol;
    };

    auto addEquality = [&](const std::string &name, unsigned bitWidth,
                           kleeExpr rhs) -> bool {
      if (!rhs || name.empty() || bitWidth == 0)
        return false;
      klee::ExprHandle lhs = makeSymbol(name, bitWidth);
      klee::ExprHandle rhsStp = convertKleeToStpExpr(rhs);
      if (!lhs || !rhsStp)
        return false;
      klee::ExprHandle eq(vc_eqExpr(vc, lhs, rhsStp));
      if (!eq)
        return false;
      liveExprs.push_back(lhs);
      liveExprs.push_back(rhsStp);
      liveExprs.push_back(eq);
      assertions.push_back(static_cast<::VCExpr>(eq));
      return true;
    };

    for (auto &it : outputKleeExpr) {
      Value *v = it.first;
      kleeExpr e = it.second;
      if (!e) continue;

      std::string varName;
      if (outputNames.count(v))
        varName = outputNames[v];
      if (varName.empty())
        varName = v->getName().str();
      varName = sanitizeName(varName);
      if (varName.empty()) continue;

      unsigned bitWidth = 0;
      auto widthIt = varWidths.find(varName);
      if (widthIt != varWidths.end())
        bitWidth = widthIt->second;
      else
        bitWidth = e->getWidth();
      if (!addEquality(varName, bitWidth, e))
        return false;
    }

    for (auto &kv : namedLocalArrays) {
      Value *v = kv.first;
      if (!namedLocalITEs.count(v)) continue;
      std::string name = kv.second->name;
      if (name.empty()) continue;
      unsigned bitWidth = kv.second->getRange();
      if (bitWidth == 0) bitWidth = namedLocalITEs[v]->getWidth();
      if (!addEquality(name, bitWidth, namedLocalITEs[v]))
        return false;
    }

    klee::ExprHandle query;
    if (assertions.empty()) {
      query = klee::ExprHandle(vc_trueExpr(vc));
      liveExprs.push_back(query);
    } else if (assertions.size() == 1) {
      query = liveExprs.back();
    } else {
      query = klee::ExprHandle(
          vc_andExprN(vc, assertions.data(), assertions.size()));
      if (!query)
        return false;
      liveExprs.push_back(query);
    }

    std::error_code EC;
    llvm::raw_fd_ostream ofs(outFileName, EC);
    if (EC) {
      errs() << "Cannot open " << outFileName << ": " << EC.message() << "\n";
      return false;
    }

    struct STPShim {
      stp::STPMgr *bm;
    };
    auto *stpObj = static_cast<STPShim *>(vc);
    auto *queryNode = static_cast<stp::ASTNode *>(
        static_cast<::VCExpr>(query));
    if (!stpObj || !stpObj->bm || !queryNode)
      return false;

    std::ostringstream ss;
    printer::SMTLIB2_PrintBack(ss, *queryNode, stpObj->bm, false);
    ss << "(check-sat)\n";
    ofs << ss.str();
    return true;
  };

  if (emitWithStpPrinter())
    return;

  errs() << "Warning: STP SMT2 printer failed; falling back to local printer\n";

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

  // Helper: walk expression tree, count occurrences of each node.
  // Nodes appearing multiple times get let-variable names.
  std::unordered_map<const klee::Expr *, unsigned> exprCount;
  std::function<void(kleeExpr)> countExprs;
  countExprs = [&](kleeExpr x) {
    if (!x) return;
    if (x->getKind() == klee::Expr::Constant) return;
    exprCount[x.get()]++;
    if (exprCount[x.get()] == 1) {  // only recurse on first visit
      for (unsigned i = 0; i < x->getNumKids(); i++)
        countExprs(x->getKid(i));
    }
  };

  // Assign let-names to nodes with count > 1
  std::unordered_map<const klee::Expr *, std::string> letNames;
  unsigned letIdx = 0;
  auto assignLetNames = [&]() {
    for (auto &kv : exprCount)
      if (kv.second > 1 && kv.first->getKind() != klee::Expr::Read)
        letNames[kv.first] = "let_" + std::to_string(letIdx++);
  };

  // Helper: print with let-substitution — shared nodes are replaced
  // by their let-name, and a flat let-binding wraps the whole assert.
  std::function<void(kleeExpr, llvm::raw_ostream &)> printLetRef;
  printLetRef = [&](kleeExpr x, llvm::raw_ostream &os) {
    auto it = letNames.find(x.get());
    if (it != letNames.end()) {
      os << it->second;
      return;
    }
    printSMTExpr(x, os, varWidths);
  };

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

    // Collect shared nodes, assign let-names, print with let wrapper
    exprCount.clear();
    letNames.clear();
    letIdx = 0;
    countExprs(e);
    assignLetNames();
    assertOS << "(assert ";
    if (!letNames.empty()) {
      assertOS << "(let (";
      for (auto &kv : letNames) {
        assertOS << "(" << kv.second << " ";
        printSMTExpr(kleeExpr(const_cast<klee::Expr*>(kv.first)), assertOS, varWidths);
        assertOS << ") ";
      }
      assertOS << ") ";
    }
    assertOS << "(= " << varName << " ";
    if (!letNames.empty())
      printLetRef(e, assertOS);
    else
      printSMTExpr(e, assertOS, varWidths);
    assertOS << ")";
    if (!letNames.empty()) assertOS << ")";
    assertOS << ")\n";
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

  // Emit assertions for named local arrays (two-phase ITE optimization).
  // Each local int alloca gets a separate assert defining its ITE value,
  // avoiding O(2^n) inlined subexpression explosion.
  {
    std::string localBuf;
    llvm::raw_string_ostream localOS(localBuf);
    // Emit assertions for named arrays: those with ITE get their ITE,
    // those without get a free symbolic declaration.
    for (auto &kv : namedLocalArrays) {
      Value *v = kv.first;
      std::string name = kv.second->name;
      if (name.empty()) continue;
      unsigned bw = kv.second->getRange();
      if (bw == 0 || bw > 64) bw = 32;
      if (!varWidths.count(name)) varWidths[name] = bw;

      if (namedLocalITEs.count(v)) {
        kleeExpr e = namedLocalITEs[v];
        localOS << "(assert (= " << name << " ";
        printSMTExpr(e, localOS, varWidths);
        localOS << "))\n";
      }
    }
    localOS.flush();
    assertBuf += localBuf;
  }

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
