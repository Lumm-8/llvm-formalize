//===- LoopUnroll.cpp - Loop unroller pass --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass implements a simple loop unroller.  It works best when loops have
// been canonicalized by the -indvars pass, allowing it to determine the trip
// counts of loops easily.
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar/LoopUnrollPass.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/LoopIterator.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopUnrollAnalyzer.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ProfileSummaryInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/PassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/LoopPassManager.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/LoopPeel.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"
#include "llvm/Transforms/Utils/LoopUtils.h"
#include "llvm/Transforms/Utils/ScalarEvolutionExpander.h"
#include "llvm/Transforms/Utils/SizeOpts.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"
#include "llvm/Transforms/Utils/UnrollLoop.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <tuple>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "loop-unroll"

cl::opt<bool> llvm::ForgetSCEVInLoopUnroll(
    "forget-scev-loop-unroll", cl::init(false), cl::Hidden,
    cl::desc("Forget everything in SCEV when doing LoopUnroll, instead of just"
             " the current top-most loop. This is sometimes preferred to reduce"
             " compile time."));

static cl::opt<unsigned>
    UnrollThreshold("unroll-threshold", cl::Hidden,
                    cl::desc("The cost threshold for loop unrolling"));

static cl::opt<unsigned>
    UnrollOptSizeThreshold(
      "unroll-optsize-threshold", cl::init(0), cl::Hidden,
      cl::desc("The cost threshold for loop unrolling when optimizing for "
               "size"));

static cl::opt<unsigned> UnrollPartialThreshold(
    "unroll-partial-threshold", cl::Hidden,
    cl::desc("The cost threshold for partial loop unrolling"));

static cl::opt<unsigned> UnrollMaxPercentThresholdBoost(
    "unroll-max-percent-threshold-boost", cl::init(400), cl::Hidden,
    cl::desc("The maximum 'boost' (represented as a percentage >= 100) applied "
             "to the threshold when aggressively unrolling a loop due to the "
             "dynamic cost savings. If completely unrolling a loop will reduce "
             "the total runtime from X to Y, we boost the loop unroll "
             "threshold to DefaultThreshold*std::min(MaxPercentThresholdBoost, "
             "X/Y). This limit avoids excessive code bloat."));

static cl::opt<unsigned> UnrollMaxIterationsCountToAnalyze(
    "unroll-max-iteration-count-to-analyze", cl::init(10), cl::Hidden,
    cl::desc("Don't allow loop unrolling to simulate more than this number of "
             "iterations when checking full unroll profitability"));

static cl::opt<unsigned> UnrollCount(
    "unroll-count", cl::Hidden,
    cl::desc("Use this unroll count for all loops including those with "
             "unroll_count pragma values, for testing purposes"));

static cl::opt<unsigned> UnrollMaxCount(
    "unroll-max-count", cl::Hidden,
    cl::desc("Set the max unroll count for partial and runtime unrolling, for"
             "testing purposes"));

static cl::opt<unsigned> UnrollFullMaxCount(
    "unroll-full-max-count", cl::Hidden,
    cl::desc(
        "Set the max unroll count for full unrolling, for testing purposes"));

static cl::opt<bool>
    UnrollAllowPartial("unroll-allow-partial", cl::Hidden,
                       cl::desc("Allows loops to be partially unrolled until "
                                "-unroll-threshold loop size is reached."));

static cl::opt<bool> UnrollAllowRemainder(
    "unroll-allow-remainder", cl::Hidden,
    cl::desc("Allow generation of a loop remainder (extra iterations) "
             "when unrolling a loop."));

static cl::opt<bool>
    UnrollRuntime("unroll-runtime", cl::Hidden,
                  cl::desc("Unroll loops with run-time trip counts"));

static cl::opt<unsigned> UnrollMaxUpperBound(
    "unroll-max-upperbound", cl::init(8), cl::Hidden,
    cl::desc(
        "The max of trip count upper bound that is considered in unrolling"));

static cl::opt<unsigned> PragmaUnrollThreshold(
    "pragma-unroll-threshold", cl::init(16 * 1024), cl::Hidden,
    cl::desc("Unrolled size limit for loops with an unroll(full) or "
             "unroll_count pragma."));

static cl::opt<unsigned> FlatLoopTripCountThreshold(
    "flat-loop-tripcount-threshold", cl::init(5), cl::Hidden,
    cl::desc("If the runtime tripcount for the loop is lower than the "
             "threshold, the loop is considered as flat and will be less "
             "aggressively unrolled."));

static cl::opt<bool> UnrollUnrollRemainder(
  "unroll-remainder", cl::Hidden,
  cl::desc("Allow the loop remainder to be unrolled."));

// This option isn't ever intended to be enabled, it serves to allow
// experiments to check the assumptions about when this kind of revisit is
// necessary.
static cl::opt<bool> UnrollRevisitChildLoops(
    "unroll-revisit-child-loops", cl::Hidden,
    cl::desc("Enqueue and re-visit child loops in the loop PM after unrolling. "
             "This shouldn't typically be needed as child loops (or their "
             "clones) were already visited."));

static cl::opt<unsigned> UnrollThresholdAggressive(
    "unroll-threshold-aggressive", cl::init(300), cl::Hidden,
    cl::desc("Threshold (max size of unrolled loop) to use in aggressive (O3) "
             "optimizations"));
static cl::opt<unsigned>
    UnrollThresholdDefault("unroll-threshold-default", cl::init(150),
                           cl::Hidden,
                           cl::desc("Default threshold (max size of unrolled "
                                    "loop), used in all but O3 optimizations"));

static cl::opt<unsigned> PragmaUnrollFullMaxIterations(
    "pragma-unroll-full-max-iterations", cl::init(1'000'000), cl::Hidden,
    cl::desc("Maximum allowed iterations to unroll under pragma unroll full."));

/// A magic value for use with the Threshold parameter to indicate
/// that the loop unroll should be performed regardless of how much
/// code expansion would result.
static const unsigned NoThreshold = std::numeric_limits<unsigned>::max();

/// Gather the various unrolling parameters based on the defaults, compiler
/// flags, TTI overrides and user specified parameters.
TargetTransformInfo::UnrollingPreferences llvm::gatherUnrollingPreferences(
    Loop *L, ScalarEvolution &SE, const TargetTransformInfo &TTI,
    BlockFrequencyInfo *BFI, ProfileSummaryInfo *PSI,
    OptimizationRemarkEmitter &ORE, int OptLevel,
    std::optional<unsigned> UserThreshold, std::optional<unsigned> UserCount,
    std::optional<bool> UserAllowPartial, std::optional<bool> UserRuntime,
    std::optional<bool> UserUpperBound,
    std::optional<unsigned> UserFullUnrollMaxCount) {
  TargetTransformInfo::UnrollingPreferences UP;

  // Set up the defaults
  UP.Threshold =
      OptLevel > 2 ? UnrollThresholdAggressive : UnrollThresholdDefault;
  UP.MaxPercentThresholdBoost = 400;
  UP.OptSizeThreshold = UnrollOptSizeThreshold;
  UP.PartialThreshold = 150;
  UP.PartialOptSizeThreshold = UnrollOptSizeThreshold;
  UP.Count = 0;
  UP.DefaultUnrollRuntimeCount = 8;
  UP.MaxCount = std::numeric_limits<unsigned>::max();
  UP.MaxUpperBound = UnrollMaxUpperBound;
  UP.FullUnrollMaxCount = std::numeric_limits<unsigned>::max();
  UP.BEInsns = 2;
  UP.Partial = false;
  UP.Runtime = false;
  UP.AllowRemainder = true;
  UP.UnrollRemainder = false;
  UP.AllowExpensiveTripCount = false;
  UP.Force = false;
  UP.UpperBound = false;
  UP.UnrollAndJam = false;
  UP.UnrollAndJamInnerLoopThreshold = 60;
  UP.MaxIterationsCountToAnalyze = UnrollMaxIterationsCountToAnalyze;
  UP.SCEVExpansionBudget = SCEVCheapExpansionBudget;
  UP.RuntimeUnrollMultiExit = false;

  // Override with any target specific settings
  TTI.getUnrollingPreferences(L, SE, UP, &ORE);

  // Apply size attributes
  bool OptForSize = L->getHeader()->getParent()->hasOptSize() ||
                    // Let unroll hints / pragmas take precedence over PGSO.
                    (hasUnrollTransformation(L) != TM_ForcedByUser &&
                     llvm::shouldOptimizeForSize(L->getHeader(), PSI, BFI,
                                                 PGSOQueryType::IRPass));
  if (OptForSize) {
    UP.Threshold = UP.OptSizeThreshold;
    UP.PartialThreshold = UP.PartialOptSizeThreshold;
    UP.MaxPercentThresholdBoost = 100;
  }

  // Apply any user values specified by cl::opt
  if (UnrollThreshold.getNumOccurrences() > 0)
    UP.Threshold = UnrollThreshold;
  if (UnrollPartialThreshold.getNumOccurrences() > 0)
    UP.PartialThreshold = UnrollPartialThreshold;
  if (UnrollMaxPercentThresholdBoost.getNumOccurrences() > 0)
    UP.MaxPercentThresholdBoost = UnrollMaxPercentThresholdBoost;
  if (UnrollMaxCount.getNumOccurrences() > 0)
    UP.MaxCount = UnrollMaxCount;
  if (UnrollMaxUpperBound.getNumOccurrences() > 0)
    UP.MaxUpperBound = UnrollMaxUpperBound;
  if (UnrollFullMaxCount.getNumOccurrences() > 0)
    UP.FullUnrollMaxCount = UnrollFullMaxCount;
  if (UnrollAllowPartial.getNumOccurrences() > 0)
    UP.Partial = UnrollAllowPartial;
  if (UnrollAllowRemainder.getNumOccurrences() > 0)
    UP.AllowRemainder = UnrollAllowRemainder;
  if (UnrollRuntime.getNumOccurrences() > 0)
    UP.Runtime = UnrollRuntime;
  if (UnrollMaxUpperBound == 0)
    UP.UpperBound = false;
  if (UnrollUnrollRemainder.getNumOccurrences() > 0)
    UP.UnrollRemainder = UnrollUnrollRemainder;
  if (UnrollMaxIterationsCountToAnalyze.getNumOccurrences() > 0)
    UP.MaxIterationsCountToAnalyze = UnrollMaxIterationsCountToAnalyze;

  // Apply user values provided by argument
  if (UserThreshold) {
    UP.Threshold = *UserThreshold;
    UP.PartialThreshold = *UserThreshold;
  }
  if (UserCount)
    UP.Count = *UserCount;
  if (UserAllowPartial)
    UP.Partial = *UserAllowPartial;
  if (UserRuntime)
    UP.Runtime = *UserRuntime;
  if (UserUpperBound)
    UP.UpperBound = *UserUpperBound;
  if (UserFullUnrollMaxCount)
    UP.FullUnrollMaxCount = *UserFullUnrollMaxCount;

  return UP;
}

namespace {

/// A struct to densely store the state of an instruction after unrolling at
/// each iteration.
///
/// This is designed to work like a tuple of <Instruction *, int> for the
/// purposes of hashing and lookup, but to be able to associate two boolean
/// states with each key.
struct UnrolledInstState {
  Instruction *I;
  int Iteration : 30;
  unsigned IsFree : 1;
  unsigned IsCounted : 1;
};

/// Hashing and equality testing for a set of the instruction states.
struct UnrolledInstStateKeyInfo {
  using PtrInfo = DenseMapInfo<Instruction *>;
  using PairInfo = DenseMapInfo<std::pair<Instruction *, int>>;

  static inline UnrolledInstState getEmptyKey() {
    return {PtrInfo::getEmptyKey(), 0, 0, 0};
  }

  static inline UnrolledInstState getTombstoneKey() {
    return {PtrInfo::getTombstoneKey(), 0, 0, 0};
  }

  static inline unsigned getHashValue(const UnrolledInstState &S) {
    return PairInfo::getHashValue({S.I, S.Iteration});
  }

  static inline bool isEqual(const UnrolledInstState &LHS,
                             const UnrolledInstState &RHS) {
    return PairInfo::isEqual({LHS.I, LHS.Iteration}, {RHS.I, RHS.Iteration});
  }
};

struct EstimatedUnrollCost {
  /// The estimated cost after unrolling.
  unsigned UnrolledCost;

  /// The estimated dynamic cost of executing the instructions in the
  /// rolled form.
  unsigned RolledDynamicCost;
};

struct PragmaInfo {
  PragmaInfo(bool UUC, bool PFU, unsigned PC, bool PEU)
      : UserUnrollCount(UUC), PragmaFullUnroll(PFU), PragmaCount(PC),
        PragmaEnableUnroll(PEU) {}
  const bool UserUnrollCount;
  const bool PragmaFullUnroll;
  const unsigned PragmaCount;
  const bool PragmaEnableUnroll;
};

} // end anonymous namespace

/// Figure out if the loop is worth full unrolling.
///
/// Complete loop unrolling can make some loads constant, and we need to know
/// if that would expose any further optimization opportunities.  This routine
/// estimates this optimization.  It computes cost of unrolled loop
/// (UnrolledCost) and dynamic cost of the original loop (RolledDynamicCost). By
/// dynamic cost we mean that we won't count costs of blocks that are known not
/// to be executed (i.e. if we have a branch in the loop and we know that at the
/// given iteration its condition would be resolved to true, we won't add up the
/// cost of the 'false'-block).
/// \returns Optional value, holding the RolledDynamicCost and UnrolledCost. If
/// the analysis failed (no benefits expected from the unrolling, or the loop is
/// too big to analyze), the returned value is std::nullopt.
static std::optional<EstimatedUnrollCost> analyzeLoopUnrollCost(
    const Loop *L, unsigned TripCount, DominatorTree &DT, ScalarEvolution &SE,
    const SmallPtrSetImpl<const Value *> &EphValues,
    const TargetTransformInfo &TTI, unsigned MaxUnrolledLoopSize,
    unsigned MaxIterationsCountToAnalyze) {
  // We want to be able to scale offsets by the trip count and add more offsets
  // to them without checking for overflows, and we already don't want to
  // analyze *massive* trip counts, so we force the max to be reasonably small.
  assert(MaxIterationsCountToAnalyze <
             (unsigned)(std::numeric_limits<int>::max() / 2) &&
         "The unroll iterations max is too large!");

  // Only analyze inner loops. We can't properly estimate cost of nested loops
  // and we won't visit inner loops again anyway.
  if (!L->isInnermost())
    return std::nullopt;

  // Don't simulate loops with a big or unknown tripcount
  if (!TripCount || TripCount > MaxIterationsCountToAnalyze)
    return std::nullopt;

  SmallSetVector<BasicBlock *, 16> BBWorklist;
  SmallSetVector<std::pair<BasicBlock *, BasicBlock *>, 4> ExitWorklist;
  DenseMap<Value *, Value *> SimplifiedValues;
  SmallVector<std::pair<Value *, Value *>, 4> SimplifiedInputValues;

  // The estimated cost of the unrolled form of the loop. We try to estimate
  // this by simplifying as much as we can while computing the estimate.
  InstructionCost UnrolledCost = 0;

  // We also track the estimated dynamic (that is, actually executed) cost in
  // the rolled form. This helps identify cases when the savings from unrolling
  // aren't just exposing dead control flows, but actual reduced dynamic
  // instructions due to the simplifications which we expect to occur after
  // unrolling.
  InstructionCost RolledDynamicCost = 0;

  // We track the simplification of each instruction in each iteration. We use
  // this to recursively merge costs into the unrolled cost on-demand so that
  // we don't count the cost of any dead code. This is essentially a map from
  // <instruction, int> to <bool, bool>, but stored as a densely packed struct.
  DenseSet<UnrolledInstState, UnrolledInstStateKeyInfo> InstCostMap;

  // A small worklist used to accumulate cost of instructions from each
  // observable and reached root in the loop.
  SmallVector<Instruction *, 16> CostWorklist;

  // PHI-used worklist used between iterations while accumulating cost.
  SmallVector<Instruction *, 4> PHIUsedList;

  // Helper function to accumulate cost for instructions in the loop.
  auto AddCostRecursively = [&](Instruction &RootI, int Iteration) {
    assert(Iteration >= 0 && "Cannot have a negative iteration!");
    assert(CostWorklist.empty() && "Must start with an empty cost list");
    assert(PHIUsedList.empty() && "Must start with an empty phi used list");
    CostWorklist.push_back(&RootI);
    TargetTransformInfo::TargetCostKind CostKind =
      RootI.getFunction()->hasMinSize() ?
      TargetTransformInfo::TCK_CodeSize :
      TargetTransformInfo::TCK_SizeAndLatency;
    for (;; --Iteration) {
      do {
        Instruction *I = CostWorklist.pop_back_val();

        // InstCostMap only uses I and Iteration as a key, the other two values
        // don't matter here.
        auto CostIter = InstCostMap.find({I, Iteration, 0, 0});
        if (CostIter == InstCostMap.end())
          // If an input to a PHI node comes from a dead path through the loop
          // we may have no cost data for it here. What that actually means is
          // that it is free.
          continue;
        auto &Cost = *CostIter;
        if (Cost.IsCounted)
          // Already counted this instruction.
          continue;

        // Mark that we are counting the cost of this instruction now.
        Cost.IsCounted = true;

        // If this is a PHI node in the loop header, just add it to the PHI set.
        if (auto *PhiI = dyn_cast<PHINode>(I))
          if (PhiI->getParent() == L->getHeader()) {
            assert(Cost.IsFree && "Loop PHIs shouldn't be evaluated as they "
                                  "inherently simplify during unrolling.");
            if (Iteration == 0)
              continue;

            // Push the incoming value from the backedge into the PHI used list
            // if it is an in-loop instruction. We'll use this to populate the
            // cost worklist for the next iteration (as we count backwards).
            if (auto *OpI = dyn_cast<Instruction>(
                    PhiI->getIncomingValueForBlock(L->getLoopLatch())))
              if (L->contains(OpI))
                PHIUsedList.push_back(OpI);
            continue;
          }

        // First accumulate the cost of this instruction.
        if (!Cost.IsFree) {
          // Consider simplified operands in instruction cost.
          SmallVector<Value *, 4> Operands;
          transform(I->operands(), std::back_inserter(Operands),
                    [&](Value *Op) {
                      if (auto Res = SimplifiedValues.lookup(Op))
                        return Res;
                      return Op;
                    });
          UnrolledCost += TTI.getInstructionCost(I, Operands, CostKind);
          LLVM_DEBUG(dbgs() << "Adding cost of instruction (iteration "
                            << Iteration << "): ");
          LLVM_DEBUG(I->dump());
        }

        // We must count the cost of every operand which is not free,
        // recursively. If we reach a loop PHI node, simply add it to the set
        // to be considered on the next iteration (backwards!).
        for (Value *Op : I->operands()) {
          // Check whether this operand is free due to being a constant or
          // outside the loop.
          auto *OpI = dyn_cast<Instruction>(Op);
          if (!OpI || !L->contains(OpI))
            continue;

          // Otherwise accumulate its cost.
          CostWorklist.push_back(OpI);
        }
      } while (!CostWorklist.empty());

      if (PHIUsedList.empty())
        // We've exhausted the search.
        break;

      assert(Iteration > 0 &&
             "Cannot track PHI-used values past the first iteration!");
      CostWorklist.append(PHIUsedList.begin(), PHIUsedList.end());
      PHIUsedList.clear();
    }
  };

  // Ensure that we don't violate the loop structure invariants relied on by
  // this analysis.
  assert(L->isLoopSimplifyForm() && "Must put loop into normal form first.");
  assert(L->isLCSSAForm(DT) &&
         "Must have loops in LCSSA form to track live-out values.");

  LLVM_DEBUG(dbgs() << "Starting LoopUnroll profitability analysis...\n");

  TargetTransformInfo::TargetCostKind CostKind =
    L->getHeader()->getParent()->hasMinSize() ?
    TargetTransformInfo::TCK_CodeSize : TargetTransformInfo::TCK_SizeAndLatency;
  // Simulate execution of each iteration of the loop counting instructions,
  // which would be simplified.
  // Since the same load will take different values on different iterations,
  // we literally have to go through all loop's iterations.
  for (unsigned Iteration = 0; Iteration < TripCount; ++Iteration) {
    LLVM_DEBUG(dbgs() << " Analyzing iteration " << Iteration << "\n");

    // Prepare for the iteration by collecting any simplified entry or backedge
    // inputs.
    for (Instruction &I : *L->getHeader()) {
      auto *PHI = dyn_cast<PHINode>(&I);
      if (!PHI)
        break;

      // The loop header PHI nodes must have exactly two input: one from the
      // loop preheader and one from the loop latch.
      assert(
          PHI->getNumIncomingValues() == 2 &&
          "Must have an incoming value only for the preheader and the latch.");

      Value *V = PHI->getIncomingValueForBlock(
          Iteration == 0 ? L->getLoopPreheader() : L->getLoopLatch());
      if (Iteration != 0 && SimplifiedValues.count(V))
        V = SimplifiedValues.lookup(V);
      SimplifiedInputValues.push_back({PHI, V});
    }

    // Now clear and re-populate the map for the next iteration.
    SimplifiedValues.clear();
    while (!SimplifiedInputValues.empty())
      SimplifiedValues.insert(SimplifiedInputValues.pop_back_val());

    UnrolledInstAnalyzer Analyzer(Iteration, SimplifiedValues, SE, L);

    BBWorklist.clear();
    BBWorklist.insert(L->getHeader());
    // Note that we *must not* cache the size, this loop grows the worklist.
    for (unsigned Idx = 0; Idx != BBWorklist.size(); ++Idx) {
      BasicBlock *BB = BBWorklist[Idx];

      // Visit all instructions in the given basic block and try to simplify
      // it.  We don't change the actual IR, just count optimization
      // opportunities.
      for (Instruction &I : *BB) {
        // These won't get into the final code - don't even try calculating the
        // cost for them.
        if (EphValues.count(&I))
          continue;

        // Track this instruction's expected baseline cost when executing the
        // rolled loop form.
        RolledDynamicCost += TTI.getInstructionCost(&I, CostKind);

        // Visit the instruction to analyze its loop cost after unrolling,
        // and if the visitor returns true, mark the instruction as free after
        // unrolling and continue.
        bool IsFree = Analyzer.visit(I);
        bool Inserted = InstCostMap.insert({&I, (int)Iteration,
                                           (unsigned)IsFree,
                                           /*IsCounted*/ false}).second;
        (void)Inserted;
        assert(Inserted && "Cannot have a state for an unvisited instruction!");

        if (IsFree)
          continue;

        // Can't properly model a cost of a call.
        // FIXME: With a proper cost model we should be able to do it.
        if (auto *CI = dyn_cast<CallInst>(&I)) {
          const Function *Callee = CI->getCalledFunction();
          if (!Callee || TTI.isLoweredToCall(Callee)) {
            LLVM_DEBUG(dbgs() << "Can't analyze cost of loop with call\n");
            return std::nullopt;
          }
        }

        // If the instruction might have a side-effect recursively account for
        // the cost of it and all the instructions leading up to it.
        if (I.mayHaveSideEffects())
          AddCostRecursively(I, Iteration);

        // If unrolled body turns out to be too big, bail out.
        if (UnrolledCost > MaxUnrolledLoopSize) {
          LLVM_DEBUG(dbgs() << "  Exceeded threshold.. exiting.\n"
                            << "  UnrolledCost: " << UnrolledCost
                            << ", MaxUnrolledLoopSize: " << MaxUnrolledLoopSize
                            << "\n");
          return std::nullopt;
        }
      }

      Instruction *TI = BB->getTerminator();

      auto getSimplifiedConstant = [&](Value *V) -> Constant * {
        if (SimplifiedValues.count(V))
          V = SimplifiedValues.lookup(V);
        return dyn_cast<Constant>(V);
      };

      // Add in the live successors by first checking whether we have terminator
      // that may be simplified based on the values simplified by this call.
      BasicBlock *KnownSucc = nullptr;
      if (BranchInst *BI = dyn_cast<BranchInst>(TI)) {
        if (BI->isConditional()) {
          if (auto *SimpleCond = getSimplifiedConstant(BI->getCondition())) {
            // Just take the first successor if condition is undef
            if (isa<UndefValue>(SimpleCond))
              KnownSucc = BI->getSuccessor(0);
            else if (ConstantInt *SimpleCondVal =
                         dyn_cast<ConstantInt>(SimpleCond))
              KnownSucc = BI->getSuccessor(SimpleCondVal->isZero() ? 1 : 0);
          }
        }
      } else if (SwitchInst *SI = dyn_cast<SwitchInst>(TI)) {
        if (auto *SimpleCond = getSimplifiedConstant(SI->getCondition())) {
          // Just take the first successor if condition is undef
          if (isa<UndefValue>(SimpleCond))
            KnownSucc = SI->getSuccessor(0);
          else if (ConstantInt *SimpleCondVal =
                       dyn_cast<ConstantInt>(SimpleCond))
            KnownSucc = SI->findCaseValue(SimpleCondVal)->getCaseSuccessor();
        }
      }
      if (KnownSucc) {
        if (L->contains(KnownSucc))
          BBWorklist.insert(KnownSucc);
        else
          ExitWorklist.insert({BB, KnownSucc});
        continue;
      }

      // Add BB's successors to the worklist.
      for (BasicBlock *Succ : successors(BB))
        if (L->contains(Succ))
          BBWorklist.insert(Succ);
        else
          ExitWorklist.insert({BB, Succ});
      AddCostRecursively(*TI, Iteration);
    }

    // If we found no optimization opportunities on the first iteration, we
    // won't find them on later ones too.
    if (UnrolledCost == RolledDynamicCost) {
      LLVM_DEBUG(dbgs() << "  No opportunities found.. exiting.\n"
                        << "  UnrolledCost: " << UnrolledCost << "\n");
      return std::nullopt;
    }
  }

  while (!ExitWorklist.empty()) {
    BasicBlock *ExitingBB, *ExitBB;
    std::tie(ExitingBB, ExitBB) = ExitWorklist.pop_back_val();

    for (Instruction &I : *ExitBB) {
      auto *PN = dyn_cast<PHINode>(&I);
      if (!PN)
        break;

      Value *Op = PN->getIncomingValueForBlock(ExitingBB);
      if (auto *OpI = dyn_cast<Instruction>(Op))
        if (L->contains(OpI))
          AddCostRecursively(*OpI, TripCount - 1);
    }
  }

  assert(UnrolledCost.isValid() && RolledDynamicCost.isValid() &&
         "All instructions must have a valid cost, whether the "
         "loop is rolled or unrolled.");

  LLVM_DEBUG(dbgs() << "Analysis finished:\n"
                    << "UnrolledCost: " << UnrolledCost << ", "
                    << "RolledDynamicCost: " << RolledDynamicCost << "\n");
  return {{unsigned(UnrolledCost.getValue()),
           unsigned(RolledDynamicCost.getValue())}};
}

UnrollCostEstimator::UnrollCostEstimator(
    const Loop *L, const TargetTransformInfo &TTI,
    const SmallPtrSetImpl<const Value *> &EphValues, unsigned BEInsns) {
  CodeMetrics Metrics;
  for (BasicBlock *BB : L->blocks())
    Metrics.analyzeBasicBlock(BB, TTI, EphValues, /* PrepareForLTO= */ false,
                              L);
  NumInlineCandidates = Metrics.NumInlineCandidates;
  NotDuplicatable = Metrics.notDuplicatable;
  Convergence = Metrics.Convergence;
  LoopSize = Metrics.NumInsts;
  ConvergenceAllowsRuntime =
      Metrics.Convergence != ConvergenceKind::Uncontrolled &&
      !getLoopConvergenceHeart(L);

  // Don't allow an estimate of size zero.  This would allows unrolling of loops
  // with huge iteration counts, which is a compile time problem even if it's
  // not a problem for code quality. Also, the code using this size may assume
  // that each loop has at least three instructions (likely a conditional
  // branch, a comparison feeding that branch, and some kind of loop increment
  // feeding that comparison instruction).
  if (LoopSize.isValid() && LoopSize < BEInsns + 1)
    // This is an open coded max() on InstructionCost
    LoopSize = BEInsns + 1;
}

bool UnrollCostEstimator::canUnroll() const {
  switch (Convergence) {
  case ConvergenceKind::ExtendedLoop:
    LLVM_DEBUG(dbgs() << "  Convergence prevents unrolling.\n");
    return false;
  default:
    break;
  }
  if (!LoopSize.isValid()) {
    LLVM_DEBUG(dbgs() << "  Invalid loop size prevents unrolling.\n");
    return false;
  }
  if (NotDuplicatable) {
    LLVM_DEBUG(dbgs() << "  Non-duplicatable blocks prevent unrolling.\n");
    return false;
  }
  return true;
}

uint64_t UnrollCostEstimator::getUnrolledLoopSize(
    const TargetTransformInfo::UnrollingPreferences &UP,
    unsigned CountOverwrite) const {
  unsigned LS = LoopSize.getValue();
  assert(LS >= UP.BEInsns && "LoopSize should not be less than BEInsns!");
  if (CountOverwrite)
    return static_cast<uint64_t>(LS - UP.BEInsns) * CountOverwrite + UP.BEInsns;
  else
    return static_cast<uint64_t>(LS - UP.BEInsns) * UP.Count + UP.BEInsns;
}

// Returns the loop hint metadata node with the given name (for example,
// "llvm.loop.unroll.count").  If no such metadata node exists, then nullptr is
// returned.
static MDNode *getUnrollMetadataForLoop(const Loop *L, StringRef Name) {
  if (MDNode *LoopID = L->getLoopID())
    return GetUnrollMetadata(LoopID, Name);
  return nullptr;
}

// Returns true if the loop has an unroll(full) pragma.
static bool hasUnrollFullPragma(const Loop *L) {
  return getUnrollMetadataForLoop(L, "llvm.loop.unroll.full");
}

// Returns true if the loop has an unroll(enable) pragma. This metadata is used
// for both "#pragma unroll" and "#pragma clang loop unroll(enable)" directives.
static bool hasUnrollEnablePragma(const Loop *L) {
  return getUnrollMetadataForLoop(L, "llvm.loop.unroll.enable");
}

// Returns true if the loop has an runtime unroll(disable) pragma.
static bool hasRuntimeUnrollDisablePragma(const Loop *L) {
  return getUnrollMetadataForLoop(L, "llvm.loop.unroll.runtime.disable");
}

// If loop has an unroll_count pragma return the (necessarily
// positive) value from the pragma.  Otherwise return 0.
static unsigned unrollCountPragmaValue(const Loop *L) {
  MDNode *MD = getUnrollMetadataForLoop(L, "llvm.loop.unroll.count");
  if (MD) {
    assert(MD->getNumOperands() == 2 &&
           "Unroll count hint metadata should have two operands.");
    unsigned Count =
        mdconst::extract<ConstantInt>(MD->getOperand(1))->getZExtValue();
    assert(Count >= 1 && "Unroll count must be positive.");
    return Count;
  }
  return 0;
}

// Computes the boosting factor for complete unrolling.
// If fully unrolling the loop would save a lot of RolledDynamicCost, it would
// be beneficial to fully unroll the loop even if unrolledcost is large. We
// use (RolledDynamicCost / UnrolledCost) to model the unroll benefits to adjust
// the unroll threshold.
static unsigned getFullUnrollBoostingFactor(const EstimatedUnrollCost &Cost,
                                            unsigned MaxPercentThresholdBoost) {
  if (Cost.RolledDynamicCost >= std::numeric_limits<unsigned>::max() / 100)
    return 100;
  else if (Cost.UnrolledCost != 0)
    // The boosting factor is RolledDynamicCost / UnrolledCost
    return std::min(100 * Cost.RolledDynamicCost / Cost.UnrolledCost,
                    MaxPercentThresholdBoost);
  else
    return MaxPercentThresholdBoost;
}

static std::optional<unsigned>
shouldPragmaUnroll(Loop *L, const PragmaInfo &PInfo,
                   const unsigned TripMultiple, const unsigned TripCount,
                   unsigned MaxTripCount, const UnrollCostEstimator UCE,
                   const TargetTransformInfo::UnrollingPreferences &UP) {

  // Using unroll pragma
  // 1st priority is unroll count set by "unroll-count" option.

  if (PInfo.UserUnrollCount) {
    if (UP.AllowRemainder &&
        UCE.getUnrolledLoopSize(UP, (unsigned)UnrollCount) < UP.Threshold)
      return (unsigned)UnrollCount;
  }

  // 2nd priority is unroll count set by pragma.
  if (PInfo.PragmaCount > 0) {
    if ((UP.AllowRemainder || (TripMultiple % PInfo.PragmaCount == 0)))
      return PInfo.PragmaCount;
  }

  if (PInfo.PragmaFullUnroll && TripCount != 0) {
    // Certain cases with UBSAN can cause trip count to be calculated as
    // INT_MAX, Block full unrolling at a reasonable limit so that the compiler
    // doesn't hang trying to unroll the loop. See PR77842
    if (TripCount > PragmaUnrollFullMaxIterations) {
      LLVM_DEBUG(dbgs() << "Won't unroll; trip count is too large\n");
      return std::nullopt;
    }

    return TripCount;
  }

  if (PInfo.PragmaEnableUnroll && !TripCount && MaxTripCount &&
      MaxTripCount <= UP.MaxUpperBound)
    return MaxTripCount;

  // if didn't return until here, should continue to other priorties
  return std::nullopt;
}

static std::optional<unsigned> shouldFullUnroll(
    Loop *L, const TargetTransformInfo &TTI, DominatorTree &DT,
    ScalarEvolution &SE, const SmallPtrSetImpl<const Value *> &EphValues,
    const unsigned FullUnrollTripCount, const UnrollCostEstimator UCE,
    const TargetTransformInfo::UnrollingPreferences &UP) {
  assert(FullUnrollTripCount && "should be non-zero!");

  if (FullUnrollTripCount > UP.FullUnrollMaxCount)
    return std::nullopt;

  // When computing the unrolled size, note that BEInsns are not replicated
  // like the rest of the loop body.
  if (UCE.getUnrolledLoopSize(UP) < UP.Threshold)
    return FullUnrollTripCount;

  // The loop isn't that small, but we still can fully unroll it if that
  // helps to remove a significant number of instructions.
  // To check that, run additional analysis on the loop.
  if (std::optional<EstimatedUnrollCost> Cost = analyzeLoopUnrollCost(
          L, FullUnrollTripCount, DT, SE, EphValues, TTI,
          UP.Threshold * UP.MaxPercentThresholdBoost / 100,
          UP.MaxIterationsCountToAnalyze)) {
    unsigned Boost =
      getFullUnrollBoostingFactor(*Cost, UP.MaxPercentThresholdBoost);
    if (Cost->UnrolledCost < UP.Threshold * Boost / 100)
      return FullUnrollTripCount;
  }
  return std::nullopt;
}

static std::optional<unsigned>
shouldPartialUnroll(const unsigned LoopSize, const unsigned TripCount,
                    const UnrollCostEstimator UCE,
                    const TargetTransformInfo::UnrollingPreferences &UP) {

  if (!TripCount)
    return std::nullopt;

  if (!UP.Partial) {
    LLVM_DEBUG(dbgs() << "  will not try to unroll partially because "
               << "-unroll-allow-partial not given\n");
    return 0;
  }
  unsigned count = UP.Count;
  if (count == 0)
    count = TripCount;
  if (UP.PartialThreshold != NoThreshold) {
    // Reduce unroll count to be modulo of TripCount for partial unrolling.
    if (UCE.getUnrolledLoopSize(UP, count) > UP.PartialThreshold)
      count = (std::max(UP.PartialThreshold, UP.BEInsns + 1) - UP.BEInsns) /
        (LoopSize - UP.BEInsns);
    if (count > UP.MaxCount)
      count = UP.MaxCount;
    while (count != 0 && TripCount % count != 0)
      count--;
    if (UP.AllowRemainder && count <= 1) {
      // If there is no Count that is modulo of TripCount, set Count to
      // largest power-of-two factor that satisfies the threshold limit.
      // As we'll create fixup loop, do the type of unrolling only if
      // remainder loop is allowed.
      count = UP.DefaultUnrollRuntimeCount;
      while (count != 0 &&
             UCE.getUnrolledLoopSize(UP, count) > UP.PartialThreshold)
        count >>= 1;
    }
    if (count < 2) {
      count = 0;
    }
  } else {
    count = TripCount;
  }
  if (count > UP.MaxCount)
    count = UP.MaxCount;

  LLVM_DEBUG(dbgs() << "  partially unrolling with count: " << count << "\n");

  return count;
}
// Returns true if unroll count was set explicitly.
// Calculates unroll count and writes it to UP.Count.
// Unless IgnoreUser is true, will also use metadata and command-line options
// that are specific to to the LoopUnroll pass (which, for instance, are
// irrelevant for the LoopUnrollAndJam pass).
// FIXME: This function is used by LoopUnroll and LoopUnrollAndJam, but consumes
// many LoopUnroll-specific options. The shared functionality should be
// refactored into it own function.
bool llvm::computeUnrollCount(
    Loop *L, const TargetTransformInfo &TTI, DominatorTree &DT, LoopInfo *LI,
    AssumptionCache *AC, ScalarEvolution &SE,
    const SmallPtrSetImpl<const Value *> &EphValues,
    OptimizationRemarkEmitter *ORE, unsigned TripCount, unsigned MaxTripCount,
    bool MaxOrZero, unsigned TripMultiple, const UnrollCostEstimator &UCE,
    TargetTransformInfo::UnrollingPreferences &UP,
    TargetTransformInfo::PeelingPreferences &PP, bool &UseUpperBound) {

  unsigned LoopSize = UCE.getRolledLoopSize();

  const bool UserUnrollCount = UnrollCount.getNumOccurrences() > 0;
  const bool PragmaFullUnroll = hasUnrollFullPragma(L);
  const unsigned PragmaCount = unrollCountPragmaValue(L);
  const bool PragmaEnableUnroll = hasUnrollEnablePragma(L);

  const bool ExplicitUnroll = PragmaCount > 0 || PragmaFullUnroll ||
                              PragmaEnableUnroll || UserUnrollCount;

  PragmaInfo PInfo(UserUnrollCount, PragmaFullUnroll, PragmaCount,
                   PragmaEnableUnroll);
  // Use an explicit peel count that has been specified for testing. In this
  // case it's not permitted to also specify an explicit unroll count.
  if (PP.PeelCount) {
    if (UnrollCount.getNumOccurrences() > 0) {
      reportFatalUsageError("Cannot specify both explicit peel count and "
                            "explicit unroll count");
    }
    UP.Count = 1;
    UP.Runtime = false;
    return true;
  }
  // Check for explicit Count.
  // 1st priority is unroll count set by "unroll-count" option.
  // 2nd priority is unroll count set by pragma.
  if (auto UnrollFactor = shouldPragmaUnroll(L, PInfo, TripMultiple, TripCount,
                                             MaxTripCount, UCE, UP)) {
    UP.Count = *UnrollFactor;

    if (UserUnrollCount || (PragmaCount > 0)) {
      UP.AllowExpensiveTripCount = true;
      UP.Force = true;
    }
    UP.Runtime |= (PragmaCount > 0);
    return ExplicitUnroll;
  } else {
    if (ExplicitUnroll && TripCount != 0) {
      // If the loop has an unrolling pragma, we want to be more aggressive with
      // unrolling limits. Set thresholds to at least the PragmaUnrollThreshold
      // value which is larger than the default limits.
      UP.Threshold = std::max<unsigned>(UP.Threshold, PragmaUnrollThreshold);
      UP.PartialThreshold =
          std::max<unsigned>(UP.PartialThreshold, PragmaUnrollThreshold);
    }
  }

  // 3rd priority is exact full unrolling.  This will eliminate all copies
  // of some exit test.
  UP.Count = 0;
  if (TripCount) {
    UP.Count = TripCount;
    if (auto UnrollFactor = shouldFullUnroll(L, TTI, DT, SE, EphValues,
                                             TripCount, UCE, UP)) {
      UP.Count = *UnrollFactor;
      UseUpperBound = false;
      return ExplicitUnroll;
    }
  }

  // 4th priority is bounded unrolling.
  // We can unroll by the upper bound amount if it's generally allowed or if
  // we know that the loop is executed either the upper bound or zero times.
  // (MaxOrZero unrolling keeps only the first loop test, so the number of
  // loop tests remains the same compared to the non-unrolled version, whereas
  // the generic upper bound unrolling keeps all but the last loop test so the
  // number of loop tests goes up which may end up being worse on targets with
  // constrained branch predictor resources so is controlled by an option.)
  // In addition we only unroll small upper bounds.
  // Note that the cost of bounded unrolling is always strictly greater than
  // cost of exact full unrolling.  As such, if we have an exact count and
  // found it unprofitable, we'll never chose to bounded unroll.
  if (!TripCount && MaxTripCount && (UP.UpperBound || MaxOrZero) &&
      MaxTripCount <= UP.MaxUpperBound) {
    UP.Count = MaxTripCount;
    if (auto UnrollFactor = shouldFullUnroll(L, TTI, DT, SE, EphValues,
                                             MaxTripCount, UCE, UP)) {
      UP.Count = *UnrollFactor;
      UseUpperBound = true;
      return ExplicitUnroll;
    }
  }

  // 5th priority is loop peeling.
  computePeelCount(L, LoopSize, PP, TripCount, DT, SE, TTI, AC, UP.Threshold);
  if (PP.PeelCount) {
    UP.Runtime = false;
    UP.Count = 1;
    return ExplicitUnroll;
  }

  // Before starting partial unrolling, set up.partial to true,
  // if user explicitly asked  for unrolling
  if (TripCount)
    UP.Partial |= ExplicitUnroll;

  // 6th priority is partial unrolling.
  // Try partial unroll only when TripCount could be statically calculated.
  if (auto UnrollFactor = shouldPartialUnroll(LoopSize, TripCount, UCE, UP)) {
    UP.Count = *UnrollFactor;

    if ((PragmaFullUnroll || PragmaEnableUnroll) && TripCount &&
        UP.Count != TripCount)
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE,
                                        "FullUnrollAsDirectedTooLarge",
                                        L->getStartLoc(), L->getHeader())
               << "Unable to fully unroll loop as directed by unroll pragma "
                  "because "
                  "unrolled size is too large.";
      });

    if (UP.PartialThreshold != NoThreshold) {
      if (UP.Count == 0) {
        if (PragmaEnableUnroll)
          ORE->emit([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE,
                                            "UnrollAsDirectedTooLarge",
                                            L->getStartLoc(), L->getHeader())
                   << "Unable to unroll loop as directed by unroll(enable) "
                      "pragma "
                      "because unrolled size is too large.";
          });
      }
    }
    return ExplicitUnroll;
  }
  assert(TripCount == 0 &&
         "All cases when TripCount is constant should be covered here.");
  if (PragmaFullUnroll)
    ORE->emit([&]() {
      return OptimizationRemarkMissed(
                 DEBUG_TYPE, "CantFullUnrollAsDirectedRuntimeTripCount",
                 L->getStartLoc(), L->getHeader())
             << "Unable to fully unroll loop as directed by unroll(full) "
                "pragma "
                "because loop has a runtime trip count.";
    });

  // 7th priority is runtime unrolling.
  // Don't unroll a runtime trip count loop when it is disabled.
  if (hasRuntimeUnrollDisablePragma(L)) {
    UP.Count = 0;
    return false;
  }

  // Don't unroll a small upper bound loop unless user or TTI asked to do so.
  if (MaxTripCount && !UP.Force && MaxTripCount < UP.MaxUpperBound) {
    UP.Count = 0;
    return false;
  }

  // Check if the runtime trip count is too small when profile is available.
  if (L->getHeader()->getParent()->hasProfileData()) {
    if (auto ProfileTripCount = getLoopEstimatedTripCount(L)) {
      if (*ProfileTripCount < FlatLoopTripCountThreshold)
        return false;
      else
        UP.AllowExpensiveTripCount = true;
    }
  }
  UP.Runtime |= PragmaEnableUnroll || PragmaCount > 0 || UserUnrollCount;
  if (!UP.Runtime) {
    LLVM_DEBUG(
        dbgs() << "  will not try to unroll loop with runtime trip count "
               << "-unroll-runtime not given\n");
    UP.Count = 0;
    return false;
  }
  if (UP.Count == 0)
    UP.Count = UP.DefaultUnrollRuntimeCount;

  // Reduce unroll count to be the largest power-of-two factor of
  // the original count which satisfies the threshold limit.
  while (UP.Count != 0 &&
         UCE.getUnrolledLoopSize(UP) > UP.PartialThreshold)
    UP.Count >>= 1;

#ifndef NDEBUG
  unsigned OrigCount = UP.Count;
#endif

  if (!UP.AllowRemainder && UP.Count != 0 && (TripMultiple % UP.Count) != 0) {
    while (UP.Count != 0 && TripMultiple % UP.Count != 0)
      UP.Count >>= 1;
    LLVM_DEBUG(
        dbgs() << "Remainder loop is restricted (that could architecture "
                  "specific or because the loop contains a convergent "
                  "instruction), so unroll count must divide the trip "
                  "multiple, "
               << TripMultiple << ".  Reducing unroll count from " << OrigCount
               << " to " << UP.Count << ".\n");

    using namespace ore;

    if (unrollCountPragmaValue(L) > 0 && !UP.AllowRemainder)
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE,
                                        "DifferentUnrollCountFromDirected",
                                        L->getStartLoc(), L->getHeader())
               << "Unable to unroll loop the number of times directed by "
                  "unroll_count pragma because remainder loop is restricted "
                  "(that could architecture specific or because the loop "
                  "contains a convergent instruction) and so must have an "
                  "unroll "
                  "count that divides the loop trip multiple of "
               << NV("TripMultiple", TripMultiple) << ".  Unrolling instead "
               << NV("UnrollCount", UP.Count) << " time(s).";
      });
  }

  if (UP.Count > UP.MaxCount)
    UP.Count = UP.MaxCount;

  if (MaxTripCount && UP.Count > MaxTripCount)
    UP.Count = MaxTripCount;

  LLVM_DEBUG(dbgs() << "  runtime unrolling with count: " << UP.Count
                    << "\n");
  if (UP.Count < 2)
    UP.Count = 0;
  return ExplicitUnroll;
}

// Returns true if the loop has an unroll(disable) pragma.
static bool hasUnrollDisablePragma(const Loop *L) {
  return getUnrollMetadataForLoop(L, "llvm.loop.unroll.disable");
}

// Returns the unroll_count pragma value, searching all blocks in the loop
// (not just the latch's loop ID). This handles the case where the frontend
// already partially unrolled the loop and placed unroll.count on the header
// while putting unroll.disable on the latch.
static unsigned unrollCountPragmaValueAllBlocks(const Loop *L) {
  unsigned Count = unrollCountPragmaValue(L);
  if (Count > 0)
    return Count;

  // Search all loop blocks for the metadata
  for (BasicBlock *BB : L->getBlocks()) {
    auto *BI = dyn_cast<BranchInst>(BB->getTerminator());
    if (!BI)
      continue;
    MDNode *LoopMD = BI->getMetadata(LLVMContext::MD_loop);
    if (!LoopMD)
      continue;
    MDNode *FoundMD = GetUnrollMetadata(LoopMD, "llvm.loop.unroll.count");
    if (!FoundMD || FoundMD->getNumOperands() != 2)
      continue;
    unsigned C =
        mdconst::extract<ConstantInt>(FoundMD->getOperand(1))->getZExtValue();
    if (C > 0)
      return C;
  }
  return 0;
}

// Fix up a loop that was already partially unrolled by the frontend: just
// make the last latch unconditionally branch to the exit so the loop is
// fully unrolled with a forced exit after the last copy.
static LoopUnrollResult
fixupAlreadyUnrolledLoop(Loop *L, LoopInfo *LI, DominatorTree &DT,
                         ScalarEvolution &SE, AssumptionCache &AC,
                         const TargetTransformInfo &TTI,
                         OptimizationRemarkEmitter &ORE,
                         bool PreserveLCSSA) {
  BasicBlock *Header = L->getHeader();
  BasicBlock *LatchBlock = L->getLoopLatch();
  BasicBlock *Preheader = L->getLoopPreheader();
  if (!LatchBlock || !Preheader)
    return LoopUnrollResult::Unmodified;

  auto *LatchBI = dyn_cast<BranchInst>(LatchBlock->getTerminator());
  if (!LatchBI)
    return LoopUnrollResult::Unmodified;

  SmallVector<BasicBlock *, 4> ExitBlocks;
  L->getExitBlocks(ExitBlocks);
  if (ExitBlocks.empty())
    return LoopUnrollResult::Unmodified;

  LLVM_DEBUG(dbgs() << "  Fixing up already-unrolled loop: forcing latch "
                    << LatchBlock->getName() << " to exit\n");
  ORE.emit([&]() {
    return OptimizationRemark(DEBUG_TYPE, "ForceUnrolled", L->getStartLoc(),
                              L->getHeader())
           << "force-exit already-unrolled loop (pragma unroll_count)";
  });

  // Find the exit successor for the latch
  BasicBlock *LatchExit = nullptr;
  for (BasicBlock *Succ : successors(LatchBlock)) {
    if (!L->contains(Succ)) {
      LatchExit = Succ;
      break;
    }
  }
  // For an unconditional latch, find the exit through the header
  if (!LatchExit) {
    // The latch goes back to the header; use the first loop exit block
    LatchExit = ExitBlocks[0];
  }

  // Remove the old predecessor relationship and redirect to exit
  if (LatchBI->isUnconditional()) {
    BasicBlock *OldSucc = LatchBI->getSuccessor(0);
    OldSucc->removePredecessor(LatchBlock, /* KeepOneInputPHIs */ true);
    LatchBI->setSuccessor(0, LatchExit);
  } else {
    // Conditional latch: replace with unconditional branch to exit
    for (unsigned s = 0; s < LatchBI->getNumSuccessors(); ++s) {
      BasicBlock *Succ = LatchBI->getSuccessor(s);
      if (Succ != LatchExit)
        Succ->removePredecessor(LatchBlock, /* KeepOneInputPHIs */ true);
    }
    auto *NewBI = BranchInst::Create(LatchExit, LatchBI->getIterator());
    NewBI->setDebugLoc(LatchBI->getDebugLoc());
    LatchBI->eraseFromParent();
  }

  // Fix original header PHIs: replace with preheader values.
  // Collect PHIs first to avoid invalidating the iterator during erasure.
  SmallVector<PHINode *, 4> PhisToRemove;
  for (Instruction &I : *Header) {
    auto *Phi = dyn_cast<PHINode>(&I);
    if (!Phi)
      break;
    PhisToRemove.push_back(Phi);
  }
  for (PHINode *Phi : PhisToRemove) {
    Value *InitVal = Phi->getIncomingValueForBlock(Preheader);
    Phi->replaceAllUsesWith(InitVal);
    Phi->eraseFromParent();
  }

  // Update DT, SCEV, and simplify
  DT.recalculate(*Header->getParent());
  SE.forgetTopmostLoop(L);

  // Promote eligible allocas to registers so downstream passes (e.g.,
  // TranslateToStp) can correctly handle path-dependent values with
  // Select instructions instead of accumulating flat UpdateList entries.
  SmallVector<AllocaInst *, 8> Allocas;
  for (BasicBlock &BB : *Header->getParent())
    for (Instruction &I : BB)
      if (auto *AI = dyn_cast<AllocaInst>(&I))
        if (isAllocaPromotable(AI))
          Allocas.push_back(AI);
  if (!Allocas.empty())
    PromoteMemToReg(Allocas, DT, &AC);

  simplifyLoopAfterUnroll(L, false, LI, &SE, &DT, &AC, &TTI, nullptr);
  LI->erase(L);

  return LoopUnrollResult::FullyUnrolled;
}
// Clones the loop body N-1 times in RPO order (following the same cloning
// approach as UnrollLoop), chains the iterations, and makes the last copy
// exit unconditionally so the loop is fully unrolled even when the trip
// count is not known at compile time.
static LoopUnrollResult
forceUnrollByPragma(Loop *L, LoopInfo *LI, DominatorTree &DT,
                    ScalarEvolution &SE, AssumptionCache &AC,
                    const TargetTransformInfo &TTI,
                    OptimizationRemarkEmitter &ORE,
                    bool PreserveLCSSA, unsigned PragmaCount) {
  BasicBlock *Header = L->getHeader();
  BasicBlock *LatchBlock = L->getLoopLatch();
  BasicBlock *Preheader = L->getLoopPreheader();
  if (!LatchBlock || !Preheader)
    return LoopUnrollResult::Unmodified;

  // Validate that the latch has a branch terminator
  auto *LatchBI = dyn_cast<BranchInst>(LatchBlock->getTerminator());
  if (!LatchBI)
    return LoopUnrollResult::Unmodified;

  SmallVector<BasicBlock *, 4> ExitBlocks;
  L->getExitBlocks(ExitBlocks);
  if (ExitBlocks.empty())
    return LoopUnrollResult::Unmodified;

  LLVM_DEBUG(dbgs() << "  Force-unrolling loop by " << PragmaCount
                    << " (pragma)\n");
  ORE.emit([&]() {
    return OptimizationRemark(DEBUG_TYPE, "ForceUnrolled", L->getStartLoc(),
                              L->getHeader())
           << "force-unrolled loop by " << ore::NV("UnrollCount", PragmaCount)
           << " iterations (pragma unroll_count)";
  });

  // Collect original PHI nodes from header
  std::vector<PHINode *> OrigPHINode;
  for (Instruction &I : *Header) {
    if (auto *Phi = dyn_cast<PHINode>(&I))
      OrigPHINode.push_back(Phi);
    else
      break;
  }

  // RPO traversal for correct cloning order (same approach as UnrollLoop)
  LoopBlocksDFS DFS(L);
  DFS.perform(LI);

  std::vector<BasicBlock *> Headers = {Header};
  std::vector<BasicBlock *> Latches = {LatchBlock};

  ValueToValueMapTy LastValueMap;

  // Insert cloned blocks after the original latch
  auto BlockInsertPt = std::next(LatchBlock->getIterator());

  for (unsigned It = 1; It < PragmaCount; ++It) {
    SmallVector<BasicBlock *, 8> NewBlocks;
    SmallDenseMap<const Loop *, Loop *, 4> NewLoops;
    NewLoops[L] = L;

    // Clone blocks in RPO order
    for (LoopBlocksDFS::RPOIterator BB = DFS.beginRPO(); BB != DFS.endRPO();
         ++BB) {
      ValueToValueMapTy VMap;
      BasicBlock *New =
          CloneBasicBlock(*BB, VMap, "." + Twine(It));
      Header->getParent()->insert(BlockInsertPt, New);

      // Tell LoopInfo about the new block
      addClonedBlockToLoopInfo(*BB, New, LI, NewLoops);

      if (*BB == Header) {
        // Fix cloned header PHI nodes: replace them with incoming values from
        // the previous iteration's latch.
        for (PHINode *OrigPHI : OrigPHINode) {
          PHINode *NewPHI = cast<PHINode>(VMap[OrigPHI]);
          Value *InVal = NewPHI->getIncomingValueForBlock(LatchBlock);
          if (Instruction *InValI = dyn_cast<Instruction>(InVal))
            if (It > 1 && L->contains(InValI))
              InVal = LastValueMap[InValI];
          VMap[OrigPHI] = InVal;
          NewPHI->eraseFromParent();
        }
      }

      // Update LastValueMap: track the newest clone of each value
      LastValueMap[*BB] = New;
      for (ValueToValueMapTy::iterator VI = VMap.begin(), VE = VMap.end();
           VI != VE; ++VI)
        LastValueMap[VI->first] = VI->second;

      // Add PHI entries for cloned values to exit blocks
      for (BasicBlock *Succ : successors(*BB)) {
        if (L->contains(Succ))
          continue;
        for (PHINode &PHI : Succ->phis()) {
          Value *Incoming = PHI.getIncomingValueForBlock(*BB);
          auto It = LastValueMap.find(Incoming);
          if (It != LastValueMap.end())
            Incoming = It->second;
          PHI.addIncoming(Incoming, New);
          SE.forgetLcssaPhiWithNewPredecessor(L, &PHI);
        }
      }

      // Track headers and latches for later rewiring
      if (*BB == Header)
        Headers.push_back(New);
      if (*BB == LatchBlock)
        Latches.push_back(New);

      NewBlocks.push_back(New);

      // Update DomTree: the cloned header dominates all cloned blocks;
      // for other blocks, use the cloned immediate dominator.
      if (*BB == Header)
        DT.addNewBlock(New, Latches[It - 1]);
      else {
        auto *BBDomNode = DT.getNode(*BB);
        auto *BBIDom = BBDomNode->getIDom();
        BasicBlock *OriginalBBIDom = BBIDom->getBlock();
        DT.addNewBlock(
            New, cast<BasicBlock>(LastValueMap[cast<Value>(OriginalBBIDom)]));
      }
    }

    // Remap all instructions in the cloned blocks to use the newest values
    remapInstructionsInBlocks(NewBlocks, LastValueMap);
    for (BasicBlock *NewBlock : NewBlocks)
      for (Instruction &I : *NewBlock)
        if (auto *II = dyn_cast<AssumeInst>(&I))
          AC.registerAssumption(II);
  }

  // Rewire latches:
  // - Iterations 0..PragmaCount-2: latch[i] → header[i+1]
  // - Last iteration: latch[last] → exit (unconditional, forced exit)
  //
  // In loop-simplified form the latch is typically unconditional (br label
  // %header), but it can also be a conditional exiting latch. Handle both.
  for (unsigned i = 0; i < Latches.size(); ++i) {
    BasicBlock *CurLatch = Latches[i];
    auto *BI = cast<BranchInst>(CurLatch->getTerminator());

    if (i + 1 < PragmaCount) {
      // Not the last iteration: rewire the successor that points back to
      // this iteration's header to point to the next iteration's header.
      BI->replaceSuccessorWith(Headers[i], Headers[i + 1]);
    } else {
      // Last iteration: force the latch to unconditionally branch to the
      // first exit block of the original loop.
      SmallVector<BasicBlock *, 4> LoopExits;
      L->getExitBlocks(LoopExits);
      BasicBlock *TargetExit = LoopExits.empty() ? nullptr : LoopExits[0];
      if (!TargetExit)
        return LoopUnrollResult::Unmodified;

      if (BI->isUnconditional()) {
        // Unconditional latch: remove the old predecessor relationship
        // before redirecting to the exit.
        BasicBlock *OldSucc = BI->getSuccessor(0);
        OldSucc->removePredecessor(CurLatch, /* KeepOneInputPHIs */ true);
        BI->setSuccessor(0, TargetExit);
      } else {
        // Conditional latch: find which successor is the exit for this clone,
        // remove predecessors from the non-exit successors, and replace with
        // an unconditional branch to the exit.
        BasicBlock *ExitForThisLatch = nullptr;
        for (unsigned s = 0; s < BI->getNumSuccessors(); ++s) {
          BasicBlock *Succ = BI->getSuccessor(s);
          if (Succ != Headers[i]) {
            ExitForThisLatch = Succ;
            break;
          }
        }
        if (!ExitForThisLatch)
          ExitForThisLatch = TargetExit;

        for (unsigned s = 0; s < BI->getNumSuccessors(); ++s) {
          BasicBlock *Succ = BI->getSuccessor(s);
          if (Succ != ExitForThisLatch)
            Succ->removePredecessor(CurLatch, /* KeepOneInputPHIs */ true);
        }

        auto *NewBI = BranchInst::Create(ExitForThisLatch, BI->getIterator());
        NewBI->setDebugLoc(BI->getDebugLoc());
        BI->eraseFromParent();
      }
    }
  }

  // Fix original header PHIs: since the loop is fully unrolled, replace them
  // with their preheader incoming values.  OrigPHINode was collected before
  // any modifications, so it is safe to iterate and erase.
  for (PHINode *PN : OrigPHINode) {
    Value *InitVal = PN->getIncomingValueForBlock(Preheader);
    PN->replaceAllUsesWith(InitVal);
    PN->eraseFromParent();
  }

  // Update dominator tree: exit blocks may now be reached through different
  // paths.  Recalculate the DT for the whole function since the CFG changed
  // substantially (back-edges removed).
  DT.recalculate(*Header->getParent());

  // Forget SCEV for the unrolled loop (must be done before erasing L from LI
  // because forgetTopmostLoop traverses up through parent loops).
  SE.forgetTopmostLoop(L);

  // Promote eligible allocas to registers so downstream passes (e.g.,
  // TranslateToStp) can correctly handle path-dependent values with
  // Select instructions instead of accumulating flat UpdateList entries.
  SmallVector<AllocaInst *, 8> Allocas;
  for (BasicBlock &BB : *Header->getParent())
    for (Instruction &I : BB)
      if (auto *AI = dyn_cast<AllocaInst>(&I))
        if (isAllocaPromotable(AI))
          Allocas.push_back(AI);
  if (!Allocas.empty())
    PromoteMemToReg(Allocas, DT, &AC);

  // Simplify the unrolled code: constant propagation and DCE on the blocks
  // that were part of the loop (must be done before erasing L).
  simplifyLoopAfterUnroll(L, false, LI, &SE, &DT, &AC, &TTI, nullptr);

  // Clean up LoopInfo (must be last — L must stay valid for the calls above).
  LI->erase(L);

  return LoopUnrollResult::FullyUnrolled;
}

static LoopUnrollResult
tryToUnrollLoop(Loop *L, DominatorTree &DT, LoopInfo *LI, ScalarEvolution &SE,
                const TargetTransformInfo &TTI, AssumptionCache &AC,
                OptimizationRemarkEmitter &ORE, BlockFrequencyInfo *BFI,
                ProfileSummaryInfo *PSI, bool PreserveLCSSA, int OptLevel,
                bool OnlyFullUnroll, bool OnlyWhenForced, bool ForgetAllSCEV,
                std::optional<unsigned> ProvidedCount,
                std::optional<unsigned> ProvidedThreshold,
                std::optional<bool> ProvidedAllowPartial,
                std::optional<bool> ProvidedRuntime,
                std::optional<bool> ProvidedUpperBound,
                std::optional<bool> ProvidedAllowPeeling,
                std::optional<bool> ProvidedAllowProfileBasedPeeling,
                std::optional<unsigned> ProvidedFullUnrollMaxCount,
                AAResults *AA = nullptr) {

  LLVM_DEBUG(dbgs() << "Loop Unroll: F["
                    << L->getHeader()->getParent()->getName() << "] Loop %"
                    << L->getHeader()->getName() << "\n");
  TransformationMode TM = hasUnrollTransformation(L);
  // When #pragma clang loop unroll_count(N) is present on any loop block,
  // we may still want to force-unroll even if another block has unroll.disable
  // (which happens when the frontend already partially unrolled the loop).
  bool HasPragmaCountAnywhere = unrollCountPragmaValueAllBlocks(L) > 0;
  if ((TM & TM_Disable) && !HasPragmaCountAnywhere)
    return LoopUnrollResult::Unmodified;

  // If this loop isn't forced to be unrolled, avoid unrolling it when the
  // parent loop has an explicit unroll-and-jam pragma. This is to prevent
  // automatic unrolling from interfering with the user requested
  // transformation.
  Loop *ParentL = L->getParentLoop();
  if (ParentL != nullptr &&
      hasUnrollAndJamTransformation(ParentL) == TM_ForcedByUser &&
      hasUnrollTransformation(L) != TM_ForcedByUser) {
    LLVM_DEBUG(dbgs() << "Not unrolling loop since parent loop has"
                      << " llvm.loop.unroll_and_jam.\n");
    return LoopUnrollResult::Unmodified;
  }

  // If this loop isn't forced to be unrolled, avoid unrolling it when the
  // loop has an explicit unroll-and-jam pragma. This is to prevent automatic
  // unrolling from interfering with the user requested transformation.
  if (hasUnrollAndJamTransformation(L) == TM_ForcedByUser &&
      hasUnrollTransformation(L) != TM_ForcedByUser) {
    LLVM_DEBUG(
        dbgs()
        << "  Not unrolling loop since it has llvm.loop.unroll_and_jam.\n");
    return LoopUnrollResult::Unmodified;
  }

  if (!L->isLoopSimplifyForm()) {
    LLVM_DEBUG(
        dbgs() << "  Not unrolling loop which is not in loop-simplify form.\n");
    return LoopUnrollResult::Unmodified;
  }

  // When automatic unrolling is disabled, do not unroll unless overridden for
  // this loop.
  if (OnlyWhenForced && !(TM & TM_Enable))
    return LoopUnrollResult::Unmodified;

  bool OptForSize = L->getHeader()->getParent()->hasOptSize();
  TargetTransformInfo::UnrollingPreferences UP = gatherUnrollingPreferences(
      L, SE, TTI, BFI, PSI, ORE, OptLevel, ProvidedThreshold, ProvidedCount,
      ProvidedAllowPartial, ProvidedRuntime, ProvidedUpperBound,
      ProvidedFullUnrollMaxCount);
  TargetTransformInfo::PeelingPreferences PP = gatherPeelingPreferences(
      L, SE, TTI, ProvidedAllowPeeling, ProvidedAllowProfileBasedPeeling, true);

  // Exit early if unrolling is disabled. For OptForSize, we pick the loop size
  // as threshold later on.
  if (UP.Threshold == 0 && (!UP.Partial || UP.PartialThreshold == 0) &&
      !OptForSize)
    return LoopUnrollResult::Unmodified;

  SmallPtrSet<const Value *, 32> EphValues;
  CodeMetrics::collectEphemeralValues(L, &AC, EphValues);

  UnrollCostEstimator UCE(L, TTI, EphValues, UP.BEInsns);
  if (!UCE.canUnroll()) {
    LLVM_DEBUG(dbgs() << "  Loop not considered unrollable.\n");
    return LoopUnrollResult::Unmodified;
  }

  unsigned LoopSize = UCE.getRolledLoopSize();
  LLVM_DEBUG(dbgs() << "  Loop Size = " << LoopSize << "\n");

  // When optimizing for size, use LoopSize + 1 as threshold (we use < Threshold
  // later), to (fully) unroll loops, if it does not increase code size.
  if (OptForSize)
    UP.Threshold = std::max(UP.Threshold, LoopSize + 1);

  if (UCE.NumInlineCandidates != 0) {
    LLVM_DEBUG(dbgs() << "  Not unrolling loop with inlinable calls.\n");
    return LoopUnrollResult::Unmodified;
  }

  // Find the smallest exact trip count for any exit. This is an upper bound
  // on the loop trip count, but an exit at an earlier iteration is still
  // possible. An unroll by the smallest exact trip count guarantees that all
  // branches relating to at least one exit can be eliminated. This is unlike
  // the max trip count, which only guarantees that the backedge can be broken.
  unsigned TripCount = 0;
  unsigned TripMultiple = 1;
  SmallVector<BasicBlock *, 8> ExitingBlocks;
  L->getExitingBlocks(ExitingBlocks);
  for (BasicBlock *ExitingBlock : ExitingBlocks)
    if (unsigned TC = SE.getSmallConstantTripCount(L, ExitingBlock))
      if (!TripCount || TC < TripCount)
        TripCount = TripMultiple = TC;

  if (!TripCount) {
    // If no exact trip count is known, determine the trip multiple of either
    // the loop latch or the single exiting block.
    // TODO: Relax for multiple exits.
    BasicBlock *ExitingBlock = L->getLoopLatch();
    if (!ExitingBlock || !L->isLoopExiting(ExitingBlock))
      ExitingBlock = L->getExitingBlock();
    if (ExitingBlock)
      TripMultiple = SE.getSmallConstantTripMultiple(L, ExitingBlock);
  }

  // If the loop contains a convergent operation, the prelude we'd add
  // to do the first few instructions before we hit the unrolled loop
  // is unsafe -- it adds a control-flow dependency to the convergent
  // operation.  Therefore restrict remainder loop (try unrolling without).
  //
  // TODO: This is somewhat conservative; we could allow the remainder if the
  // trip count is uniform.
  UP.AllowRemainder &= UCE.ConvergenceAllowsRuntime;

  // Try to find the trip count upper bound if we cannot find the exact trip
  // count.
  unsigned MaxTripCount = 0;
  bool MaxOrZero = false;
  if (!TripCount) {
    MaxTripCount = SE.getSmallConstantMaxTripCount(L);
    MaxOrZero = SE.isBackedgeTakenCountMaxOrZero(L);
  }

  // computeUnrollCount() decides whether it is beneficial to use upper bound to
  // fully unroll the loop.
  bool UseUpperBound = false;
  bool IsCountSetExplicitly = computeUnrollCount(
      L, TTI, DT, LI, &AC, SE, EphValues, &ORE, TripCount, MaxTripCount,
      MaxOrZero, TripMultiple, UCE, UP, PP, UseUpperBound);

  // When the loop was already partially unrolled by the frontend (has both
  // unroll.count on one block and unroll.disable on the latch),
  // computeUnrollCount may set UP.Count=0 because of the disable metadata.
  // Fix up the last latch to exit unconditionally before the early return.
  if (HasPragmaCountAnywhere && !TripCount && !OnlyFullUnroll &&
      ((TM & TM_Disable) || hasUnrollDisablePragma(L))) {
    LoopUnrollResult Result = fixupAlreadyUnrolledLoop(L, LI, DT, SE, AC, TTI,
                                                       ORE, PreserveLCSSA);
    if (Result != LoopUnrollResult::Unmodified)
      return Result;
  }

  if (!UP.Count)
    return LoopUnrollResult::Unmodified;

  UP.Runtime &= UCE.ConvergenceAllowsRuntime;

  if (PP.PeelCount) {
    assert(UP.Count == 1 && "Cannot perform peel and unroll in the same step");
    LLVM_DEBUG(dbgs() << "PEELING loop %" << L->getHeader()->getName()
                      << " with iteration count " << PP.PeelCount << "!\n");
    ORE.emit([&]() {
      return OptimizationRemark(DEBUG_TYPE, "Peeled", L->getStartLoc(),
                                L->getHeader())
             << " peeled loop by " << ore::NV("PeelCount", PP.PeelCount)
             << " iterations";
    });

    ValueToValueMapTy VMap;
    if (peelLoop(L, PP.PeelCount, PP.PeelLast, LI, &SE, DT, &AC, PreserveLCSSA,
                 VMap)) {
      simplifyLoopAfterUnroll(L, true, LI, &SE, &DT, &AC, &TTI, nullptr);
      // If the loop was peeled, we already "used up" the profile information
      // we had, so we don't want to unroll or peel again.
      if (PP.PeelProfiledIterations)
        L->setLoopAlreadyUnrolled();
      return LoopUnrollResult::PartiallyUnrolled;
    }
    return LoopUnrollResult::Unmodified;
  }

  // Do not attempt partial/runtime unrolling in FullLoopUnrolling
  if (OnlyFullUnroll && (UP.Count < TripCount || UP.Count < MaxTripCount)) {
    LLVM_DEBUG(
        dbgs() << "Not attempting partial/runtime unroll in FullLoopUnroll.\n");
    return LoopUnrollResult::Unmodified;
  }

  // At this point, UP.Runtime indicates that run-time unrolling is allowed.
  // However, we only want to actually perform it if we don't know the trip
  // count and the unroll count doesn't divide the known trip multiple.
  // TODO: This decision should probably be pushed up into
  // computeUnrollCount().
  UP.Runtime &= TripCount == 0 && TripMultiple % UP.Count != 0;

  // When #pragma clang loop unroll_count(N) is present on a loop with unknown
  // trip count, force-unroll: clone the body N times and make the last copy
  // exit unconditionally instead of going back to check the loop condition.
  // This bypasses the normal runtime-unrolling path which would create a
  // remainder loop.
  //
  // Two cases:
  // 1. Fresh IR: loop has 1 body copy, unroll.count = N.  Clone N-1 more
  //    copies and force the last latch to exit.
  // 2. Already-unrolled IR (frontend did partial unroll): loop already has
  //    unroll.disable on the latch.  Just fix the latch to exit unconditionally.
  unsigned PragmaCount = unrollCountPragmaValueAllBlocks(L);
  if (PragmaCount > 0 && !TripCount && !OnlyFullUnroll) {
    bool AlreadyUnrolled = (TM & TM_Disable) || hasUnrollDisablePragma(L);
    LoopUnrollResult Result;
    if (AlreadyUnrolled) {
      // Frontend already unrolled the body; just force the last latch to exit.
      Result = fixupAlreadyUnrolledLoop(L, LI, DT, SE, AC, TTI, ORE,
                                        PreserveLCSSA);
    } else if (UP.Force) {
      // Fresh loop; clone the body and force exit.
      Result = forceUnrollByPragma(L, LI, DT, SE, AC, TTI, ORE,
                                   PreserveLCSSA, PragmaCount);
    } else {
      Result = LoopUnrollResult::Unmodified;
    }
    if (Result != LoopUnrollResult::Unmodified) {
      // Loop has been erased; nothing more to do.
      return Result;
    }
    // If the force-unroll failed, fall through to normal unrolling.
  }

  // Save loop properties before it is transformed.
  MDNode *OrigLoopID = L->getLoopID();

  // Unroll the loop.
  Loop *RemainderLoop = nullptr;
  UnrollLoopOptions ULO;
  ULO.Count = UP.Count;
  ULO.Force = UP.Force;
  ULO.AllowExpensiveTripCount = UP.AllowExpensiveTripCount;
  ULO.UnrollRemainder = UP.UnrollRemainder;
  ULO.Runtime = UP.Runtime;
  ULO.ForgetAllSCEV = ForgetAllSCEV;
  ULO.Heart = getLoopConvergenceHeart(L);
  ULO.SCEVExpansionBudget = UP.SCEVExpansionBudget;
  ULO.RuntimeUnrollMultiExit = UP.RuntimeUnrollMultiExit;
  LoopUnrollResult UnrollResult = UnrollLoop(
      L, ULO, LI, &SE, &DT, &AC, &TTI, &ORE, PreserveLCSSA, &RemainderLoop, AA);
  if (UnrollResult == LoopUnrollResult::Unmodified)
    return LoopUnrollResult::Unmodified;

  if (RemainderLoop) {
    std::optional<MDNode *> RemainderLoopID =
        makeFollowupLoopID(OrigLoopID, {LLVMLoopUnrollFollowupAll,
                                        LLVMLoopUnrollFollowupRemainder});
    if (RemainderLoopID)
      RemainderLoop->setLoopID(*RemainderLoopID);
  }

  if (UnrollResult != LoopUnrollResult::FullyUnrolled) {
    std::optional<MDNode *> NewLoopID =
        makeFollowupLoopID(OrigLoopID, {LLVMLoopUnrollFollowupAll,
                                        LLVMLoopUnrollFollowupUnrolled});
    if (NewLoopID) {
      L->setLoopID(*NewLoopID);

      // Do not setLoopAlreadyUnrolled if loop attributes have been specified
      // explicitly.
      return UnrollResult;
    }
  }

  // If loop has an unroll count pragma or unrolled by explicitly set count
  // mark loop as unrolled to prevent unrolling beyond that requested.
  if (UnrollResult != LoopUnrollResult::FullyUnrolled && IsCountSetExplicitly)
    L->setLoopAlreadyUnrolled();

  return UnrollResult;
}

namespace {

class LoopUnroll : public LoopPass {
public:
  static char ID; // Pass ID, replacement for typeid

  int OptLevel;

  /// If false, use a cost model to determine whether unrolling of a loop is
  /// profitable. If true, only loops that explicitly request unrolling via
  /// metadata are considered. All other loops are skipped.
  bool OnlyWhenForced;

  /// If false, when SCEV is invalidated, only forget everything in the
  /// top-most loop (call forgetTopMostLoop), of the loop being processed.
  /// Otherwise, forgetAllLoops and rebuild when needed next.
  bool ForgetAllSCEV;

  std::optional<unsigned> ProvidedCount;
  std::optional<unsigned> ProvidedThreshold;
  std::optional<bool> ProvidedAllowPartial;
  std::optional<bool> ProvidedRuntime;
  std::optional<bool> ProvidedUpperBound;
  std::optional<bool> ProvidedAllowPeeling;
  std::optional<bool> ProvidedAllowProfileBasedPeeling;
  std::optional<unsigned> ProvidedFullUnrollMaxCount;

  LoopUnroll(int OptLevel = 2, bool OnlyWhenForced = false,
             bool ForgetAllSCEV = false,
             std::optional<unsigned> Threshold = std::nullopt,
             std::optional<unsigned> Count = std::nullopt,
             std::optional<bool> AllowPartial = std::nullopt,
             std::optional<bool> Runtime = std::nullopt,
             std::optional<bool> UpperBound = std::nullopt,
             std::optional<bool> AllowPeeling = std::nullopt,
             std::optional<bool> AllowProfileBasedPeeling = std::nullopt,
             std::optional<unsigned> ProvidedFullUnrollMaxCount = std::nullopt)
      : LoopPass(ID), OptLevel(OptLevel), OnlyWhenForced(OnlyWhenForced),
        ForgetAllSCEV(ForgetAllSCEV), ProvidedCount(std::move(Count)),
        ProvidedThreshold(Threshold), ProvidedAllowPartial(AllowPartial),
        ProvidedRuntime(Runtime), ProvidedUpperBound(UpperBound),
        ProvidedAllowPeeling(AllowPeeling),
        ProvidedAllowProfileBasedPeeling(AllowProfileBasedPeeling),
        ProvidedFullUnrollMaxCount(ProvidedFullUnrollMaxCount) {
    initializeLoopUnrollPass(*PassRegistry::getPassRegistry());
  }

  bool runOnLoop(Loop *L, LPPassManager &LPM) override {
    if (skipLoop(L))
      return false;

    Function &F = *L->getHeader()->getParent();

    auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    LoopInfo *LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    ScalarEvolution &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();
    const TargetTransformInfo &TTI =
        getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
    auto &AC = getAnalysis<AssumptionCacheTracker>().getAssumptionCache(F);
    // For the old PM, we can't use OptimizationRemarkEmitter as an analysis
    // pass.  Function analyses need to be preserved across loop transformations
    // but ORE cannot be preserved (see comment before the pass definition).
    OptimizationRemarkEmitter ORE(&F);
    bool PreserveLCSSA = mustPreserveAnalysisID(LCSSAID);

    LoopUnrollResult Result = tryToUnrollLoop(
        L, DT, LI, SE, TTI, AC, ORE, nullptr, nullptr, PreserveLCSSA, OptLevel,
        /*OnlyFullUnroll*/ false, OnlyWhenForced, ForgetAllSCEV, ProvidedCount,
        ProvidedThreshold, ProvidedAllowPartial, ProvidedRuntime,
        ProvidedUpperBound, ProvidedAllowPeeling,
        ProvidedAllowProfileBasedPeeling, ProvidedFullUnrollMaxCount);

    if (Result == LoopUnrollResult::FullyUnrolled)
      LPM.markLoopAsDeleted(*L);

    return Result != LoopUnrollResult::Unmodified;
  }

  /// This transformation requires natural loop information & requires that
  /// loop preheaders be inserted into the CFG...
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<AssumptionCacheTracker>();
    AU.addRequired<TargetTransformInfoWrapperPass>();
    // FIXME: Loop passes are required to preserve domtree, and for now we just
    // recreate dom info if anything gets unrolled.
    getLoopAnalysisUsage(AU);
  }
};

} // end anonymous namespace

char LoopUnroll::ID = 0;

INITIALIZE_PASS_BEGIN(LoopUnroll, "loop-unroll", "Unroll loops", false, false)
INITIALIZE_PASS_DEPENDENCY(AssumptionCacheTracker)
INITIALIZE_PASS_DEPENDENCY(LoopPass)
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_END(LoopUnroll, "loop-unroll", "Unroll loops", false, false)

Pass *llvm::createLoopUnrollPass(int OptLevel, bool OnlyWhenForced,
                                 bool ForgetAllSCEV, int Threshold, int Count,
                                 int AllowPartial, int Runtime, int UpperBound,
                                 int AllowPeeling) {
  // TODO: It would make more sense for this function to take the optionals
  // directly, but that's dangerous since it would silently break out of tree
  // callers.
  return new LoopUnroll(
      OptLevel, OnlyWhenForced, ForgetAllSCEV,
      Threshold == -1 ? std::nullopt : std::optional<unsigned>(Threshold),
      Count == -1 ? std::nullopt : std::optional<unsigned>(Count),
      AllowPartial == -1 ? std::nullopt : std::optional<bool>(AllowPartial),
      Runtime == -1 ? std::nullopt : std::optional<bool>(Runtime),
      UpperBound == -1 ? std::nullopt : std::optional<bool>(UpperBound),
      AllowPeeling == -1 ? std::nullopt : std::optional<bool>(AllowPeeling));
}

PreservedAnalyses LoopFullUnrollPass::run(Loop &L, LoopAnalysisManager &AM,
                                          LoopStandardAnalysisResults &AR,
                                          LPMUpdater &Updater) {
  // For the new PM, we can't use OptimizationRemarkEmitter as an analysis
  // pass. Function analyses need to be preserved across loop transformations
  // but ORE cannot be preserved (see comment before the pass definition).
  OptimizationRemarkEmitter ORE(L.getHeader()->getParent());

  // Keep track of the previous loop structure so we can identify new loops
  // created by unrolling.
  Loop *ParentL = L.getParentLoop();
  SmallPtrSet<Loop *, 4> OldLoops;
  if (ParentL)
    OldLoops.insert_range(*ParentL);
  else
    OldLoops.insert_range(AR.LI);

  std::string LoopName = std::string(L.getName());

  bool Changed =
      tryToUnrollLoop(&L, AR.DT, &AR.LI, AR.SE, AR.TTI, AR.AC, ORE,
                      /*BFI*/ nullptr, /*PSI*/ nullptr,
                      /*PreserveLCSSA*/ true, OptLevel, /*OnlyFullUnroll*/ true,
                      OnlyWhenForced, ForgetSCEV, /*Count*/ std::nullopt,
                      /*Threshold*/ std::nullopt, /*AllowPartial*/ false,
                      /*Runtime*/ false, /*UpperBound*/ false,
                      /*AllowPeeling*/ true,
                      /*AllowProfileBasedPeeling*/ false,
                      /*FullUnrollMaxCount*/ std::nullopt) !=
      LoopUnrollResult::Unmodified;
  if (!Changed)
    return PreservedAnalyses::all();

  // The parent must not be damaged by unrolling!
#ifndef NDEBUG
  if (ParentL)
    ParentL->verifyLoop();
#endif

  // Unrolling can do several things to introduce new loops into a loop nest:
  // - Full unrolling clones child loops within the current loop but then
  //   removes the current loop making all of the children appear to be new
  //   sibling loops.
  //
  // When a new loop appears as a sibling loop after fully unrolling,
  // its nesting structure has fundamentally changed and we want to revisit
  // it to reflect that.
  //
  // When unrolling has removed the current loop, we need to tell the
  // infrastructure that it is gone.
  //
  // Finally, we support a debugging/testing mode where we revisit child loops
  // as well. These are not expected to require further optimizations as either
  // they or the loop they were cloned from have been directly visited already.
  // But the debugging mode allows us to check this assumption.
  bool IsCurrentLoopValid = false;
  SmallVector<Loop *, 4> SibLoops;
  if (ParentL)
    SibLoops.append(ParentL->begin(), ParentL->end());
  else
    SibLoops.append(AR.LI.begin(), AR.LI.end());
  erase_if(SibLoops, [&](Loop *SibLoop) {
    if (SibLoop == &L) {
      IsCurrentLoopValid = true;
      return true;
    }

    // Otherwise erase the loop from the list if it was in the old loops.
    return OldLoops.contains(SibLoop);
  });
  Updater.addSiblingLoops(SibLoops);

  if (!IsCurrentLoopValid) {
    Updater.markLoopAsDeleted(L, LoopName);
  } else {
    // We can only walk child loops if the current loop remained valid.
    if (UnrollRevisitChildLoops) {
      // Walk *all* of the child loops.
      SmallVector<Loop *, 4> ChildLoops(L.begin(), L.end());
      Updater.addChildLoops(ChildLoops);
    }
  }

  return getLoopPassPreservedAnalyses();
}

PreservedAnalyses LoopUnrollPass::run(Function &F,
                                      FunctionAnalysisManager &AM) {
  auto &LI = AM.getResult<LoopAnalysis>(F);
  // There are no loops in the function. Return before computing other expensive
  // analyses.
  if (LI.empty())
    return PreservedAnalyses::all();
  auto &SE = AM.getResult<ScalarEvolutionAnalysis>(F);
  auto &TTI = AM.getResult<TargetIRAnalysis>(F);
  auto &DT = AM.getResult<DominatorTreeAnalysis>(F);
  auto &AC = AM.getResult<AssumptionAnalysis>(F);
  auto &ORE = AM.getResult<OptimizationRemarkEmitterAnalysis>(F);
  AAResults &AA = AM.getResult<AAManager>(F);

  LoopAnalysisManager *LAM = nullptr;
  if (auto *LAMProxy = AM.getCachedResult<LoopAnalysisManagerFunctionProxy>(F))
    LAM = &LAMProxy->getManager();

  auto &MAMProxy = AM.getResult<ModuleAnalysisManagerFunctionProxy>(F);
  ProfileSummaryInfo *PSI =
      MAMProxy.getCachedResult<ProfileSummaryAnalysis>(*F.getParent());
  auto *BFI = (PSI && PSI->hasProfileSummary()) ?
      &AM.getResult<BlockFrequencyAnalysis>(F) : nullptr;

  bool Changed = false;

  // The unroller requires loops to be in simplified form, and also needs LCSSA.
  // Since simplification may add new inner loops, it has to run before the
  // legality and profitability checks. This means running the loop unroller
  // will simplify all loops, regardless of whether anything end up being
  // unrolled.
  for (const auto &L : LI) {
    Changed |=
        simplifyLoop(L, &DT, &LI, &SE, &AC, nullptr, false /* PreserveLCSSA */);
    Changed |= formLCSSARecursively(*L, DT, &LI, &SE);
  }

  // Add the loop nests in the reverse order of LoopInfo. See method
  // declaration.
  SmallPriorityWorklist<Loop *, 4> Worklist;
  appendLoopsToWorklist(LI, Worklist);

  while (!Worklist.empty()) {
    // Because the LoopInfo stores the loops in RPO, we walk the worklist
    // from back to front so that we work forward across the CFG, which
    // for unrolling is only needed to get optimization remarks emitted in
    // a forward order.
    Loop &L = *Worklist.pop_back_val();
#ifndef NDEBUG
    Loop *ParentL = L.getParentLoop();
#endif

    // Check if the profile summary indicates that the profiled application
    // has a huge working set size, in which case we disable peeling to avoid
    // bloating it further.
    std::optional<bool> LocalAllowPeeling = UnrollOpts.AllowPeeling;
    if (PSI && PSI->hasHugeWorkingSetSize())
      LocalAllowPeeling = false;
    std::string LoopName = std::string(L.getName());
    // The API here is quite complex to call and we allow to select some
    // flavors of unrolling during construction time (by setting UnrollOpts).
    LoopUnrollResult Result = tryToUnrollLoop(
        &L, DT, &LI, SE, TTI, AC, ORE, BFI, PSI,
        /*PreserveLCSSA*/ true, UnrollOpts.OptLevel, /*OnlyFullUnroll*/ false,
        UnrollOpts.OnlyWhenForced, UnrollOpts.ForgetSCEV,
        /*Count*/ std::nullopt,
        /*Threshold*/ std::nullopt, UnrollOpts.AllowPartial,
        UnrollOpts.AllowRuntime, UnrollOpts.AllowUpperBound, LocalAllowPeeling,
        UnrollOpts.AllowProfileBasedPeeling, UnrollOpts.FullUnrollMaxCount,
        &AA);
    Changed |= Result != LoopUnrollResult::Unmodified;

    // The parent must not be damaged by unrolling!
#ifndef NDEBUG
    if (Result != LoopUnrollResult::Unmodified && ParentL)
      ParentL->verifyLoop();
#endif

    // Clear any cached analysis results for L if we removed it completely.
    if (LAM && Result == LoopUnrollResult::FullyUnrolled)
      LAM->clear(L, LoopName);
  }

  if (!Changed)
    return PreservedAnalyses::all();

  return getLoopPassPreservedAnalyses();
}

void LoopUnrollPass::printPipeline(
    raw_ostream &OS, function_ref<StringRef(StringRef)> MapClassName2PassName) {
  static_cast<PassInfoMixin<LoopUnrollPass> *>(this)->printPipeline(
      OS, MapClassName2PassName);
  OS << '<';
  if (UnrollOpts.AllowPartial != std::nullopt)
    OS << (*UnrollOpts.AllowPartial ? "" : "no-") << "partial;";
  if (UnrollOpts.AllowPeeling != std::nullopt)
    OS << (*UnrollOpts.AllowPeeling ? "" : "no-") << "peeling;";
  if (UnrollOpts.AllowRuntime != std::nullopt)
    OS << (*UnrollOpts.AllowRuntime ? "" : "no-") << "runtime;";
  if (UnrollOpts.AllowUpperBound != std::nullopt)
    OS << (*UnrollOpts.AllowUpperBound ? "" : "no-") << "upperbound;";
  if (UnrollOpts.AllowProfileBasedPeeling != std::nullopt)
    OS << (*UnrollOpts.AllowProfileBasedPeeling ? "" : "no-")
       << "profile-peeling;";
  if (UnrollOpts.FullUnrollMaxCount != std::nullopt)
    OS << "full-unroll-max=" << UnrollOpts.FullUnrollMaxCount << ';';
  OS << 'O' << UnrollOpts.OptLevel;
  OS << '>';
}
