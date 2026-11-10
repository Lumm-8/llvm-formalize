# M8 loop bounds and completeness

The executor constructs `DominatorTree` and `LoopInfo` for every function body
in the module. It executes the original CFG; it does not physically unroll IR.
The analyzer removes every LoopInfo latch-to-header edge and checks the
remaining CFG for cyclic SCCs. Any residual cycle, including one inside a
single-entry outer natural loop, is `UNKNOWN-IRREDUCIBLE-CYCLE`. Other
revisited blocks outside natural loops are `UNKNOWN-LOOP-UNSUPPORTED`.

Each loop counter belongs to a dynamic call frame and one dynamic loop
activation. Entering a loop header from outside creates an activation with zero
backedge traversals. An internal edge retains it. A real latch-to-header edge
increments it. An exit edge ends it. A later entry creates a new activation.
Nested loops have separate activations; forks copy their current counters;
repeated and recursive calls use different frame IDs. JSON records the frame
ID, activation ID, stable static `function:header` ID, bound, source, and
traversal count for every activated loop. A non-header entry into a natural
loop is UNKNOWN.

The repeated-callee test exercises a real exit followed by entry to the same
static loop in a fresh frame. In one reducible function CFG, a path from an
apparent exit back to the same dominated header is itself a backedge, so LLVM
includes that path in the natural loop rather than treating it as a separate
exit and reentry.

The bound means **maximum CFG latch-to-header traversals per dynamic
activation**. Bound `N` permits exactly `N` such edges; the next edge is
suppressed before the header executes. Bound zero permits the initial header
entry and body, but no backedge. This is not necessarily the number of source
loop-body executions after loop rotation or other CFG changes. Multiple latches
are counted by the actual edge traversed. A rejected path records the latch
instruction, attempted count `N+1`, limit `N`, and suppressed edge in JSON.

## Bound sources

The first available source wins:

1. `formal.loop.bound` loop metadata;
2. `llvm.loop.unroll.count` loop metadata;
3. `--loop-bound N` / `-formal-loop-bound=N` (default 32).

JSON emits bounds through the same unsigned encoding in root
`limits.default_loop_backedge_bound` and per-loop `backedge_bound`: values up
to `INT64_MAX` are numbers, and larger `uint64_t` values are decimal strings.

The metadata operands are unsigned integer constants from 0 through 2^64−1.
Zero is explicitly allowed. Duplicate, malformed, negative, oversized, or
different latch metadata is `UNKNOWN-LOOP-UNSUPPORTED`; the global default is
never substituted for a bad local annotation. A loop carrying only
`llvm.loop.unroll.disable` is treated conservatively as
`UNKNOWN-LOOP-METADATA-LOST`. `llvm.loop.unroll.count` is an optimization hint,
not a dedicated formal contract. Pinned Clang's
`#pragma clang loop unroll_count(3)` output is checked with FileCheck; an
`opt -passes=loop-unroll` test demonstrates that optimization consumes the
count and leaves `unroll.disable`. Run formal analysis on the checked emitted
IR before transformations that may consume or rewrite loop metadata. When a
pipeline relies on local bounds, pass `--require-local-loop-bound` or
`-formal-require-local-loop-bound`. This conservative provenance requirement
reports `UNKNOWN-LOOP-METADATA-LOST` if any reached natural loop lacks a
surviving `formal.loop.bound` or `llvm.loop.unroll.count`; the global default
is then not substituted. Without this opt-in requirement, an IR file whose
metadata has been completely erased is indistinguishable from a loop that
never carried a local bound. The pragma test executes actual transformed IR
and also checks a copy with the loop attachment removed under this option.

## Limit policies

`--loop-limit-policy` and `-formal-loop-limit-policy` accept:

| Value | Result when an edge exceeds its bound |
|---|---|
| `unknown` (default) | Suppress the edge, record `UNKNOWN-LOOP-BOUND`, and make the overall verdict UNKNOWN unless a feasible error has priority. |
| `error` | Suppress the edge and report `INVALID_TASK` (exit 3). This is an analysis policy failure, not a program error. |
| `assume-exit` | Suppress the edge, mark an explicit unwinding assumption, and report `CONDITIONAL` (exit 2) if no other incompleteness or feasible error exists. No synthetic program exit edge is added. |

Completed paths in a run with an incomplete path form an
`UNDER_APPROXIMATION` relation. Verdict order is INTERNAL_ERROR,
INVALID_TASK, a feasible closed UNSAFE witness, then UNKNOWN/CONDITIONAL and
SAFE. The
root JSON fields `loop_limit_policy` and `unwinding_assumption`, path fields
`loop_policy_error` and `unwinding_assumption`, plus the `loop` and
`loop_bounds` objects make the scope explicit.

The shared incompleteness ledger also covers recursion depth, state and
instruction caps, wall time, solver query cap, solver timeout/UNKNOWN, memory
and pointer candidate caps, unsupported instruction/call/attribute, and
registration failures. Each path includes state ID, call stack, IR/source
location, and any applicable limit and observed value. An UNSAT successor is
pruned and is not counted as incomplete.

`FormalExecutor.M8.BackedgeContract` checks N−1/N/N+1 and N=0 directly from
JSON. `DynamicActivations` checks break, continue, multiple exits/latches,
nested loops, calls, recursion, and state forks. `NativeDifferential` compares
all four permitted inputs of a hand-written loop to native C execution and
asks STP for both directional counterexamples and a relation XOR witness.

## Incomplete reason codes

The JSON `paths[].reason` and root `incompleteness_reasons` use these stable
codes. Existing M0–M7 codes remain part of the same ledger:

| Family | Codes |
|---|---|
| Loop/CFG | `UNKNOWN-LOOP-BOUND`, `UNKNOWN-LOOP-UNSUPPORTED`, `UNKNOWN-IRREDUCIBLE-CYCLE`, `UNKNOWN-LOOP-METADATA-LOST` |
| Resource and solver | `UNKNOWN-CALL-DEPTH`, `UNKNOWN-STATE-LIMIT`, `UNKNOWN-INSTRUCTION-LIMIT`, `UNKNOWN-WALL-TIME-LIMIT`, `UNKNOWN-SOLVER-QUERY-LIMIT`, `UNKNOWN-SOLVER-TIMEOUT`, `UNKNOWN-SOLVER-UNKNOWN`, `UNKNOWN-SOLVER-FAILURE`, `UNKNOWN-ALLOCATION-LIMIT`, `POINTER_RESOLUTION_INCOMPLETE` |
| Unsupported LLVM | `UNKNOWN-UNSUPPORTED-CONSTANT`, `UNKNOWN-UNSUPPORTED-CAST`, `UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-CALL`, `UNKNOWN-UNSUPPORTED-EXCEPTION-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-ATOMIC-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-FLOATING-POINT`, `UNKNOWN-UNSUPPORTED-VECTOR`, `UNKNOWN-UNSUPPORTED-AGGREGATE`, `UNKNOWN-UNSUPPORTED-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-TYPE`, `UNSUPPORTED_ADDRESS_SPACE`, `UNKNOWN-UNSUPPORTED-GLOBAL`, `UNKNOWN-SCALABLE-TYPE` |
| Undefined or unmodeled values | `UNKNOWN-UNDEF`, `UNKNOWN-POISON`, `UNKNOWN-FREEZE`, `UNKNOWN-NSW-NUW`, `UNKNOWN-EXACT`, `UNKNOWN-POISON-ANNOTATION`, `UNKNOWN-DIVIDE-BY-ZERO`, `UNKNOWN-SDIV-OVERFLOW`, `UNKNOWN-OVERSHIFT`, `UNINITIALIZED_READ`, `UNKNOWN-INBOUNDS-GEP`, `UNKNOWN-ALIGNMENT-UNDETERMINED` |
| Memory and registration | `POINTER_PROVENANCE_LOST`, `UNKNOWN-POINTER-COMPARISON`, `SYMBOLIC_ALLOCATION_SIZE`, `UNKNOWN-ALLOCATION-FAILURE-ASSUMED`, `UNKNOWN-ENTRY-ATTRIBUTE-CONFLICT`, `UNKNOWN-REGISTRATION-UNSUPPORTED`, `UNKNOWN-SYMBOLIC-REGISTRATION-SIZE`, `UNKNOWN-REGISTRATION-NAME`, `UNKNOWN-REGISTRATION-OVERLAP`, `UNKNOWN-USE-BEFORE-INPUT`, `UNKNOWN-REGISTRATION-DESCRIPTOR-MISMATCH`, `UNKNOWN-DEAD-REGISTERED-OUTPUT` |
