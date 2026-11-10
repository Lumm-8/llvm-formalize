# FormalPass M0–M8 Support Matrix

This matrix is the executable contract for the implemented M0 through M8 scope.
`SUPPORTED` means the named behavior is implemented and tested at its stated
layer. For M0 through M2 it does not mean that LLVM instructions are
symbolically executed; M3 adds only the scalar instruction subset below.
`REJECTED` means preflight deterministically produces an `INVALID_TASK` result.
`UNKNOWN` identifies unsupported semantics; M0 through M8 never turn one
of these cases into `SAFE`. `EXPERIMENTAL` is not used by M0/M1.

## M0 plugin and input contract

| Feature | Status | Owner | Tests / notes |
|---|---|---:|---|
| New-PM plugin load and `-passes=formal-exec` | SUPPORTED | M0 | `FormalPass.Plugin.EntryDiagnostics`, `tests/ir/m0-valid.ll` |
| Pinned LLVM package, major, revision and host `opt` | SUPPORTED | M0 | Configure pins `VCSRevision.h` and the exact `opt` SHA-256; runtime requires the host executable to be the configured pinned file and records its path/hash/version/revision; incompatible LLVM 13 is a negative test |
| Whole-module LLVM Verifier | SUPPORTED | M0 | `tests/ir/m0-malformed.ll`; verifier runs before entry and semantic checks |
| Exact, non-interposable external C `void ()` entry definition | SUPPORTED | M0 | `tests/FormalPassUnitTest.cpp`, `tests/ir/m0-valid.ll` |
| Missing/declaration/wrong-signature entry | REJECTED | M0 | `tests/ir/m0-entry-errors.ll` |
| Local, available-externally, weak/interposable, or non-C calling-convention entry | REJECTED | M0 | `tests/ir/m0-entry-errors.ll` |
| Non-C function or call-site calling convention in the reachable call graph | UNKNOWN | M0 | Every `CallBase` and reachable definition must use the C convention; the call-site/callee mismatch case is covered by `FormalPassCallSite` and `tests/ir/m0-unsupported-contract.ll` |
| ABI-affecting function, parameter and call-site attributes | MODELED SUBSET / UNKNOWN | M7 | Defined direct calls validate concrete `nonnull`, `dereferenceable`, `align`, `byval`, `sret`, and selected effect attributes; unproved promises, `inalloca`, `preallocated`, and other forms return UNKNOWN. See [M7_CALLS_HEAP.md](M7_CALLS_HEAP.md) |
| Linux x86_64, little-endian, 64-bit default pointers | SUPPORTED | M0 | Preflight scope only; `tests/ir/m0-valid.ll` |
| Missing triple or DataLayout | REJECTED | M0 | `tests/ir/m0-missing-triple.ll` and `FormalPassTargetProperties`; `opt` may inject a default layout before plugin execution, so the empty-layout branch is unit-tested |
| Other target triples or incompatible layouts | REJECTED | M0 | `tests/ir/m0-target-contract.ll`, `tests/ir/m0-incompatible-layout.ll` |
| Missing/unrecognized producer metadata | REJECTED | M0 | Default fail-closed; trusted hand-written IR requires explicit `-formal-allow-unversioned-ir` and records a warning; `tests/ir/m0-missing-producer.ll` |
| Runtime host `opt` identity, size and SHA-256 | REJECTED ON MISMATCH | M0 | Hashes the running executable image, not only its pathname; known-vector/same-size-different-content unit tests and `RuntimeHostDigestMismatch` load a test-only wrong-digest plugin under the correct pinned `opt` |
| Reachable direct/aliased callee with a body | SUPPORTED | M0/M7 | Every alias in the resolution chain and the final function must have an exact, non-interposable definition; M0 preflight is covered by `tests/ir/m0-registration-and-limits.ll`; M7 executes single and chained aliases with normal frames, returns, and pointer effects in [`alias-calls.ll`](../tests/ir/m7/alias-calls.ll) |
| Reachable non-intrinsic declaration | MODELED ALLOWLIST / UNKNOWN | M0/M7 | Preflight admits only M7's named allocator, byte-memory, and termination models; the executor validates their ABI before execution. Other declarations remain UNKNOWN; `tests/ir/m0-reachable-declaration.ll`, `FormalExecutor.Lit.M7` |
| Reachable weak/interposable definition or alias | REJECTED | M0 | The module does not contain the exact runtime body; weak/linkonce/ODR aliases are rejected before following their aliasees; `tests/ir/m0-reachable-declaration.ll`, `tests/ir/m0-alias-contract.ll` |
| Unresolved function pointer | UNKNOWN | M0/M7 | `tests/ir/m0-unsupported-contract.ll`; execution/target-set analysis is M7 |
| Inline assembly | UNKNOWN | M0/post-V1 | `tests/ir/m0-unsupported-contract.ll` |
| Atomics and fences | UNKNOWN | M0/post-V1 | `tests/ir/m0-unsupported-contract.ll` |
| Scalable vectors | UNKNOWN | M0/post-V1 | `tests/ir/m0-unsupported-contract.ll`; dedicated execution semantics are not present |
| Fixed vectors | UNKNOWN | M0/M6 | Explicit preflight gate includes types nested in constant trees plus opaque-pointer `alloca` allocated and GEP source element types; `tests/ir/m0-semantic-manifest.ll`, `tests/ir/m0-nested-constant-types.ll`, `tests/ir/m0-opaque-pointer-semantic-types.ll` |
| Floating-point types and opcodes | UNKNOWN | M0/post-V1 | Explicit preflight gate includes every node in global/reachable constant trees plus opaque-pointer `alloca` allocated and GEP source element types; `tests/ir/m0-semantic-manifest.ll`, `tests/ir/m0-constant-fp-type.ll`, `tests/ir/m0-opaque-pointer-semantic-types.ll` |
| EH opcodes (`invoke`, pads, returns, `landingpad`, `resume`) | UNKNOWN | M0/post-V1 | Explicit preflight gate in `tests/ir/m0-semantic-manifest.ll` |
| `freeze`, poison/undef operands and poison-generating annotations | UNKNOWN | M0/M9 | Uses LLVM's common poison-annotation query for `nsw`/`nuw`/`exact`, `or disjoint`, `icmp samesign`, `zext nneg`, every GEP nowrap/inbounds/inrange flag, return attributes and poison metadata; nested `ConstantExpr` trees and global initializers are included |
| Unknown intrinsic, `llvm.trap`, `llvm.assume` | UNKNOWN | M0/later | Every non-manifest intrinsic is gated; `tests/ir/m0-semantic-manifest.ll` |
| Non-default pointer address spaces | UNKNOWN | M0/M4 | Direct instruction/function types, every type nested in reachable/global constant trees, and opaque-pointer `alloca`/GEP semantic types are gated; `tests/ir/m0-registration-and-limits.ll`, `tests/ir/m0-nested-constant-types.ll`, `tests/ir/m0-opaque-pointer-semantic-types.ll` |
| Non-empty `llvm.global_ctors` | REJECTED | M0 | `tests/ir/m0-global-ctors.ll`; constructors are not scheduled |
| Registration builtin call shape and constant ASCII names | SUPPORTED | M0 | Static preflight only; `tests/ir/m0-registration-and-limits.ll` |
| Registration semantics, name/overlap checks and return-time capture | SUPPORTED IN M5 | M5 | `FormalExecutor.Lit.M5`, `FormalExecutor.M5.RelationUnit`, `FormalExecutor.M5.OptPlugin`; the plugin executes M5 with `-formal-run-m5` |
| Configurable module, type, constant and metadata limits | SUPPORTED | M0 | Functions/blocks/instructions, integer width, aggregate/type work, constant traversal/payload/depth, global initializers, metadata nodes/strings/depth, metadata-wrapped Values, `llvm.ident` entries/bytes and canonical textual IR bytes. Type budgets include opaque-pointer `alloca` allocated and instruction/ConstantExpr GEP source element types. Metadata SCCs are condensed before greatest-depth analysis, so legal cycles are accepted while shared DAG depth is preserved; every scheduled edge consumes a cumulative work budget. Covered by `tests/ir/m0-resource-bounds.ll`, `tests/ir/m0-hidden-type-resource-bounds.ll`, `tests/ir/m0-metadata-bounds.ll`, `tests/ir/m0-metadata-shared-depth.ll`, and `tests/ir/m0-metadata-cycles.ll` |
| No IR mutation / preserved analyses | SUPPORTED | M0 | `tests/ir/m0-preserved.ll` |
| Run-isolated atomic `result.json` | SUPPORTED | M0 | Each invocation atomically creates `runs/run-<generated-id>/`; no fixed root result is authoritative. JSON binds run ID, canonical IR SHA-256 and exact host identity. Forced writer failure and concurrent same-root runs cannot alias results; `StaleArtifact` and `ConcurrentArtifacts` |
| Plugin operation exit codes | SUPPORTED | M0 | UNKNOWN `2`, invalid task/configuration `3`, artifact creation/write/internal error `4`; without `-formal-run-m5`, normal `opt` completion `0` means preflight success only; `FormalPass.Plugin.ExitCodes`, `ArtifactIOFailure` |

## M1 expression semantics

| Feature | Status | Owner | Tests / notes |
|---|---|---:|---|
| Explicit `Bool` sort | SUPPORTED | M1 | `FormalSort.ExplicitBoolBitVectorAndArraySorts` |
| Explicit `BitVector(width)` sort | SUPPORTED | M1 | Positive width; widths are bounded by `ExprContextOptions` |
| `Array(BitVector(indexWidth), BitVector(8))` sort | SUPPORTED | M1 | Byte arrays only; `FormalArray.*` |
| `Constant` and `Symbol` | SUPPORTED | M1 | Constants use `llvm::APInt`; symbol names are validated, and convenience constant constructors reject over-limit widths before allocating an `APInt` |
| `Add/Sub/Mul/UDiv/SDiv/URem/SRem` | SUPPORTED | M1 | Fixed-width SMT bitvector semantics; division-by-zero nodes are not folded |
| `And/Or/Xor/Shl/LShr/AShr` | SUPPORTED | M1 | Fixed-width SMT bitvector semantics |
| `Eq/Ult/Ule/Slt/Sle` | SUPPORTED | M1 | Results have `Bool` sort; signed and unsigned comparisons are distinct |
| `Not` and BV1-to-Bool conversion | SUPPORTED | M1 | No implicit Bool/BV coercion |
| `Select` | SUPPORTED | M1 | Bool guard and identical scalar arm sorts required |
| `Concat/Extract/ZExt/SExt/Trunc` | SUPPORTED | M1 | Boundary and width validation in `tests/ExprUnitTest.cpp` |
| Immutable, context-owned hash-consed nodes | SUPPORTED | M1 | Same structure shares identity; cross-context operands are rejected |
| Persistent `UpdateList`, `Read`, byte writes | SUPPORTED | M1 | Structural sharing, read-after-write, update order, fork isolation, context-lifetime safety and a hard Array-count limit |
| Local bounded simplification | SUPPORTED | M1 | Rules are listed in `docs/SOUNDNESS.md`; AST depth/node/update limits apply |
| Stable SMT-LIB v2 output | SUPPORTED | M1 | Incremental bounded output, linear UpdateList serialization, declaration lower-bound checks, cross-process byte comparison and pinned STP validation |
| Random expression property tests | SUPPORTED | M1 | Fixed seeds; full-width APInt folds plus symbolic AST/assignment checks through STP cover arithmetic, div/rem, shifts, signed comparisons, select, concat/extract/ext/trunc, Bool/Not/BV1, random array updates, and randomized simplification-vs-raw-SMT equivalence |
| Undefined division fold boundaries | SUPPORTED | M1 | Divide/remainder by zero and `sdiv(INT_MIN,-1)` remain explicit nodes; the later LLVM executor must gate UB |
| LLVM `i1` instruction semantics | SUPPORTED | M3 | Stored as BV1; branch and select guards use the single explicit `ExprContext::toBool` conversion |
| `undef`, poison, `freeze`, `nsw`, `nuw`, `exact` | UNKNOWN | M9 | These are not represented by the M1 Expr AST |

## M2 solver boundary

| Feature | Status | Owner | Tests / notes |
|---|---|---:|---|
| Backend-neutral `check`, `mayBeTrue`, `mustBeTrue`, `getValue`, `getInitialValues` | SUPPORTED | M2 | `FormalSolver.DistinguishesSatUnsatAndTruthQueries`, width/model tests |
| SAT / UNSAT / UNKNOWN / TIMEOUT / INTERNAL_ERROR separation | SUPPORTED | M2 | Stable enum and machine reason codes; backend error/invalid query/timeout cannot become UNSAT |
| Query ID, elapsed time, backend/version/SAT-backend metadata | SUPPORTED | M2 | Every `SolverResult`; the production constructor runtime-checks STP 2.3.3 + revision and the complete locked-compiler/asserting/testing-on/shared/MiniSat-only compilation profile selected by the dynamic loader |
| Fresh STP VC per query | SUPPORTED | M2 | `QueriesAndTranslationCachesAreContextIsolated`; no incremental push/pop dependency |
| Per-context Formal Expr and Array translation cache | SUPPORTED | M2 | Hit/translation counters and distinct context IDs are tested |
| Bool and BV1 separation | SUPPORTED | M2 | Only explicit `toBool` crosses the boundary; invalid BV constraint fails deterministically |
| All M1 scalar nodes through public STP C API | SUPPORTED | M2 | Direct validity/refutation queries cover arithmetic, overflow, bitwise, div/rem edge cases, overshifts, comparisons, Select, concat/extract/ext/trunc at widths through 128; `getValue` reads the translated target from STP and independently audits it |
| Persistent Array/Read/Write translation | SUPPORTED | M2 | Latest-write order, read-after-write, and initial-root extraction through symbolic multi-update/alias chains are validated together |
| Arbitrary-width scalar model extraction | SUPPORTED | M2 | `i1`, `i8`, `i24`, `i32`, `i64`, `i65`, `i128`; STP bit strings are parsed into APInt |
| Initial byte-array model extraction | SUPPORTED | M2 | Requests use stable per-query array IDs; parent validation requires exact request order/root identity/byte count and equality with the returned base-array model. Ambiguous distinct same-name/sort arrays fail closed |
| SAT model validation by Formal evaluator | SUPPORTED | M2 | The wire model must contain exactly every query-schema scalar name and sort, so even symbols in inactive Select branches cannot be omitted. Every asserted original constraint plus truth-query polarity is then evaluated independently; `getValue` and every requested initial byte are cross-checked against the returned model |
| Worker protocol and resource isolation | SUPPORTED | M2 | Versioned query serialization plus immediate `fork`/`exec` helper; exact query-derived sorts/widths/array IDs, bounded counts/strings/response, digit/range-checked APInt, no trailing bytes, strict status/reason/payload matrix, parse exception boundary, RLIMIT core/file/address-space/CPU, and normal worker exit for LSan. Signal/unexpected-exit/resource-limit termination is UNKNOWN/`UNKNOWN-SOLVER-FAILURE`; reserved internal exit codes 123–126 and malformed data remain INTERNAL_ERROR, with the exit code retained in diagnostics |
| Timeout handling | SUPPORTED | M2 | Finite queries use one overflow-safe, saturating absolute parent deadline across preparation, serialization, worker execution, response parsing and mandatory model validation. Polling observes `waitpid(WNOHANG)` before deciding to kill; normally exited workers, including exit/kill races, are read and classified. After envelope validation, worker InternalError and SAT model semantics are interpreted before final timeout classification. Valid at-or-after-deadline results cannot be SAT/UNSAT, while malformed protocol/model results remain INTERNAL_ERROR and are never masked by TIMEOUT. Arbitrary parent `AdditionalModelAudit` callbacks are rejected with finite deadlines and require explicit `UnlimitedTimeout`. MiniSat conflict return 3 maps to TIMEOUT; zero timeout returns before query traversal or STP initialization |
| Failed/timeout/model-invalid SMT-LIB query export | SUPPORTED | M2 | Nonzero-deadline failures produce standalone QF_BV/QF_AUFBV artifacts with original target/constraints, versions, options, timeout and reason code. Zero-deadline TIMEOUT intentionally writes only metadata plus an empty query and never traverses the untrusted query |
| Z3 representative cross-check | SUPPORTED IN CI | M2 test oracle | The CI container installs Z3 and configures with `FORMAL_REQUIRE_Z3_TEST_ORACLE=ON`, making absence a configuration failure. Every semantic sample emits both an expected-UNSAT validity query and an expected-SAT witness query; scalar/real Array select-store, signed div/rem/comparison, AShr and Bool/Not families are covered. Non-CI builds retain an explicit capability skip when Z3 is absent |

## M3 scalar symbolic executor

M3 is exposed as the `FormalExecutor` library and the test-oriented
`FormalScalarExecutor` driver. Function arguments must be explicitly pre-bound
to scalar Expr values. This is not the M5 registration API and is not a claim
that ordinary Clang `-O0` output is supported.

| Feature | Status | Tests / notes |
|---|---|---|
| `ExecutionState`, one-frame `StackFrame`, `ProgramCounter` | SUPPORTED | Stable state IDs, depth, instruction count, predecessor, local bindings, constraints, status and IR/source termination location |
| Deterministic DFS worklist | SUPPORTED | LLVM successor/case order is preserved under LIFO scheduling; results are emitted in stable state-ID order |
| Integer widths | SUPPORTED | APInt/Expr fixed-width semantics, including i1, i8, i24, i32, i65 and i128 |
| Constants and inputs | SUPPORTED | `ConstantInt` and explicitly pre-bound integer arguments only; no missing SSA value is synthesized |
| Modular arithmetic and bitwise operations | SUPPORTED | `add sub mul and or xor`; unflagged fixed-width semantics |
| Division, remainder and shifts | SUPPORTED WITH DEFINEDNESS SPLIT | `udiv sdiv urem srem shl lshr ashr`; defined and undefined guards are independently queried |
| Integer comparisons | SUPPORTED | All ten `icmp` predicates; signed and unsigned translations are separate |
| Integer casts | SUPPORTED | `trunc zext sext`; same-width integer `bitcast` where LLVM admits it |
| `select` | SUPPORTED | Builds one Expr and never forks, constrains, or queries the solver |
| Branch and switch | SUPPORTED | Both branch polarities queried; every switch case plus the all-cases-false default guard queried; only SAT successors scheduled |
| PHI | SUPPORTED | Incoming value selected by actual predecessor; all PHIs in a block resolve against one pre-entry local snapshot, then commit together |
| `ret void`, `ret integer` | SUPPORTED | Produces a `Completed` `PathResult` with constraints and termination location |
| `unreachable` | ERROR | A feasible path reaching it is `REACHED-UNREACHABLE`, distinct from incomplete and internal error |
| Loops/backedges | BOUNDED | Natural loops use per-frame dynamic activation counters; each traversed latch-to-header edge counts once; bound exhaustion is `UNKNOWN-LOOP-BOUND` |
| Solver non-answer | UNKNOWN | TIMEOUT, backend UNKNOWN and worker failure preserve the affected path as incomplete; none is pruned as UNSAT |
| Aggregate result | SUPPORTED | Internal error dominates; otherwise path error, then incomplete/UNKNOWN, then `M3_EXECUTOR_COMPLETE`. The last status is not program `SAFE` |

### M3 opcode table

| Classification | LLVM IR |
|---|---|
| Executed | `add`, `sub`, `mul`, `udiv`, `sdiv`, `urem`, `srem`, `and`, `or`, `xor`, `shl`, `lshr`, `ashr`; `icmp eq/ne/ult/ule/ugt/uge/slt/sle/sgt/sge`; `trunc`, `zext`, `sext`, legal integer `bitcast`; `select`, `phi`, unconditional/conditional `br`, `switch`, `ret`, `unreachable` |
| Scalar operands accepted | `ConstantInt`, pre-bound integer `Argument`, and an already-bound supported instruction result |
| UNKNOWN: memory/pointers | `alloca`, `load`, `store`, `getelementptr`, pointer/integer casts and other non-integer casts |
| UNKNOWN: calls/control | `invoke`, all EH instructions, `indirectbr`, `callbr`, `va_arg`; unsupported CFG cycles |
| UNKNOWN: other value families | atomics/fences, floating point, vectors, aggregates, unsupported constants/opcodes/types |
| UNKNOWN: LLVM special values/flags | `undef`, `poison`, `freeze`, `nsw`, `nuw`, `exact`, other poison-generating annotations, overshift, divide by zero and `sdiv INT_MIN/-1` |

### M3 UNKNOWN and error reason codes

| Kind | Stable codes |
|---|---|
| Unsupported value/IR | `UNKNOWN-UNSUPPORTED-CONSTANT`, `UNKNOWN-UNSUPPORTED-CAST`, `UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-CALL`, `UNKNOWN-UNSUPPORTED-EXCEPTION-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-ATOMIC-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-FLOATING-POINT`, `UNKNOWN-UNSUPPORTED-VECTOR`, `UNKNOWN-UNSUPPORTED-AGGREGATE`, `UNKNOWN-UNSUPPORTED-INSTRUCTION`, `UNKNOWN-UNSUPPORTED-TYPE` |
| Undefined/special semantics | `UNKNOWN-UNDEF`, `UNKNOWN-POISON`, `UNKNOWN-FREEZE`, `UNKNOWN-NSW-NUW`, `UNKNOWN-EXACT`, `UNKNOWN-POISON-ANNOTATION`, `UNKNOWN-DIVIDE-BY-ZERO`, `UNKNOWN-SDIV-OVERFLOW`, `UNKNOWN-OVERSHIFT` |
| Completeness/resource | `UNKNOWN-LOOP-BOUND`, `UNKNOWN-LOOP-UNSUPPORTED`, `UNKNOWN-WALL-TIME-LIMIT`, `UNKNOWN-INSTRUCTION-LIMIT`, `UNKNOWN-STATE-LIMIT`, `UNKNOWN-SOLVER-QUERY-LIMIT`, `UNKNOWN-SOLVER-TIMEOUT`, `UNKNOWN-SOLVER-UNKNOWN`, `UNKNOWN-SOLVER-FAILURE` |
| Feasible program error | `REACHED-UNREACHABLE` |
| Executor invariant failure | `INTERNAL-MISSING-SSA-BINDING`, `INTERNAL-INVALID-PHI-PREDECESSOR`, `INTERNAL-INVALID-SYMBOLIC-BINDING`, `INTERNAL-NO-FEASIBLE-SUCCESSOR`, `INTERNAL-INVALID-EXECUTOR-STATE`, `INTERNAL-SOLVER-ERROR`, `INTERNAL-EXPR-CONSTRUCTION` |

### M3 test inventory

`tests/ir/m3/scalar.ll`, `control.ll`, and `unknown.ll` cover every `icmp`,
modular arithmetic, all required widths and casts, normal shifts/div/rem,
definedness failures, diamond feasibility in all three combinations, select
without a fork, switch cases/default/duplicate destinations, predecessor-aware
and parallel PHIs, both return forms, `unreachable`, precise unsupported
families, solver timeout, loop rejection, and repeated-run stability.
Every unsupported-family check asserts both its stable reason code and exact
`function:block:opcode#ordinal` termination location.
`FormalExecutor.Unit` injects both-UNSAT, TIMEOUT, UNKNOWN, worker-failure and
solver-internal results and checks missing-binding and Bool-constraint
invariants. `FormalExecutor.Differential` exhausts three independent i8 domains
and the complete i8 x i8 domain. The pinned Clang compiles the same hand-written
IR into a native shared object, and every native function result is checked
against the exactly one matching symbolic path and return Expr. This separately
covers signed/unsigned branches, switch, casts and two-input selection without
using an APInt formula as the concrete oracle.

## M4 object memory, pointers and GEP

The test-oriented executor consumes hand-written IR with `--m4` (its default
mode retains M3 behavior); M0 preflight
remains a separate static contract and does not yet delegate to M4. Full
algorithms, status meanings and limitations are in
[M4_MEMORY.md](M4_MEMORY.md).

| Family | Executed | UNKNOWN / error boundary |
|---|---|---|
| Global constants | Zero and integer scalar, fixed nested array/struct integer and pointer initializers, null/global pointer and constant GEP pointer initializers; M6 retains `undef` aggregate members as uninitialized bytes; read-only constant global | External global, alias and unsupported aggregate member: UNKNOWN; `externally_initialized` entry bytes stay unknown until overwritten; write to read-only: `READ_ONLY_WRITE` |
| Stack allocation | Concrete fixed-size `alloca`; fresh dynamic object ID and checked alloc-size multiplication | Symbolic count: `SYMBOLIC_ALLOCATION_SIZE`; scalable or oversized: UNKNOWN |
| Memory access | Non-atomic/non-volatile integer and pointer `load/store`, DataLayout store size and endian order, checked bounds and alignment | Null/OOB/dead/definite misalignment: error, except `null_pointer_is_valid` address-zero accesses return UNKNOWN while address-zero memory is unmodeled; uninitialized bits, uncertain alignment, non-default address space: UNKNOWN |
| GEP | First index, nested arrays, constant struct field with StructLayout padding, signed symbolic indices, one-past construction, plain `inbounds` checks including all-zero-index null GEP | Vector GEP, constant `inrange`, unsupported flags, overflow or unproved inbounds: UNKNOWN |
| Pointer values | Null, guarded object/offset alternatives, pointer `eq/ne` and select, pointer store/load through shadow; null-derived unflagged GEP compares numeric offsets; object instances must match the current state's binding even when numeric IDs collide | Distinct constant globals with a mergeable `unnamed_addr`/`local_unnamed_addr` address, integer loads observing pointer-derived bytes (including partial overwrite and `memcpy`), arbitrary pointer bytes, lost shadow, foreign object instance, `inttoptr` and unrecoverable pointer/int round trip: UNKNOWN |
| Pointer `llvm.memcpy` (M4 baseline) | Bounded concrete disjoint nonvolatile copy, preserving fully covered pointer shadows and invalidating partial destination shadows; zero-length no-op after supported argument-attribute checks | Concrete overlap reports `OVERLAPPING_MEMCPY`; unsupported parameter attributes (`nonnull`, `dereferenceable`, etc.), unproved zero-length alignment, symbolic length/offset and volatility: UNKNOWN. M6 adds `memmove` and `memset` |
| Resolution | SAT provenance candidates only; candidate range guard retained in constraints | Solver timeout/UNKNOWN and cap exhaustion: UNKNOWN; no adjacent-object conflation |

M4 stable feasible errors are `NULL_DEREFERENCE`, `OUT_OF_BOUNDS`,
`READ_ONLY_WRITE`, `DEAD_OBJECT_ACCESS`, and `MISALIGNED_ACCESS`.
Incomplete reasons are `UNINITIALIZED_READ`, `POINTER_PROVENANCE_LOST`,
`POINTER_RESOLUTION_INCOMPLETE`, `UNSUPPORTED_ADDRESS_SPACE`,
`UNKNOWN-POINTER-COMPARISON`,
`SYMBOLIC_ALLOCATION_SIZE`, `UNKNOWN-SCALABLE-TYPE`,
`UNKNOWN-UNSUPPORTED-GLOBAL`, `UNKNOWN-INBOUNDS-GEP`,
`UNKNOWN-ALLOCATION-LIMIT`, and `UNKNOWN-ALIGNMENT-UNDETERMINED`.
Broken model invariants are `INTERNAL_MEMORY_MODEL_ERROR`. No completed M4
execution is a full-program SAFE verdict.

`FormalMemory.Unit` tests direct COW/shadow/bounds, foreign-ID collisions,
overlapping logical intervals, mergeable/external-initialized globals,
symbolic inbounds forks, `null_pointer_is_valid`, direct address-sensitive
byte-marker and memcpy propagation assertions, mandatory validated error
models, model-derived native ASan/UBSan replay for all four currently
executable memory-error classes, and fixed-seed differential properties.
`EveryReportedM4IRMemoryErrorModelIsNativelyReplayed` discovers all M4 lit
entries expecting a memory error, requires an exact replay-ledger match, and
validates and replays every error path model they report. The direct C++
error-path tests likewise replay every reported model. This is a gate over
the M4 test corpus, not arbitrary application replay.
`FormalExecutor.Lit.M4` covers the IR families in
`tests/ir/m4/`. `FormalMemory.Benchmark` asserts object sharing across 64
forks and 128 stack objects. The standalone sanitized
`tests/ir/m4/replay.c` gives native diagnostics for those four classes.
M7 adds dead-object access on return and full-object lifetime intrinsics;
`FormalExecutor.M7.UseAfterReturnReplay` checks one exported witness against a
native ASan `stack-use-after-return` report;
bounded loop execution remains gated for M8.

## M5 registered byte relations

`formal-check` and the plugin's `-formal-run-m5` option execute validated
`void ()` entries. The test-oriented `FormalScalarExecutor --m5` also accepts
hand-written IR. See [M5_REGISTRATION.md](M5_REGISTRATION.md) for the API and
artifact contract.

| Feature | Status | Tests / notes |
|---|---|---|
| Scalar, byte-array, stack and global input/output slices | SUPPORTED | `FormalExecutor.Lit.M5`; input slices of non-pointer fields are accepted even when the containing object has concrete pointer fields, while slices overlapping pointer fields return UNKNOWN; fresh symbolic input bytes are installed at registration and outputs are captured before top-frame destruction |
| In-place registration and disjoint slices | SUPPORTED | Input/output may share a name and object; same-kind overlap and duplicate names, including exact duplicate calls, return UNKNOWN; `FormalExecutor.Lit.M5`, `FormalExecutor.M5.ArtifactClosure` |
| `__formal_assume` | SUPPORTED FOR i32 ABI | The public C ABI and native replay use `void(i32)`; M5 compares that value with zero and false prunes the path. Other IR widths are UNKNOWN in the executor and invalid at preflight; solver UNKNOWN stays incomplete. `FormalExecutor.Lit.M5`, `FormalExecutor.Unit`, `FormalExecutor.M5.AssumeTruthTable` |
| Closed SMT relation and versioned JSON | SUPPORTED SUBSET | Internal free symbols and array reads are rejected; normal path disjunction preserves output tuples; partial execution exports an explicit underapproximation and cannot return SAFE; `FormalExecutor.M5.RelationUnit`, `FormalExecutor.M5.ArtifactClosure` |
| Exhaustive native equivalence | SUPPORTED SUBSET | STP proves both XOR directions for 256 byte inputs and four input domains; the 256 input scalar case and three four-input cases also check SAT and absence of extra outputs at each fixed input, including the complete two-output tuple; `FormalExecutor.M5.TruthTable`, `FormalExecutor.M5.AssumeTruthTable`, `FormalExecutor.M5.TupleTruthTable`, `FormalExecutor.M5.ClangO0TruthTable` |
| Plugin/formal-check semantic agreement | SUPPORTED | Both call `analyzeM5` and `writeM5Artifacts`; tests compare relation bytes and semantic JSON for Clang `-O0` and branch-specific object bindings; `FormalExecutor.M5.OptPlugin`, `FormalExecutor.M5.ArtifactClosure` |
| Native model replay | SUPPORTED SUBSET | Registered byte inputs are injected before first read; the bounds-error test runs `formal-check` on Clang IR from the same source compiled into its ASan/UBSan replay executable, and checks both normal capture and an error witness. Arbitrary caller-supplied binaries have no automatic IR/binary identity proof |
| Builtin `noreturn` attribute | UNKNOWN / INVALID TASK | Executor rejects a call-site or declaration `noreturn` before applying builtin effects; M0 preflight rejects it as invalid registration. `tests/ir/m5/builtin-attributes.ll` |
| Builtin restricted memory effects | UNKNOWN / INVALID TASK | Registration and assume builtins require unrestricted `memory(readwrite)` effects after combining call-site and declaration attributes. `memory(none)`, `memory(read)`, and other restrictions are rejected before effects; `tests/ir/m5/builtin-memory-effects.ll`, `tests/ir/m5/builtin-memory-declaration.ll` |
| M5 entry `noreturn` attribute | UNKNOWN / INVALID TASK | Preflight rejects the entry attribute; direct executor use returns UNKNOWN with a false normal relation. A dynamic `ret` from such an entry is LLVM undefined behavior; `tests/ir/m5/entry-noreturn.ll` |
| Pointer-field inputs, symbolic sizes, path-varying descriptors, dead outputs | UNKNOWN | Explicit diagnostics in `FormalExecutor.Lit.M5`; pointer-byte slices are rejected; full-object `llvm.lifetime.end` makes output capture UNKNOWN |
| Zero sizes, null registration, out-of-bounds regions, conditional pointer candidates | UNKNOWN | Deterministic rejection in M5/V1; no native UB is silently modeled; `tests/ir/m5/edge-cases.ll` |
| Heap-region registration and heap lifetime | SUPPORTED SUBSET | M7 allocators produce fresh objects and free ends them; `tests/ir/m5/heap-registration-blocked.ll` enables failure paths and explicitly excludes null with `__formal_assume`. Without failure forks, M5 records a success assumption and an incomplete failure path, so it cannot report unconditional SAFE; `FormalExecutor.M7.HeapEvents` checks all three allocators. Dead regions, symbolic sizes, and unsupported runtimes cannot yield SAFE |
| Ordinary direct calls and heap operations | SUPPORTED SUBSET / UNKNOWN | Defined C calls, frames, returns, `byval`/`sret`, stack lifetime, aliases, and named heap models are exercised by `FormalExecutor.Lit.M7` (including [`alias-calls.ll`](../tests/ir/m7/alias-calls.ll), calloc overflow, and pointer-shadow copying through realloc/byval), `FormalExecutor.M7.ClangABI` (including a pointer-bearing Clang byval argument and an exact pinned-Clang IR snapshot check), `FormalExecutor.M7.NoundefAggregate`, `FormalExecutor.M7.CrossCallTruthTable`, `FormalExecutor.M7.PluginConsistency`, seven M7 heap replay tests, `FormalExecutor.M7.OverlapMemcpyReplay`, `FormalExecutor.M7.UseAfterReturnReplay`, `FormalExecutor.M7.AbortReplay`, and `FormalExecutor.M7.AssertReplay`; passing native replay tests retain witnesses and native evidence in the build's `m7-evidence/`; unmodeled ABIs remain UNKNOWN |

## LLVM semantic families outside the M4 executor subset

| Family | Status | Owner | Current boundary |
|---|---|---:|---|
| Loads, stores, GEP, globals, stack objects | SUPPORTED SUBSET | M4/M7 | M7 adds full-object stack lifetime start/end and return-time frame cleanup; partial lifetimes remain UNKNOWN. See [M4_MEMORY.md](M4_MEMORY.md) and [M7_CALLS_HEAP.md](M7_CALLS_HEAP.md) |
| Aggregates and memory intrinsics | SUPPORTED SUBSET / UNKNOWN | M6 | Fixed nested struct/array SSA aggregates, extract/insertvalue, and aggregate load/store discard or write undef padding; bytewise `llvm.memcpy`/`llvm.memmove` and bounded `llvm.memset` are modeled. Overlapping `memcpy` is an error; symbolic lengths/offsets, volatile intrinsics and unmodeled forms return UNKNOWN. See [M6_MEMORY.md](M6_MEMORY.md) |
| Vectors | UNKNOWN | post-V1 | Vector value and memory semantics are not modeled |
| Direct calls, recursion, heap and external models | SUPPORTED SUBSET / UNKNOWN | M7 | Separate frames and shared address space; recursion depth bound returns UNKNOWN. x86_64 Linux malloc/calloc/realloc/free and selected C/C++ runtime symbols have explicit models; unknown calls stay UNKNOWN. See [M7_CALLS_HEAP.md](M7_CALLS_HEAP.md) |
| Loops and path truncation | BOUNDED / CONDITIONAL / UNKNOWN | M8 | Natural loops use per-frame dynamic activations and actual latch-to-header edges. Residual CFG cycles after removing natural backedges, including subcycles inside an outer loop, return `UNKNOWN-IRREDUCIBLE-CYCLE`. The default bound policy records UNKNOWN; explicit error policy produces INVALID_TASK, and explicit assume-exit produces CONDITIONAL. `--require-local-loop-bound` prevents fallback when expected local metadata has vanished. See [M8_LOOPS.md](M8_LOOPS.md) |
| Floating point | UNKNOWN | post-V1 | No FP Expr nodes exist |
| TLS, exceptions, RTTI, virtual dispatch, threads | UNKNOWN | post-V1 | Atomics/inline assembly are classified `UNKNOWN` at preflight |
| Function pointers | UNKNOWN | M7 | Indirect calls without a statically resolved direct callee are classified UNKNOWN at preflight/execution |
| SAFE/UNSAFE verdicts and model-injection replay | SUPPORTED SUBSET | M5 | Registered byte programs produce closed relations and replayable error inputs; unsupported paths remain UNKNOWN |

M8's loop tests include exact bound edges, dynamic activations, local metadata,
irreducible cycles, native differential relation checks, and pass/driver
agreement. The only transformation accepted as a bound source is metadata
still present on the analyzed IR.

## Explicit M0 opcode and intrinsic manifest

`VALIDATED` below means only that the opcode belongs to the intended V1 input
contract; M0 does not execute it. Reachable opcodes are classified as follows:

| Classification | Opcodes |
|---|---|
| Accepted input-contract structure | `ret`, `br`, `switch`, `unreachable`; integer arithmetic/div/rem/shifts/bitwise; `alloca`, `load`, `store`, `getelementptr`; integer/pointer casts; `icmp`, `phi`, direct `call`, `select`, `extractvalue`, `insertvalue`. For opaque pointers, `alloca` allocated types and GEP source element types must independently satisfy the semantic/type-resource gates |
| UNKNOWN: exception handling | `invoke`, `resume`, `cleanupret`, `catchret`, `catchswitch`, `cleanuppad`, `catchpad`, `landingpad` |
| UNKNOWN: floating point | `fneg`, FP arithmetic, FP/integer casts, `fptrunc`, `fpext`, `fcmp`, or any instruction carrying an FP type |
| UNKNOWN: vectors | Any fixed/scalable vector-typed instruction, including element/shuffle operations |
| UNKNOWN: value/UB semantics | `freeze`, poison/undef constants, all LLVM poison-generating instruction annotations and flagged nested `ConstantExpr`, including wrap/exact/disjoint/samesign/nneg and GEP nowrap/inbounds/inrange |
| UNKNOWN: unsupported control/ABI | `indirectbr`, `callbr`, `va_arg`, internal user opcodes, atomics/fences, inline assembly and unresolved calls |

The only accepted intrinsic IDs are debug records, `lifetime.start/end`, and
the planned V1 bulk-memory contract (`memcpy`, `memmove`, `memset`). Every other
intrinsic ID is `UNKNOWN [UNSUPPORTED_INTRINSIC]`; acceptance here still does
not claim execution semantics before its owner milestone.
