# FormalPass M0/M1/M2 Support Matrix

This matrix is the executable contract for the implemented M0, M1 and M2 scope.
`SUPPORTED` means the named behavior is implemented and tested at its stated
layer. It does not mean that LLVM instructions are symbolically executed.
`REJECTED` means preflight deterministically produces an `INVALID_TASK` result.
`UNKNOWN` identifies semantics owned by a later milestone; M0/M1/M2 never turn one
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
| ABI-affecting function, parameter and call-site attributes (`byval`, `sret`, `inalloca`, `preallocated`, `noalias`, and related attributes) | UNKNOWN | M7 | M0 relies on LLVM Verifier for structural validity but does not interpret or certify these ABI semantics; an executor must gate/model them before any `SAFE` result |
| Linux x86_64, little-endian, 64-bit default pointers | SUPPORTED | M0 | Preflight scope only; `tests/ir/m0-valid.ll` |
| Missing triple or DataLayout | REJECTED | M0 | `tests/ir/m0-missing-triple.ll` and `FormalPassTargetProperties`; `opt` may inject a default layout before plugin execution, so the empty-layout branch is unit-tested |
| Other target triples or incompatible layouts | REJECTED | M0 | `tests/ir/m0-target-contract.ll`, `tests/ir/m0-incompatible-layout.ll` |
| Missing/unrecognized producer metadata | REJECTED | M0 | Default fail-closed; trusted hand-written IR requires explicit `-formal-allow-unversioned-ir` and records a warning; `tests/ir/m0-missing-producer.ll` |
| Runtime host `opt` identity, size and SHA-256 | REJECTED ON MISMATCH | M0 | Hashes the running executable image, not only its pathname; known-vector/same-size-different-content unit tests and `RuntimeHostDigestMismatch` load a test-only wrong-digest plugin under the correct pinned `opt` |
| Reachable direct/aliased callee with a body | SUPPORTED | M0 | Every alias in the resolution chain and the final function must have an exact, non-interposable definition; strong alias success remains covered by `tests/ir/m0-registration-and-limits.ll`; execution remains M7 |
| Reachable non-intrinsic declaration | UNKNOWN | M0/M7 | Unsupported external semantics are incomplete, not malformed; `tests/ir/m0-reachable-declaration.ll` |
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
| Registration semantics, dominance, aliasing and capture | UNKNOWN | M5 | No registration execution is implemented |
| Configurable module, type, constant and metadata limits | SUPPORTED | M0 | Functions/blocks/instructions, integer width, aggregate/type work, constant traversal/payload/depth, global initializers, metadata nodes/strings/depth, metadata-wrapped Values, `llvm.ident` entries/bytes and canonical textual IR bytes. Type budgets include opaque-pointer `alloca` allocated and instruction/ConstantExpr GEP source element types. Metadata SCCs are condensed before greatest-depth analysis, so legal cycles are accepted while shared DAG depth is preserved; every scheduled edge consumes a cumulative work budget. Covered by `tests/ir/m0-resource-bounds.ll`, `tests/ir/m0-hidden-type-resource-bounds.ll`, `tests/ir/m0-metadata-bounds.ll`, `tests/ir/m0-metadata-shared-depth.ll`, and `tests/ir/m0-metadata-cycles.ll` |
| No IR mutation / preserved analyses | SUPPORTED | M0 | `tests/ir/m0-preserved.ll` |
| Run-isolated atomic `result.json` | SUPPORTED | M0 | Each invocation atomically creates `runs/run-<generated-id>/`; no fixed root result is authoritative. JSON binds run ID, canonical IR SHA-256 and exact host identity. Forced writer failure and concurrent same-root runs cannot alias results; `StaleArtifact` and `ConcurrentArtifacts` |
| Plugin operation exit codes | SUPPORTED | M0 | UNKNOWN `2`, invalid task/configuration `3`, artifact creation/write/internal error `4`; normal `opt` completion `0` means preflight operation success, never formal SAFE; `FormalPass.Plugin.ExitCodes`, `ArtifactIOFailure` |

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
| LLVM `i1` instruction semantics | UNKNOWN | M3 | M1 only exposes explicit BV1-to-Bool conversion |
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

## LLVM semantic families not implemented in M0/M1/M2

| Family | Status | Owner | Current boundary |
|---|---|---:|---|
| Integer opcodes, `icmp`, casts, PHI, branch, switch, return | UNKNOWN | M3 | IR is inspected but never interpreted |
| Loads, stores, GEP, globals, stack objects, lifetime | UNKNOWN | M4 | No memory model exists |
| Aggregates, vectors and memory intrinsics | UNKNOWN | M6 | Fixed/scalable vector use is classified `UNKNOWN` at preflight; planned bulk-memory intrinsics are admitted only as input-contract structure |
| Direct calls, recursion, heap and external models | UNKNOWN | M7 | Preflight only traverses direct defined callees |
| Loops and path truncation | UNKNOWN | M8 | No scheduler or loop execution exists |
| Floating point | UNKNOWN | post-V1 | No FP Expr nodes exist |
| TLS, exceptions, RTTI, virtual dispatch, threads | UNKNOWN | post-V1 | Atomics/inline assembly are classified `UNKNOWN` at preflight |
| Function pointers | UNKNOWN | M7 | Unresolved indirect calls are classified `UNKNOWN` at preflight |
| SAFE/UNSAFE verdicts and counterexample replay | UNKNOWN | M3+ / M5 | M2 answers solver queries only; it does not execute IR or issue program verdicts |

The future capability-gate CTests are intentionally skipped. They document that
path truncation, loop bounds, unsupported instruction execution and native
UNSAFE replay cannot yet be classified by an executor.

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
