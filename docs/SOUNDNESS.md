# M0/M1/M2 Soundness and Trust Boundary

## Claim boundary

M0, M1 and M2 do not verify a C/C++ program and do not emit `SAFE` or `UNSAFE`.
M0 establishes that a module satisfies a narrow input contract. M1 provides a
solver-independent expression language. A successful M0 result is
`status: VALIDATED`, `reason: VALID`, and `verdict: null`; it is not evidence
that the module's instructions have been symbolically executed.

M2 supplies a solver query interface and STP translation boundary. No path
scheduler, LLVM instruction executor, memory model, call executor, program
verdict aggregation, registration API, state machine, or replay runtime is part
of this scope.

## M0 preflight guarantees

The pass invokes LLVM's verifier before all project-specific checks. It then
requires the configured entry to be an externally visible C-calling-convention
`void ()` function with an exact, non-interposable definition. The same rule
applies to reachable module callees; available-externally, weak, linkonce and
otherwise interposable bodies are not treated as the runtime implementation.
Direct-call alias resolution checks every `GlobalAlias` in the chain before
following its aliasee; a weak, linkonce, de-refinable or otherwise interposable
alias cannot be hidden by pointer-cast/alias stripping even when its ultimate
function has an exact body.
The accepted target contract is pinned LLVM
`20.0.0git`
IR for little-endian Linux x86_64 with 64-bit default pointers. Every Clang version
recorded in `llvm.ident` must match the pinned package version and Git revision.
Configure reads LLVM's generated `VCSRevision.h` and rejects a linked-toolchain
revision mismatch. It also hashes the configured `opt` at build time. At
runtime the pass opens Linux `/proc/self/exe`, which continues to identify the
executing inode across pathname replacement, computes that running image's
actual SHA-256, and compares filesystem identity, size and digest with the
configured executable. The kernel AF_ALG SHA-256 implementation is used when
available, with a bounded-memory portable implementation as fallback. Only an
exact match records host version/revision; JSON always records the measured
runtime digest and identifies its basis. Missing or unrecognized producer metadata is rejected by
default. Explicit `-formal-allow-unversioned-ir` is a trust override intended
only for hand-written regression IR and is recorded as a warning. Producer
metadata is an input-contract consistency check, not cryptographic provenance;
an adversarial producer can forge module metadata and is outside this trust
boundary.

Preflight walks direct reachable callees through pointer casts. Unsupported
external declarations, unresolved indirect calls, inline assembly,
atomics/fences, fixed/scalable vectors, floating point, exception-handling
opcodes, freeze/poison/undef and every poison-generating annotation reported by
LLVM (including disjoint, samesign, nneg and all GEP flags), non-default address
spaces, unknown intrinsics and unsupported calling conventions produce
`UNKNOWN` with exit 2. Opcode and intrinsic dispatch is an explicit manifest in
`docs/SUPPORT_MATRIX.md`; unknown entries do not fall through to VALIDATED.
The same vector, floating-point and address-space type gates inspect every node
of nested constant trees, not only an instruction's immediate operand type.
Because LLVM pointers are opaque, these gates also inspect every `alloca`
allocated type and every instruction/ConstantExpr GEP source element type.
Calling-convention validation applies independently to every reachable function
definition and every `CallBase`, so a non-C call site cannot bypass the gate by
targeting a C-convention definition.
These cases are distinguished from
malformed or invalid verification tasks and cannot become `SAFE`. Dynamic
global constructors remain invalid under the current initialization contract.
Debug compile-unit metadata is advisory and produces a warning when absent.

Registration builtins are checked only for ABI shape and constant printable
ASCII names. This does not prove registration-before-read, alias safety,
descriptor consistency, lifetime safety, or output capture. Those properties
belong to M4/M5 and remain `UNKNOWN` until an executor exists.

LLVM ABI-affecting attributes such as `byval`, `sret`, `inalloca`,
`preallocated` and `noalias` are currently checked only for structural validity
by LLVM's verifier. M0 does not interpret or certify their runtime meaning;
they remain `UNKNOWN` and belong to the M7 call model. A future executor must
gate or model them before producing any `SAFE` result.

Function, basic-block, instruction, constant-node/payload,
constant-graph-depth, global-initializer-byte, integer-width, aggregate-depth,
iterative type-work, metadata node/string/depth, debug-record, `llvm.ident`
entry/string and
canonical textual-IR byte limits are configurable and checked over the whole
module before producer strings are copied or an input digest is emitted.
`ValueAsMetadata`, including `ConstantAsMetadata`, contributes its underlying
Value type and constant graph to those same whole-module budgets. Constant-node
types, `alloca` allocated types and instruction/ConstantExpr GEP
source element types all contribute to the integer-width, aggregate-depth and
type-work budgets. Type, constant and metadata graphs use iterative walkers.
Metadata strongly connected
components are computed iteratively and condensed before depth analysis, so
legal self-referential loop/debug metadata is one component rather than
infinite nesting. Greatest depth is then computed over the condensation DAG, so
visiting a shared node first through a shallow named-metadata root cannot hide a
later deep acyclic path. Every scheduled metadata edge consumes a cumulative
work budget, including duplicate edges in a shared DAG. Constant and metadata
depth are bounded before LLVM's module printer or artifact hash traverses the
graph.
Exceeding a limit is an
`INVALID_TASK` result. The pass does not mutate IR and returns
`PreservedAnalyses::all()` on its successful path.

## Result and failure discipline

All M0 artifacts are written beneath the single `-formal-output-dir` option.
No legacy output alias is accepted, avoiding ambiguous artifact destinations.
The output path must be non-empty, must name a directory, and must not contain
parent-directory (`..`) components; these are configuration errors with exit 3.
Each invocation atomically creates a generated
`<output>/runs/run-<id>/` directory. Only the `result.json` inside that exact
run directory is authoritative; a legacy `<output>/result.json` is never read,
overwritten or associated with a new run. The run-local result is written to a
generated temporary file and atomically renamed. Concurrent invocations sharing
an output root receive distinct directories. A failed invocation therefore
cannot expose another run's result as its own. Consumers must retain the run ID
printed by the pass and verify the matching JSON `run_id`, host identity and
`input_module_sha256`. Input-contract failures write `INVALID_TASK` and exit 3.
Any failure after static output-path validation while creating `runs/` or the
unique run directory is an artifact I/O `INTERNAL_ERROR`, as are result write
and rename failures; all exit with code 4.
Unsupported semantics write `UNKNOWN` and exit 2.

Normal return from the pass leaves `opt` exit code 0 because the pass operation
succeeded. This is not the production `formal-check` SAFE exit code: M0 JSON
remains `status: VALIDATED` and `verdict: null`.

The JSON currently records schema/run identity, phase, status/reason/message,
entry, plugin LLVM identity, verified host-opt path/hash/version/revision,
canonical input-IR SHA-256 when resource validation completed, producer, target
triple, DataLayout, warnings and a null verdict.
It does not yet record normalization flags, target CPU/features, environment
model, resource usage, solver versions or dependency revisions; those fields
must be added before a semantic release.

## M1 expression semantics

Every expression has exactly one explicit sort:

- `Bool` for logical conditions and comparison results;
- `BitVector(width)` for nonzero-width modular bitvectors;
- `Array(BitVector(indexWidth), BitVector(8))` for byte arrays.

There is no implicit conversion between Bool and BV1. `toBool` accepts only a
Bool or BV1 and represents BV1 truth as equality with one. Constants are
`llvm::APInt`, so folding preserves arbitrary fixed widths, including widths
larger than a host word. Constant convenience APIs validate the requested
width against the context limit before constructing an `APInt`, preventing an
over-limit request from allocating first. Arithmetic and shifts use modular SMT bitvector
semantics. The M1 AST does not encode LLVM poison or UB preconditions; an LLVM
executor must handle division by zero, overshifts and instruction flags before
constructing ordinary Expr nodes.

Nodes, arrays and updates are immutable and owned by one `ExprContext`.
Cross-context composition is rejected. Hash-consing keys include kind, sort,
constant value, name, extract offset, operands, array root and update head.
Thus identical structures in one context share identity while distinct
structures remain distinct. SMT declarations are sorted by name, so output is
independent of unordered-map iteration and construction order.

`UpdateList` is persistent: writes prepend immutable nodes and preserve old
versions. `read(write(A,i,v),i)` returns `v`. A concrete read skips concrete
writes at provably unequal indices; symbolic potentially-aliasing updates stay
in order. The SMT writer emits the oldest-to-newest nested `store` sequence.

## Simplification ledger

Simplification is deliberately local and bounded. Implemented rules are:

- full constant folding where the APInt operation is defined by the layer;
- `x + 0`, `x - 0`, `x - x`, multiplication by zero/one;
- bitwise zero/all-ones identities and `x op x` for And/Or/Xor;
- `not(not(x))` and constant Not;
- constant Select guards and `ite(c,x,x) = x`;
- full-width Extract and no-op extensions/truncations;
- direct array read-after-write and skipping concrete unequal writes.

Divide/rem by zero and signed `INT_MIN / -1` are deliberately not constant
folded; they remain explicit nodes for the later LLVM UB policy. No global
rewriting, reassociation, solver-aided simplification or LLVM-level
UB reasoning occurs. Configurable maximum bit width, AST depth, node count and
update count bound construction. SMT serialization separately bounds collection
work, expanded nodes and output bytes. Expr construction additionally limits
the number of Array roots. Expr/Array/UpdateList shared references remain safe
after their creating context is destroyed, while reuse in another context is
rejected. Serialization checks every append before allocation,
stores declaration names as non-owning views, and emits persistent updates
linearly, so oversized declarations or update chains fail in a controlled way.

## SMT-LIB boundary

The writer emits stable SMT-LIB v2 using `QF_BV` or `QF_AUFBV`, explicit sorts,
sorted declarations and validated quoted ASCII identifiers. It is an oracle and
debug serialization path, not a solver backend. CTest uses the pinned external
`stp` executable only as a test-only syntax and semantics oracle; required CI
checks its locked version/revision, compiler, MiniSat profile, `ONLY_SIMPLE=OFF`,
shared/asserting Debug build profile, enabled upstream tests, and disabled
CryptoMiniSat/Python settings. No STP headers, handles or KLEE internals are linked into production
M0/M1 libraries.

## M2 handoff boundary

The required M0/M1 handoff checks are represented by the pinned Clang
`20.0.0git` C-to-bitcode test, the ASan/UBSan/LSan profile, the dependency lock,
and `containers/dev/Dockerfile`. The required workflow runs Debug, Release and
sanitizer configurations with the pinned toolchain, warnings-as-errors,
`clang-format`, and `clang-tidy`. A
local run does not substitute for a clean execution of that workflow when
cutting a release.

M2 adds a backend-neutral solver boundary with distinct SAT, UNSAT, UNKNOWN,
TIMEOUT and INTERNAL_ERROR states. `STPSolver` uses only STP's public C API and
creates a new validity checker for every query. All STP types, expressions,
counterexamples and context lifetime remain private to that query; no handle is
stored in Formal Expr or exposed to callers. Per-query Expr and Array caches
therefore cannot cross an incompatible STP context.

STP is explicitly forced to the locked MiniSat backend. CMake compiles and runs
an API/version probe that checks the exact STP tag/revision, the selected SAT
backend, and a non-host-width query before accepting the package. Ordinary
configure never downloads a dependency. The production `STPSolver` constructor
repeats the tag/revision and complete compilation-profile check against the
library selected by the runtime loader: the locked compiler is required,
`-DNDEBUG` is forbidden, and the asserting, testing-on, shared, MiniSat-only,
non-Python profile is enforced.

Fault injection is compiled only into the separate `FormalSolverTest` library
and `FormalSTPTestWorker`. The production solver library, query tools and
`FormalSTPWorker` contain no test-mode state, setter or result-mutation branch;
a build-time regression test audits their symbols and fault-injection strings.

Every SAT result extracts all scalar symbols reached by the query, all base
array elements needed by Formal reads, and every byte explicitly registered by
an `ArrayModelRequest`. The parent requires the wire scalar assignment to be an
exact name/sort-complete match for the query schema before evaluation; a model
cannot omit a symbol merely because a selected branch does not read it.
Symbols with the same name but different Expr identities are rejected before
translation, including when they come from different `ExprContext` instances.
The name-keyed STP and wire models can therefore never merge two independent
scalars into one variable. This applies even when the symbols have the same
sort and each is otherwise a valid input to a separate query.
Arbitrary-width values are recovered from STP bit
strings into `APInt`; host integer getters are not used. The independent Formal
evaluator then checks every original Bool constraint and the asserted truth
query polarity. For initial-array queries the parent additionally requires
exact request count/order/root identity/byte count and checks every returned
byte against the returned base-array model. Distinct `ArrayRef` objects with
the same name and sort are rejected as an ambiguous query instead of being
silently unified by STP. Missing/conflicting assignments, a false constraint,
malformed model data, or translation exception yields INTERNAL_ERROR with
`SOLVER_MODEL_VALIDATION_FAILED` (or the corresponding stable reason), never
UNSAT. Multiple satisfying assignments are accepted; tests validate the model
semantically instead of requiring a solver-chosen constant.
Every array model request is validated in the parent before solving and again
in the worker: null roots, byte counts above the configured limit, and counts
that wrap the index sort are `INVALID_QUERY`, even if the formula is UNSAT.

STP's MiniSat public API supports a conflict budget but not an interruptible
wall-clock timeout. Formal therefore runs every finite, nonzero-deadline query
in an isolated worker process. One overflow-safe, saturating parent-side
`steady_clock` deadline covers query preparation/serialization, fork/exec,
worker reconstruction and solving, response parsing, and mandatory parent-side
model validation. The deadline is checked before polling the worker and again
after worker completion, response reading, and final model validation; a valid
worker result that completes at or beyond the deadline is TIMEOUT and cannot be
accepted as SAT or UNSAT. After structural envelope checks, the parent
interprets worker internal failures and performs mandatory SAT model validation
before applying the final deadline classification. Malformed protocol data,
inconsistent metadata, worker internal failures, and model-validation failures
therefore retain INTERNAL_ERROR even if reading, detecting, or exporting that
corruption crosses the deadline; corruption is never downgraded to an ordinary
incomplete result. Each deadline poll first calls `waitpid(WNOHANG)`: a worker
already observed as exited is always read and classified, even when the parent
was descheduled past the deadline. If exit races with the deadline kill, a
normal `waitpid` exit is likewise read; only an actually signalled worker is
immediately classified as TIMEOUT. The parent kills and reaps a still-running
worker at the deadline without accepting a partial result. Completed in-budget results
and arbitrary-width scalar/array models are
transferred back through a size-bounded wire format derived from the exact
query schema. Array identities are numeric per-query IDs, APInt widths must
equal the requested sort, decimal payloads are digit/range checked before
construction, collection counts are bounded, and trailing bytes are rejected.
The parent then enforces a status/reason/payload matrix: for example UNSAT must
carry `SOLVER_UNSAT` and no model/value/initial bytes. Any mismatch is malformed
worker data. Wire parsing has an exception boundary, so allocation or parsing
failures also become INTERNAL_ERROR rather than terminating the verifier.

The parent first serializes a versioned, query-bounded AST/array/request record.
After `fork`, the child only installs hard core/file/address-space/CPU limits,
closes every descriptor except standard input/output/error and the request and
result descriptors, positions the two retained descriptors, and immediately
`exec`s the dedicated
`FormalSTPWorker`; allocator, STL, filesystem and STP work therefore starts in
a clean process image rather than the multithreaded parent's copied runtime.
The address-space limit is omitted in ASan builds because ASan reserves a very
large virtual address range. The response has a separate parent-side size cap.
ASan/UBSan remain active in the exec worker, and the worker returns normally
from `main`, so LSan checks SAT translation, model extraction and array paths
instead of being bypassed by `_exit`.

An arbitrary caller-supplied `AdditionalModelAudit` executes synchronously in
the parent and cannot be cancelled safely. It is therefore rejected for every
finite timeout and is available only with the explicit
`STPSolverOptions::UnlimitedTimeout` opt-out. Thus a configuration advertised
as a finite hard timeout never contains an uncancellable callback. The
unlimited mode still retains worker crash/resource isolation, but intentionally
has no wall-clock termination guarantee.

A zero deadline expires before query validation/traversal, query-schema
construction, worker creation, or STP initialization; its diagnostic artifact is
therefore intentionally minimal and does not serialize the untrusted query.
MiniSat conflict exhaustion remains a distinct STP return-code path to TIMEOUT.
A worker signal, unexpected exit code, or resource-limit termination becomes
UNKNOWN/`UNKNOWN-SOLVER-FAILURE`. Reserved normal exit codes for request
decoding failure (123), internal exception/setup failure (124), response write
failure (125), and exec failure (126) retain their numeric code in diagnostics
and become INTERNAL_ERROR/`SOLVER_BACKEND_FAILURE`. Malformed worker data and
parent-side process-control failures likewise remain INTERNAL_ERROR.
None of these outcomes can become UNSAT or SAFE.

UNKNOWN, TIMEOUT, backend/translation failures and model-validation failures
are never cached as UNSAT (M2 implements no result cache). Nonzero-deadline
failures are exported atomically as standalone SMT-LIB containing declarations,
original constraints, query target/polarity, STP/SAT versions, options, timeout
and a machine-readable reason code. For initial-array queries the artifact
also declares requested roots and records their ordered names, index widths,
and byte counts, including roots that the formula does not read. A
zero-deadline TIMEOUT intentionally emits
only metadata and an empty query because traversing the untrusted query would
violate the immediate-timeout contract. An artifact-write failure remains an
internal diagnostic failure and is reported without changing a solver failure
into a verification verdict.

M2 does not reinterpret M0 `VALIDATED` as `SAFE` and implements no M3
instruction semantics.
