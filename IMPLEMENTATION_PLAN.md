# LLVM IR Symbolic Execution Pass: Implementation Plan

## Goal

Implement an LLVM new-pass-manager plugin that selects a user-defined top-level
function, symbolically executes reachable LLVM IR, and exports an SMT model
relating registered input memory regions to registered output regions.

Pipeline:

```text
C/C++ -> LLVM bitcode -> normalization -> symbolic executor -> Expr AST -> STP/SMT-LIB
```

The Pass is the front end and coordinator. The symbolic executor is a separate
library. Dom/PostDom and CUDD are optional later optimizations for state merging
and Boolean control-flow guards; they are not the main constraint representation.

Keep the `opt` plugin for integration and development, but also build a thin
standalone `formal-check` driver over the same executor library. Symbolic
execution is long-running and emits multiple artifacts; a dedicated driver can
provide stable exit codes, timeout handling, batch execution, and diagnostics
without relying on `opt` process behavior. The plugin and driver must invoke the
same public executor API and pass the same semantic regression suite.

## Version 1 Contract

Support a fixed LLVM version and IR compiled at `-O0 -g`; a `void ()` top-level
function; integer bitvectors; direct defined calls; globals, `alloca`, heap,
arrays, structs, GEP, load/store, `memcpy`, and `memset`; bounded loops and
recursion; and registered input/output regions.

Return `UNSAFE` with a concrete model, `SAFE` only when every path completed,
and `UNKNOWN` for a bound hit or unsupported operation. Start with single-threaded
programs. Exceptions, atomics, inline assembly, unresolved indirect calls, and
unknown external functions must initially yield `UNKNOWN`, never a silent proof.

Pin one LLVM major version in CI and reject bitcode produced by incompatible
toolchains. Record `clang --version`, `opt --version`, target triple, and
`DataLayout` in every result artifact. Run LLVM's verifier before analysis and
after every normalization pipeline; malformed IR is an analysis error.

Define the result lattice precisely:

```text
UNSAFE   a modeled error is reachable and its solver model is replayable
SAFE     all reachable states terminated normally and every query was complete
UNKNOWN  any path hit a limit, unsupported semantic, timeout, or solver unknown
```

`UNSAFE` takes precedence over `UNKNOWN`; otherwise any incomplete path prevents
`SAFE`. Version 1 reports reachable LLVM undefined behavior as `UNSAFE-UB`.
Unsupported poison/undef behavior must produce `UNKNOWN`, not an unqualified
fresh symbolic value.

Aggregate final status with this order:

```text
INTERNAL_ERROR / INVALID_TASK  tool/input contract failed; no verification verdict
UNSAFE or UNSAFE-UB            at least one satisfiable modeled error path exists
UNKNOWN                        no error found, but at least one path is incomplete
SAFE                           all feasible paths complete and no error is reachable
```

An internal error does not become `UNKNOWN`; it invalidates the run. Preserve
already found counterexamples as diagnostics, but do not issue an overall formal
verdict from a corrupted execution. Add property-selection options so users can
choose whether UB, bounds, lifetime, divide-by-zero, and assertions are checked;
disabled properties and their semantic consequences are recorded in the report.

## Dependency Strategy

Dependencies must be classified as production, optional backend, test-only, or
deferred. Pin exact versions in a lock file or container image; do not build
against whichever LLVM or solver happens to be installed first on the host.

| Component | Role | Decision | Reason |
|---|---|---|---|
| LLVM/Clang | IR parser, analyses, `DataLayout`, pass API | Required and version-pinned | LLVM C++ APIs and bitcode compatibility change across major versions |
| STP | Primary QF_BV + array solver | Required from M2 onward | Direct match for byte arrays and fixed-width arithmetic |
| Z3 | Independent solver backend/oracle | Optional in production, recommended in CI | Cross-solver disagreement exposes translation and solver integration bugs |
| KLEE | Architecture reference and differential oracle | Test/reference only; do not link KLEE internals in V1 | KLEE Expr, Solver, Memory, and Executor are coupled; embedding them largely turns this project into a KLEE fork |
| CUDD | BDD control-flow guards | Deferred optional optimization | Useful only after baseline merging is correct and measured |
| Alive2 | LLVM UB and refinement oracle | Test-only | Valuable for instruction/normalization validation; not an interprocedural production executor |
| GoogleTest | C++ unit tests | Test dependency | Fits LLVM-style support-library tests |
| lit/FileCheck | IR and command regression tests | Test dependency | Native LLVM regression-test workflow |
| RapidCheck/libFuzzer | Property and fuzz testing | Optional test dependency | Finds AST, parser, and translation corner cases |
| ASan/UBSan/LSan | Implementation runtime checking | CI dependency | Detects errors in the verifier itself |

Use LLVM facilities already required by the project where they fit: `APInt`,
ADT containers, `Expected/Error`, `raw_ostream`, `llvm::json`, `llvm::cl`,
debug metadata, and testing helpers. Do not add `nlohmann/json`, CLI11, Boost,
or a second arbitrary-precision integer library without a demonstrated gap.
Prefer a small owned persistent-memory structure over importing all of KLEE for
one data type.

### KLEE decision

Use KLEE as a semantic reference for `Array`/`UpdateList`, `MemoryObject`,
`ObjectState`, copy-on-write `AddressSpace`, solver chains, query caching, and
counterexample replay. Do not copy source files casually: first record KLEE's
license obligations and preserve notices for any intentionally derived code.

Do not make KLEE a required library dependency in version 1. Its internal API is
not a stable standalone SDK, it constrains supported LLVM versions, and its
object ownership and expression types would leak through this project's public
interfaces. If reuse is later judged preferable to independence, make that an
explicit architectural fork decision: either build a KLEE extension, or own a
separate executor. Avoid an undocumented half-KLEE design.

### STP and Z3 decisions

STP is the first required concrete backend. Use its supported public C/C++ API;
do not construct or reimplement STP internal AST classes. Keep all STP handles
inside `STPSolver` and translate from the solver-independent Formal Expr AST.

Implement a Z3 backend after STP correctness, before heavy optimization. It is
not required for the first vertical slice, but CI should run a representative
solver-consistency suite through both. On disagreement:

```text
SAT vs UNSAT       internal error; preserve query and stop the affected result
SAT vs UNKNOWN     result may follow SAT, but record the disagreement
UNSAT vs UNKNOWN   report UNKNOWN unless a trusted backend policy says otherwise
different models  acceptable if both satisfy the same query
```

Export every failed or disputed query as standalone SMT-LIB with declarations,
assertions, query, solver versions, random seeds, and timeout settings.

### Build options and reproducibility

Expose explicit CMake options:

```text
FORMAL_ENABLE_STP=ON
FORMAL_ENABLE_Z3=OFF
FORMAL_ENABLE_CUDD=OFF
FORMAL_ENABLE_KLEE_DIFF=OFF
FORMAL_ENABLE_ALIVE2_TESTS=OFF
FORMAL_ENABLE_SANITIZERS=OFF
FORMAL_ENABLE_FUZZING=OFF
FORMAL_BUILD_TESTS=ON
```

Prefer `find_package(LLVM CONFIG REQUIRED)` and imported targets for solvers.
Reject an LLVM major-version mismatch at configure time. Do not use network
fetching during ordinary builds; provide a documented container/lock manifest
for reproducible dependency installation. Record dependency versions and Git
revisions in `formal-check --version` and every JSON report.

Add these repository files:

```text
cmake/Dependencies.cmake       package discovery and feature checks
cmake/FormalSanitizers.cmake   ASan/UBSan/LSan build profile
dependencies.lock.yaml        exact versions, revisions, hashes, build options
containers/dev/Dockerfile      reproducible developer/CI toolchain
THIRD_PARTY_NOTICES.md         licenses and copied/derived-code notices
docs/SUPPORT_MATRIX.md
docs/SOUNDNESS.md
```

Recommended Linux solver profile for the initial bitvector/array scope:

```text
STP: pinned release/commit, shared library, assertions in Debug,
     one pinned SAT backend (prefer the upstream-recommended CaDiCaL profile),
     floating-point support disabled until LLVM FP semantics are in scope
Z3:  pinned stable release, C++ API, CI cross-check build only at first
KLEE/Alive2: pinned external executables in the extended-test container;
             never linked into FormalPass production binaries
```

The lock manifest records transitive STP choices, including SAT backend and
allocator, because they affect behavior and performance. Run upstream STP tests
when producing the dependency image. Do not mix headers from one installation
with a library from another; CMake configure checks should compile and execute a
small version/API probe.

Choose one dependency provisioning path per supported environment:

```text
CI/release       pinned container built from verified source archives/commits
developer        same container, or documented system packages matching lock
offline          pre-fetched archive bundle with checksums
```

Avoid Git submodules for LLVM/KLEE unless the repository intends to maintain
those sources directly. Do not use CMake `FetchContent` implicitly during a
normal configure, because it weakens reproducibility and breaks offline builds.

Supported CI configurations should initially be:

```text
required: Linux x86_64, pinned Clang/LLVM, STP, Debug + Release
required: Linux x86_64, ASan+UBSan, STP
recommended: Linux x86_64, STP+Z3 cross-check
optional: second LLVM major during an explicit upgrade branch
```

Avoid promising Windows/macOS support until dynamic plugin loading, STP
packaging, and replay tests pass there.

## Semantic Support Matrix

Maintain `docs/SUPPORT_MATRIX.md` as an executable contract. Each row has one
of `SUPPORTED`, `REJECTED`, `UNKNOWN`, or `EXPERIMENTAL`, an owner milestone,
and at least one test. Include:

```text
integer and pointer widths; vectors; floating point; aggregates
each LLVM opcode and intrinsic
globals, TLS, stack, heap, address spaces, function pointers
calling conventions and ABI attributes
exceptions, RTTI, virtual calls, atomics, threads, inline assembly
undef, poison, freeze, lifetime, and arithmetic/GEP flags
external and C/C++ runtime function models
```

Version 1 C++ support means LLVM IR generated for ordinary single-threaded
code with direct calls, POD/trivially represented objects, arrays, structs,
and explicitly modeled allocation/runtime calls. It does not imply full C++
language support. Constructors/destructors are ordinary calls when bodies are
available; virtual dispatch, exceptions, RTTI, function-local static guards,
standard-library containers, and complex ABI paths require separate support or
produce `UNKNOWN`.

## User API

Create `runtime/formal_api.h`:

```c
#pragma once
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

__attribute__((noinline, used))
void __formal_register_input(void *addr, size_t size, const char *name);
__attribute__((noinline, used))
void __formal_register_output(void *addr, size_t size, const char *name);
__attribute__((noinline, used))
void __formal_assume(int condition);

#define FORMAL_INPUT(x) __formal_register_input(&(x), sizeof(x), #x)
#define FORMAL_OUTPUT(x) __formal_register_output(&(x), sizeof(x), #x)
#define FORMAL_INPUT_BUF(ptr, size, name) __formal_register_input((ptr), (size), (name))
#define FORMAL_OUTPUT_BUF(ptr, size, name) __formal_register_output((ptr), (size), (name))

#ifdef __cplusplus
}
#endif
```

Example program:

```c
#include "formal_api.h"
#include <stdint.h>

struct Result { uint32_t sum; int valid; };

void formal_top(void) {
  uint32_t a, b;
  struct Result out;
  FORMAL_INPUT(a);
  FORMAL_INPUT(b);
  FORMAL_OUTPUT(out);
  __formal_assume(a <= 100000 && b <= 100000);
  out.sum = a + b;
  out.valid = out.sum >= a;
}
```

The executor recognizes these calls as builtins. Inputs must be registered
before their first read. Outputs are read when the top-level function returns,
before its stack frame is destroyed. A buffer with symbolic length needs an
explicit range assumption, such as `__formal_assume(len <= sizeof(buf))`.

Provide two runtime implementations with identical exported symbols:

```text
formal_marker_runtime       optional no-op/trap stubs for ordinary linking
formal_replay_runtime       loads named input bytes from counterexample.json,
                            captures outputs, enforces assume, and calls entry
```

The replay runtime initializes bytes at `__formal_register_input`; this avoids
native evaluation of uninitialized input objects. `__formal_assume(false)` marks
that replay as outside the analyzed domain. The replay executable uses a small
generated `main` wrapper to call the configured top function and compares
captured outputs/errors with the result artifact. Never claim replay by merely
checking that a model satisfies SMT; run the native instrumented binary.

Registration describes bytes, not C++ types. `sizeof(x)` intentionally includes
structure padding. Type/field names from debug information are an optional
presentation layer and must never determine execution semantics. Require names
to be compile-time constant, valid UTF-8/ASCII according to the chosen report
format, globally unique after namespace qualification, and stable across paths.

Define these edge cases before implementation:

```text
size == 0                    legal no-op registration or deterministic rejection
symbolic size                UNKNOWN in V1; bounded symbolic regions are later
overlapping inputs           reject unless they denote exactly the same region
overlapping outputs          legal only with explicit alias metadata in JSON
input and output same bytes  legal in-place transformation
path-varying registration    UNKNOWN in V1; normal returns require same descriptors
dead object at capture       UNSAFE lifetime error or UNKNOWN by documented policy
```

Registration descriptors are identified by `(kind, name, size, registration
ordinal)`. All feasible normal returns must agree on this descriptor sequence;
the underlying object/offset may be path-dependent only if pointer resolution
enumerates a complete finite set and the output relation guards each alternative.
`__formal_assume(condition)` treats any nonzero integer as true, using the
target-width value produced by Clang; the executor adds `condition != 0`.

The initial header is supported only with the documented `-O0` pipeline. Before
supporting optimized IR, add an early marker-canonicalization pass that replaces
recognized calls with internal executor descriptors while preserving operands,
debug locations, and ordering. Test that DCE, inlining, and memory optimization
cannot erase or change registration semantics.

At preflight, conservatively verify that an input registration dominates every
reachable read of its registered bytes. This is an alias-aware memory property,
not merely textual instruction order: reads may occur through GEPs, bitcasts,
callee pointer arguments, or memory intrinsics. If dominance/alias analysis
cannot prove registration-before-read, either let dynamic execution report an
uninitialized read on the affected path or reject the task; never retroactively
symbolize bytes already consumed.

## Input IR and Normalization Contract

The supported input is a whole LLVM module whenever possible, not a single
function copied out of its module. This preserves globals, initializers,
`DataLayout`, target triple, aliases, runtime declarations, and reachable
callees. Version 1 requires all analyzed function bodies to use the same module
and target layout.

Compile with reproducible flags:

```bash
clang -O0 -g -fno-discard-value-names \
  -fno-exceptions -fno-rtti \
  -emit-llvm -c subject.cpp -o subject.bc
```

Use `-Xclang -disable-O0-optnone` only when a tested normalization pass requires
it. Do not use `-fwrapv` or `-fno-strict-aliasing` implicitly; such flags alter
the source-to-IR contract and must be recorded when selected. For C++, document
the target ABI and standard library, even when no library calls are expected.

Preflight checks must validate:

```text
bitcode/LLVM compatibility; target triple; non-empty DataLayout
entry existence/linkage/signature; reachable declarations
unsupported calling conventions, address spaces, scalable vectors, atomics
registration call shape and constant names
debug information availability (warning only unless source replay requires it)
dynamic global initialization requirements (`llvm.global_ctors`)
```

Build a reachable call graph from the entry, but account for aliases and
bitcasts around direct callees. Do not treat an unresolved indirect call as
unreachable. Classify it as an incompleteness point unless a sound target-set
analysis supplies all possible callees.

Invoking an arbitrary top function does not automatically execute C++ dynamic
global constructors. Version 1 should reject a reachable dependency on
dynamically initialized globals unless `--run-global-ctors` is enabled and the
executor models the ordered `llvm.global_ctors` list before the top frame.
Destructor execution is not part of a normal top-function return unless the
verification contract explicitly requests program-exit semantics. Record the
chosen initialization/environment contract in JSON.

## Loop Contract

Use a Clang-recognized annotation first:

```c
#pragma clang loop unroll_count(16)
for (int i = 0; i < n; ++i) body(i);
```

Read the emitted `llvm.loop.unroll.count` metadata as an execution bound. Do
not physically unroll IR by default. Version 1 defines the bound precisely as
the maximum number of CFG backedge traversals per dynamic loop activation. Just
before taking a backedge that would exceed the bound, suppress that successor
and record `UNKNOWN-LOOP-BOUND`. Do not execute instructions after the rejected
backedge. Provide:

```text
-formal-loop-bound=32
-formal-loop-limit-policy=unknown|error|assume-exit
```

A custom `#pragma formal loop_bound N` requires a separate Clang pragma plugin,
because a normal LLVM Pass cannot see discarded unknown pragmas.

Treat `llvm.loop.unroll.count` as a temporary transport convention: it is an
optimization hint, so another pipeline may consume or rewrite it. Run formal
execution before loop-unroll transforms and test that Clang emitted the expected
metadata. The long-term interface should emit custom `formal.loop.bound`
metadata from a small Clang frontend plugin.

A backedge bound is intentionally an IR-level unwinding measure and may differ
from the number of source loop-body executions after rotation or other CFG
changes. Reports must say `backedge bound`, not silently call it a source
iteration count. If the product requires exactly N source iterations, the Clang
plugin must mark a canonical iteration-entry edge and the executor must count
that marker; add separate tests before exposing that user-facing meaning.

## Source Layout

```text
include/formal/
  Expr.h              immutable bitvector/array AST
  Value.h             BV, pointer, aggregate, undef/poison symbolic values
  Constraints.h       path constraints and query cache
  Memory.h            MemoryObject, ObjectState, AddressSpace
  State.h             PC, stack frame, loop counters
  Solver.h            backend-neutral solver interface
  Executor.h          scheduler and execution context
  Registration.h      registration descriptors and builtins
  Result.h            verdict lattice and incompleteness reasons
lib/
  FormalPass.cpp      plugin registration and module pass
  Normalization.cpp
  Expr.cpp
  Constraints.cpp
  Memory.cpp
  State.cpp
  InstructionExecutor.cpp
  CallExecutor.cpp
  Registration.cpp
  Result.cpp
  STPSolver.cpp
  SMTLibWriter.cpp
  ModelWriter.cpp
tools/formal-check/
  FormalCheck.cpp      standalone production driver over the executor library
runtime/formal_api.h
runtime/formal_marker_runtime.c
runtime/formal_replay_runtime.cpp
tests/
  unit/
  ir/
  e2e/
  replay/
  differential/
```

## Core Design

### Expressions and solving

Represent constraints as immutable, hash-consed ASTs, not SMT strings. The
minimum nodes are:

```text
Constant, Concat, Extract, ZExt, SExt, Trunc,
Add, Sub, Mul, UDiv, SDiv, URem, SRem,
And, Or, Xor, Shl, LShr, AShr,
Eq, Ult, Ule, Slt, Sle, Not, Select,
Array, Read, UpdateList (or Write)
```

Every node has an explicit sort and every bitvector node has an explicit width.
A branch forks states with `cond` and `Not(cond)` added to their
`ConstraintManager`. Translate Expr to STP only for queries; always keep an
SMT-LIB writer for debugging and regression tests.

Use explicit sorts:

```text
Bool
BitVector(width)
Array(domain=BitVector(indexWidth), range=BitVector(8))
```

LLVM `i1` remains a one-bit bitvector in `SymbolicValue`; branch/select
conditions convert it to SMT `Bool` with `value == 1`. Comparisons return the
documented internal Boolean or `i1` representation through one centralized
conversion. Never rely on backend-specific implicit coercion between `Bool`
and `(_ BitVec 1)`.

`Array` has an explicit domain width and an 8-bit range for byte memory.
`UpdateList` is persistent and copy-on-write, so forked states share unchanged
writes. The executor owns this AST; do not implement STP's internal AST.
`STPSolver` only translates Formal Expr nodes into STP API objects and caches
the translated expressions and arrays.

The expression/value layer must define a policy for `undef`, `poison`, and
`freeze`, even if version 1 returns `UNKNOWN` when their exact semantics matter.
They are not interchangeable: separate `undef` uses may differ, poison
propagates, and one `freeze` result is stable across all of its uses.

Canonicalization must be sound and bounded. Use LLVM `APInt` for constants and
width-sensitive folding. Keep rewrites local initially; every nontrivial rewrite
needs either a truth-table/property test or an Alive2/solver equivalence test.
Track AST depth and node count to prevent recursive translation stack overflow
and simplifier blow-up.

### Solver interface and query discipline

Use semantic queries rather than exposing backend handles:

```text
check(constraints) -> SAT | UNSAT | UNKNOWN
mayBeTrue(constraints, expr)
mustBeTrue(constraints, expr)
getValue(constraints, expr)
getInitialValues(constraints, arrays)
```

Each result carries backend name/version, elapsed time, timeout reason, and
query ID. Validate every SAT model by evaluating all asserted Formal Expr nodes
under the returned assignment. If validation fails, classify it as an internal
solver/translation error and preserve the query. Never convert solver exceptions
or timeouts into UNSAT.

Adopt a KLEE-like decorator chain without depending on KLEE code:

```text
Logging -> Timing -> Validating -> CounterexampleCache -> Independence -> Core
```

Implement only Logging, Timing, Validating, and Core in M2. Add counterexample
caching and constraint independence only after profiling and semantic tests.
Cache keys include the normalized full query, target expression, backend,
backend version, relevant options, and timeout policy. Do not reuse an UNSAT
answer from a timed-out or incomplete query.

### Memory

Use object-based byte memory:

```text
MemoryObject: id, logical base, size, alignment, Global|Stack|Heap, live
ObjectState: bytes = Array[BitVec(indexWidth) -> BitVec(8)]
AddressSpace: MemoryObject -> ObjectState
```

The address space is shared by all call frames. SSA bindings are frame-local and
map LLVM `Value*` to a tagged `SymbolicValue`, not only `Expr`:

```text
SymbolicValue = BVExpr | PointerValue | AggregateValue | UndefValue | PoisonValue
```

Pointer resolution produces one or more live objects plus a byte offset;
multiple feasible objects fork the state with bounds constraints. Aggregate
values may be represented recursively or immediately packed according to a
single documented lowering policy; do not reinterpret them as integers without
`DataLayout`.

Retain pointer provenance internally as `(object identity, offset Expr)` or a
set of alternatives. A logical bitvector address may be exported to SMT, but
must not be the only pointer representation. Specify null, one-past pointers,
pointer comparisons, `ptrtoint`/`inttoptr`, address spaces, and escaped stack
addresses. Return `UNKNOWN` where provenance cannot be preserved soundly.

Structures remain byte arrays. Use `DataLayout` and `StructLayout` to compute
GEP offsets, alignment, padding, and endian-correct load/store packing. For
opaque pointers use `GEP.getSourceElementType()`, not a pointer pointee type.
Respect `inbounds` GEP and arithmetic flags such as `nsw`, `nuw`, and `exact`:
violations produce poison and may later trigger UB. Handle
`llvm.lifetime.start/end`; stack object lifetime is not always identical to
function entry and return.

Object allocation rules must be deterministic and explicit:

```text
global size           concrete from DataLayout
alloca count          concrete in V1; bounded symbolic allocation is later
malloc/new size       concrete in V1; symbolic size returns UNKNOWN initially
allocation failure    branch success/null only if failure modeling is enabled
zero-size allocation  unique/non-unique policy documented and tested
realloc               preserves min(old,new) bytes; old object semantics split
```

Separate allocation identity from numeric address. A deterministic allocator can
assign non-overlapping logical bases for SMT export, but object provenance is
authoritative. Use arbitrary-precision checked host arithmetic when calculating
object ranges so verifier-side overflow cannot create an unsound bounds check.

Memory reads need initialization metadata per byte or bit. Distinguish concrete
initialized, symbolic initialized, uninitialized/undef, and poison-containing
bytes. Structure padding is part of memory and affects bytewise operations, but
reading indeterminate padding as a typed value must follow LLVM semantics.

Use `DataLayout::getTypeStoreSize()` for memory transfer width and
`getTypeAllocSize()` for object/element allocation strides; do not derive either
from scalar bit width. Reject scalable `TypeSize` in V1. Test non-byte integer
widths such as `i1`, `i24`, and `i65`, because register width, store size, and
allocation size are different concepts.

Byte arrays alone do not preserve pointer provenance when pointers are stored in
memory. Add shadow pointer metadata to `ObjectState`:

```text
PointerShadowEntry
  byte range and alignment
  pointer width/address space
  PointerValue alternatives: (MemoryObject identity, offset Expr)
  numeric representation Expr used for bytewise observation
```

A full-width pointer store writes its numeric bytes and installs a shadow entry.
A compatible full pointer load restores the `PointerValue`. Any overlapping
ordinary byte/integer write invalidates affected shadow entries. `memcpy` and
`memmove` propagate a shadow entry only when the complete pointer representation
is copied without incompatible rewriting; otherwise later dereference returns
`UNKNOWN` unless a sound numeric-to-provenance reconstruction policy applies.
Test pointer round trips through stack slots because unoptimized Clang emits
them routinely.

Raw registration of a struct containing pointer fields does not automatically
make those pointers range over valid objects. In V1, pointer fields must be
initialized to concrete modeled objects before registration, or remain output-
only. Dereferencing arbitrary symbolic pointer bytes returns `UNKNOWN`. A future
typed API may register a pointer's finite target set explicitly; do not infer
targets from C++ debug types.

### Calls

For a defined direct call, push a frame, bind arguments, execute callee IR,
then bind the returned Expr at the caller call site. Destroy callee `alloca`
objects on return. Implement builtin models for `malloc`, `calloc`, `realloc`,
`free`, `memcpy`, `memmove`, `memset`, `assert`, `abort`, and registration calls.
Unknown externals and indirect calls return `UNKNOWN` in version 1.

External models implement a common contract:

```text
match signature and attributes
check preconditions and pointer ranges
return Expr/value
declare exact read/write/free/allocate effects
emit error states and incompleteness reasons
```

Test models against native implementations where their behavior is portable.
For `malloc` family and C++ `new/delete`, identify the actual symbols emitted by
the pinned target ABI, including mangled operator names and exception-disabled
failure behavior. `memcpy` overlap is UB; `memmove` overlap is valid. Model
`llvm.memcpy.*`, `llvm.memmove.*`, and `llvm.memset.*` intrinsics before relying
on libc names.

Normalization must use an explicit allow-list. Start with:

```text
Verifier -> restricted SimplifyCFG -> optional LowerSwitch -> Verifier
```

Do not enable `mem2reg`, SROA, InstCombine, loop unrolling, or UB-exploiting
optimizations until differential tests show that registration calls, object
identity, debug locations, and loop metadata survive. Normalization must not
hide missing executor semantics.

### Execution, search, and result construction

Keep execution deterministic by default. Assign stable IDs to instructions,
states, objects, symbolic arrays, queries, and errors. A worklist strategy is a
pluggable component:

```text
DFS                 simplest baseline and low memory
BFS                 useful for shallow counterexamples, high memory
coverage-new         prioritize states reaching uncovered blocks/edges
random-path          optional, always record seed
```

Start with DFS and expose state/time/query/memory limits. Limits affect
completeness, never reachability already proven. Deduplicate the same error by
`(kind, source location, call stack)` while preserving at least one model.

For each completed path `p`, construct:

```text
PathRelation_p(I,O) = PC_p(I) AND O = FinalOutput_p(I)
ProgramRelation(I,O) = OR over every complete normal path
```

Error paths are exported separately. Ensure the final formula is closed except
for declared input/output symbols: internal arrays, temporary nondeterministic
values, allocator choices, and path guards must be defined, existentially
represented according to the selected SMT format, or eliminated. Emit one
manifest mapping every byte-level symbol to its registration/object/source.

When paths are incomplete, a relation over completed paths may still be useful,
but label it `UNDER_APPROXIMATION`; it is not the complete program model. Never
combine an under-approximate relation with a `SAFE` verdict.

## Correctness and Soundness Ledger

Maintain `docs/SOUNDNESS.md`. Every approximation is classified:

```text
exact              implements the supported LLVM semantics
over-approximation may add behavior; suitable for proving safety, may add alarms
under-approximation may miss behavior; useful for bugs only, blocks SAFE
unsupported        stops affected path as UNKNOWN
```

Record the policy for loops, recursion, symbolic allocation sizes, pointer
resolution caps, external calls, allocation failure, uninitialized memory,
poison, environment, and solver timeout. Each JSON report embeds the active
ledger entries. This is part of the result, not merely developer documentation.

The tool has two distinct modes:

```text
bug-finding mode   may use documented under-approximations; verdict is never SAFE
proof mode         permits only exact semantics or sound over-approximations
```

Default to proof mode for formal-model export. `assume-exit` loop handling is
therefore rejected in proof mode unless the user explicitly requests an
under-approximate model.

## Performance and Optimization Plan

Do not optimize from intuition alone. Establish a deterministic baseline corpus
and record these metrics per run:

```text
executed instructions; created/live/terminated states; forks; merges
Expr nodes by kind; maximum AST depth; UpdateList length; memory-object copies
solver queries by kind; SAT/UNSAT/UNKNOWN; cache hits; translation/solve time
pointer-resolution candidates; loop backedges by dynamic activation; wall time; peak RSS
```

Store benchmark JSON and compare medians over at least five warmed runs. Keep
correctness and performance baselines separate: correctness tests should be
small and exact; benchmarks may be larger and run nightly.

Optimize in this order:

1. **Expression construction:** hash-consing, `APInt` folding, canonical operand
   order for commutative nodes, bounded local rewrites, and balanced `Concat` /
   `And` / `Or` trees.
2. **Solver translation:** per-query backend-node caches, incremental contexts
   where backend semantics are understood, query logging, and model validation.
3. **Constraint reduction:** remove syntactic duplicates, slice constraints by
   dependency on queried arrays/variables, then add an independence solver.
4. **Counterexample cache:** reuse complete satisfying assignments only after
   validating them against the new query; cache proven UNSAT only with exact keys.
5. **Memory:** persistent `AddressSpace`, object-level copy-on-write, concrete-byte
   fast paths, compact constant objects, and read-over-write simplification.
6. **Pointer resolution:** interval indexing for concrete addresses, provenance
   candidate sets, `mayBeTrue` before forking, and explicit resolution caps.
7. **Search:** coverage-aware scheduling, state/error deduplication, and resource
   budgets. Search order may change discovery time, not semantic verdict rules.
8. **State merging:** merge only compatible states at measured join points; use
   a profitability heuristic based on changed locals/objects, guard size, and
   expected solver cost. Reject merges that cause excessive AST growth.
9. **Function summaries:** begin with pure functions, then explicit memory
   read/write footprints. Summary keys include function body hash, argument
   expressions, alias partition, relevant memory version, target layout, and
   semantic options.
10. **CUDD:** map only atomic control predicates to BDD variables and retain a
    side table from each atom to its Formal Expr. Use CUDD for Boolean guard
    canonicalization/reachability; never encode general bitvector arithmetic as
    a BDD by default.

Avoid parallel state execution until solver contexts, expression arenas, object
IDs, logging, and result ordering have explicit thread-safety rules. A safer
first concurrency step is process-level parallelism across independent modules
or test cases. Deterministic single-process mode remains mandatory for replay.

Performance changes pass three gates:

```text
semantic equivalence against baseline results
no new UNKNOWN/incomplete paths on the correctness corpus
measured improvement on a named benchmark without a major memory regression
```

A reasonable initial regression threshold is 10% for wall time and peak RSS on
stable benchmarks, with noisy tests excluded only through a documented review.

## Robustness and Security Boundaries

Treat input bitcode, registration names, debug paths, SMT models, and external
solver output as untrusted. The verifier must fail closed as `UNKNOWN` or an
input/internal error, never crash into a `SAFE` result.

Apply configurable hard limits before and during execution:

```text
module/functions/blocks/instructions
constant and global initializer size
bit width, aggregate nesting, AST depth and node count
number and size of memory objects
states, forks, queries, pointer resolutions, loop/call depth
per-query and whole-run wall time; peak resident memory; artifact size
```

Sanitize user names before using them as SMT identifiers or file names; maintain
the original name separately in JSON. Write artifacts under one validated output
directory using generated IDs, never source-provided relative paths.

An embedded solver is fast but difficult to recover from if it crashes or
ignores a timeout. Provide a worker-process solver mode for CI, untrusted
inputs, and production hard timeouts. The parent serializes an immutable,
versioned query, forks only to apply OS limits and immediately exec a dedicated
helper, and accepts only a bounded versioned response whose
status/reason/payload and model satisfy the original query schema. A SAT scalar
model must be an exact name/sort-complete match for that schema, including
symbols in evaluator-inactive Select branches. Classify signals, resource-limit
termination, and unreserved exit codes as `UNKNOWN-SOLVER-FAILURE`; reserved
worker protocol/internal exit codes must remain internal errors and retain the
numeric exit code in diagnostics. Never retry indefinitely.

Internal invariant failures should produce an internal-error artifact containing
the minimal current state/query when feasible. They must use a distinct process
exit code and cannot be downgraded to ordinary unsupported behavior or TIMEOUT,
even when detecting or exporting the corruption crosses a query deadline.
Deadline polling must observe a normally exited worker before deciding to kill;
an exit/kill race that reaps a normal exit must read and classify the complete
response instead of discarding a possible internal error as TIMEOUT.
Arbitrary caller callbacks cannot participate in a finite hard-timeout query;
they require an explicit unlimited-timeout opt-out unless moved into a safely
terminable process boundary.

## Milestone Dependency and Release Gates

The critical dependency chain is:

```text
M0 -> M1 -> M2 -> M3 -> M4 -> M5 -> M6 -> M7 -> M8 -> M9 -> M10
       AST   solver scalar memory registration aggregate calls loops UB  optimize
```

Avoid implementing later semantic layers on placeholders. In particular,
memory registration requires M4 pointer resolution; path feasibility requires
M2; state merging requires the complete result lattice and baseline executor.

Release gates:

```text
Developer preview (M3)
  scalar IR only; no claim of C/C++ memory support

Memory-model preview (M6)
  registered scalar/array/struct relations in supported straight-line/branching code

Interprocedural bounded release (M8)
  direct calls, heap, bounded loops; SAFE always includes explicit bound scope

Proof-oriented release (M9)
  supported UB/poison contract, soundness ledger, full regression/replay gates

Optimized release (M10)
  same semantics with measured optional acceleration
```

Z3 cross-check support should land after the STP vertical slice and before M10.
CUDD cannot become a required dependency unless benchmarks show net benefit and
baseline-vs-CUDD equivalence remains in required CI.

## Milestones and Acceptance Criteria

Each milestone is complete only when its implementation, negative tests,
end-to-end tests, and documented unsupported cases all pass. A build alone is
not acceptance.

### M0: Plugin scaffold and IR contract

Create a C++20 CMake project and loadable new-PM plugin. Implement
`-passes=formal-exec`, entry/output options, LLVM version diagnostics, entry
validation, and verifier invocation. The pass must return
`PreservedAnalyses::all()` if it does not mutate IR.

Acceptance: plugin loading, valid entry, missing entry, declaration-only entry,
wrong signature, malformed IR, and target `DataLayout` tests all pass.

### M1: Solver-independent Expr and array AST

Implement immutable hash-consed bitvector nodes, `Array`, persistent
`UpdateList`, `Read`, local simplification, strict width/type checks, and stable
SMT-LIB v2 output. Define AST ownership and deterministic naming.

Acceptance: unit and property tests cover every node, width boundary, constant
fold, structural sharing, symbolic array read-after-write, and malformed-node
rejection. Sanitizer tests show no leaks or use-after-free.

### M2: STP solver boundary

Implement the backend-neutral `Solver` interface and Formal Expr to STP API
translation with per-query caches. Support SAT, UNSAT, UNKNOWN, timeout, model
extraction, bitvectors, arrays, and strict query-context isolation. Incremental
push/pop is an optional backend capability, not a semantic requirement. Keep
the SMT-LIB writer as an independent oracle/debug path.

Acceptance: solver tests cover SAT/UNSAT, signed and unsigned comparisons,
overflow, array read/write, model byte extraction, timeout/UNKNOWN propagation,
and repeated queries. Where another solver is available, both backends agree.

### M3: Scalar symbolic executor

Implement execution states, frame-local SSA bindings, worklist scheduling,
constants, integer operations, casts, `icmp`, `select`, `br`, `switch`, PHI,
and return. Track the incoming CFG edge for PHI semantics. Query STP before
enqueuing a symbolic branch and retain dead-path statistics. Arithmetic with
`nsw`, `nuw`, `exact`, poison operands, or invalid shift/division conditions
must either implement the documented definedness condition or stop the affected
path as `UNKNOWN`; never erase flags. Use unsigned flag-free programs for the
first vertical slice until the corresponding semantics pass tests.

Acceptance: hand-written IR tests cover every predicate, signedness, overflow,
multi-predecessor PHIs, infeasible branch removal, switch defaults, and
unsupported instructions. Scalar functions with symbolic arguments agree with
concrete execution over exhaustive 8-bit inputs.

### M4: Object memory, pointers, and GEP

Implement globals and initializers, `alloca`, object identity plus symbolic
offset pointers, byte-array `ObjectState`, copy-on-write `AddressSpace`,
load/store, endian packing, GEP using `DataLayout`, null/bounds/alignment checks,
and symbolic pointer target forking with a configurable cap. `inbounds` GEP and
lifetime intrinsics must be modeled or explicitly gate affected paths before
ordinary Clang-generated pointer code is declared supported.

Acceptance: tests cover little and synthetic big endian layouts, global
constant expressions, one-past pointers, zero-size objects, symbolic indices,
overlapping objects, aliases, null, exact-end access, and out-of-bounds access.
Every reported memory model is replayed under ASan/UBSan when applicable.

### M5: Registration and relational model output

Add `formal_api.h`; recognize input, output, and assume builtins during
execution. Resolve each registered `(address, size)` region through the normal
memory model. Detect duplicate names, overlap, symbolic sizes, use-before-input,
dead output objects, and output aliasing. Capture top-frame outputs before
destroying the frame and emit a closed input/output relation plus JSON metadata.

Acceptance: scalar, array, in-place input/output, overlapping slice, global,
stack, and heap-region registration tests pass. For small inputs, enumerate all
values and prove that every native output satisfies the exported relation and
that the relation admits no extra output.

### M6: Structures, aggregates, and bulk memory

Implement nested structs, arrays of structs, unions as byte reinterpretation,
aggregate constants, extract/insertvalue where retained, padding-aware copies,
`memcpy`, `memmove`, and `memset`. Do not assign symbolic input bytes to padding
unless the registered byte region intentionally includes padding; document the
chosen initialization policy.

Acceptance: compare all offsets and sizes with `DataLayout`/`StructLayout`;
test packed/unpacked/nested structs, tail padding, unions, flexible-array
boundaries, overlapping `memmove`, and structure copy. Native byte dumps must
match modeled bytes for initialized regions.

### M7: Direct calls, ABI-relevant attributes, and heap

Implement direct non-inlined calls, frames, returns, shared address space,
stack object lifetime, vararg rejection, and heap builtins. Validate or model
relevant attributes including `noundef`, `nonnull`, `dereferenceable`, `byval`,
`sret`, `readonly`, and `noalias`; unsupported calling conventions return
`UNKNOWN`.

Acceptance: tests cover scalar/struct returns, pointer mutation, aliasing actual
arguments, recursion bounds, `byval`/`sret` cases generated by the pinned Clang,
malloc/calloc/realloc/free, double-free, use-after-free, and use-after-return.

### M8: Loops, bounds, and completeness accounting

Use `LoopInfo`; preferably require LoopSimplify form so each natural loop has a
single latch, or count each actual latch-to-header CFG edge according to the
documented IR-level backedge contract. Track counters by dynamic loop activation
so nested loops and repeated function calls do not share counts. Read local
bounds, apply defaults, and record exact incompleteness reasons. Preserve loop
metadata through any canonicalization and map diagnostics back to source.

Acceptance: zero-trip, one-trip, early break, continue, multiple exits, nested,
multiple-latch, irreducible-cycle, and recursive-call tests pass. A bound hit,
solver timeout, state cap, or unsupported instruction always prevents `SAFE`.

### M9: LLVM UB and special-value semantics

Implement or explicitly gate `undef`, poison propagation, `freeze`, `nsw`,
`nuw`, `exact`, shift-range rules, divide overflow/zero, `inbounds` GEP, and
`llvm.lifetime.start/end`. Separate ordinary error properties from LLVM UB in
the result format.

Acceptance: focused hand-written IR tests derived from LangRef examples match
expected `SAFE`, `UNSAFE-UB`, or `UNKNOWN`. Run Alive2-based differential checks
for small supported transformations when Alive2 is available.

### M10: Optimization and state merging

Add query caching, expression rewriting, conservative state merging at common
post-dominators, then optional CUDD guard sharing. Merge only states with the
same PC, compatible dynamic call stack, loop activation, live object set, and
solver context. Memory differences become guarded updates/read expressions.

Acceptance: baseline and optimized modes produce identical verdicts and
equivalent output relations. Dedicated benchmarks must show reduced states or
queries without unacceptable AST growth; otherwise the optimization stays off.

## Test Architecture

Use five complementary layers. No single layer is sufficient for a verifier.

```text
unit       C++ tests for Expr, solver translation, memory, and result lattice
IR         hand-written .ll tests for exact LLVM instruction semantics
e2e        C/C++ -> clang -> bitcode -> FormalPass -> JSON/SMT result
replay     execute reported concrete inputs natively with ASan/UBSan
differential compare against concrete enumeration, another solver, KLEE, or Alive2
```

Use LLVM `lit` plus `FileCheck` for plugin/IR diagnostics and GoogleTest or
Catch2 for C++ units. Use CTest as the single top-level command so local and CI
execution are identical. Tests must never depend only on debug text; assert the
machine-readable JSON status and reason code.

### Common test fixture

Each end-to-end test directory should contain:

```text
case.c or case.cpp
expected.json          verdict, reason, output names, optional path count
check_model.smt2       optional negated property expected UNSAT
replay.c               optional counterexample consumer
```

The harness performs:

```text
1. Compile with the pinned clang and flags.
2. Run opt -passes=formal-exec with deterministic seed and limits.
3. Validate JSON against expected fields.
4. Parse relation.smt2 with STP and, when available, a second solver.
5. For UNSAFE, run the native replay with generated bytes.
6. Fail if stderr contains an unclassified unsupported operation.
```

Preserve failing artifacts under `build/test-artifacts/<test-name>/`.

### M0 tests

```text
plugin-load                 pass appears and runs under opt
valid-entry                 accepts exactly void () definition
missing-entry               deterministic configuration error
declaration-entry           rejected
wrong-signature             rejected
malformed-ir                LLVM verifier rejects before execution
version-and-layout          JSON records tool versions/triple/DataLayout
preserved-analyses          no accidental IR mutation
global-ctor-preflight       rejects or schedules dynamic initialization by policy
```

### M1 tests

```text
all-node-construction       valid widths and result sorts
invalid-widths              deterministic rejection, no assertions in release
constant-folding            compare APInt results at widths 1, 8, 32, 65, 128
hash-consing                same structure shares identity; unequal does not
select-simplification       ite(c,x,x)=x and constant guards
extract-concat              boundary offsets and reconstruction
array-read-write            read(write(A,i,v),i)=v
symbolic-update             preserve update order and old-array reads
stable-naming               byte-identical SMT output across repeated runs
ownership                   ASan/LSan/UBSan clean
```

Add property tests using random ASTs. Evaluate each AST concretely under random
assignments and compare with solver evaluation. Persist every random seed.

### M2 tests

```text
sat-unsat                   contradictory and satisfiable constraint sets
signed-vs-unsigned          boundary values around the sign bit
modular-overflow            widths 1, 8, 32, 64 and non-native widths
array-model                 model extraction after symbolic writes
query-isolation             push/pop or fresh-context implementation cannot leak
translation-cache           repeated Expr produces one backend translation
timeout                     maps only to UNKNOWN, never UNSAT
cross-solver                STP agrees with SMT-LIB consumed by another solver
```

### M3 tests

Use hand-written IR so Clang lowering changes cannot mask interpreter bugs:

```text
all-icmp-predicates         eq/ne and signed/unsigned order
casts                       trunc/zext/sext/bitcast supported cases
phi                         diamond, loop-like predecessor, parallel PHIs
branch-pruning              infeasible successor is not scheduled
switch                      cases and default
division-and-shifts         normal cases plus gated UB cases
unsupported-opcode          exact UNKNOWN reason and source/IR location
```

For functions over one or two `i8` inputs, enumerate the complete input domain,
run native code, and compare reachable returns with the symbolic relation.

### M4 tests

```text
global-initializers         zero, scalar, aggregate, pointer constants
alloca-and-alias            same object via multiple GEPs
byte-pack                   i8/i16/i24/i32/i64 reads and writes
endianness                  little and synthetic big-endian modules
gep                         arrays, nested aggregates, negative index arithmetic
symbolic-index              feasible target forks and range constraints
one-past                    construct allowed pointer; reject dereference
null-and-oob                exact boundary, underflow, overflow
zero-size-and-alignment     explicit policy and diagnostics
copy-on-write               forked states do not mutate siblings
pointer-stack-roundtrip     provenance survives pointer store/load
pointer-shadow-invalidate   overlapping integer/byte store invalidates metadata
pointer-memcpy              complete copy preserves provenance; partial copy gates it
```

### M5 tests

```text
scalar-registration         input bytes reconstruct the scalar correctly
input-before-read           valid and invalid ordering
output-snapshot             captured before top-frame destruction
inout                       same region registered as input and output
slices-and-overlap          explicit supported policy
duplicate-names             deterministic rejection
symbolic-size               supported bound or UNKNOWN
dead-output                 stack/heap lifetime diagnostic
path-varying-descriptors    descriptor mismatch returns UNKNOWN
relation-closure            no undeclared internal symbols remain free
truth-table                 exact relation vs exhaustive native executions
pointer-field-input         arbitrary pointer bytes produce explicit UNKNOWN
replay-runtime              injects bytes before first native input read
```

For an exported relation `R(I,O)`, check both directions on small domains:

```text
soundness:    native(I)=O  implies R(I,O)
precision:    R(I,O)       implies O=native(I)
```

If nondeterminism is intentionally modeled, replace equality with membership in
the documented native outcome set.

Where a reference SMT relation `Expected(I,O)` is available, test logical
equivalence rather than textual equality:

```text
assert xor(Produced(I,O), Expected(I,O))
check-sat  ; must be UNSAT
```

For multi-output relations, compare the complete tuple and declared domain.
Reuse this equivalence harness for optimized vs baseline execution, state merge,
function summaries, SMT writer changes, and solver backend changes. Textual
goldens remain useful only for printer stability and human-facing diagnostics.

### M6 tests

```text
layout                      packed/unpacked/nested/array-of-struct
padding                     initialized and uninitialized padding policy
union                       byte reinterpretation and active-member caveat
aggregate-copy              load/store aggregate or lowered memcpy
memcpy                      non-overlap and invalid overlap policy
memmove                     forward/backward overlap
memset                      symbolic object with concrete byte/count
flexible-array              registration controls accessible trailing bytes
```

Generate a native layout probe using `sizeof`, `_Alignof`/`alignof`, and
`offsetof`; compare it with the model for the same target triple.

### M7 tests

```text
direct-call                 concrete and symbolic arguments
pointer-side-effect         callee store visible to caller
return-shapes               void, scalar, and Clang-generated sret
alias-arguments             p==q and p!=q paths
stack-lifetime              lifetime intrinsics and use-after-return
heap                        malloc/calloc/realloc/free success paths
heap-errors                 double free, invalid free, use-after-free
recursion                   base case and depth-bound UNKNOWN
attributes                  noundef/nonnull/byval/sret/readonly/noalias
unknown-call                UNKNOWN with callee and call-site location
```

### M8 tests

```text
zero-trip                   loop body never executes
exact-bound                 N allowed backedges complete
bound-plus-one              UNKNOWN before the disallowed backedge traversal
break-continue              counts actual IR backedges according to the contract
nested-loop                 independent dynamic counters
multiple-exit               all exit conditions retained
multiple-latch              canonicalize or count each traversed latch edge
reentered-callee            loop counters do not leak between calls
irreducible-cycle           explicit UNKNOWN unless separately supported
all-incomplete-reasons      loop/state/depth/time/query limits block SAFE
```

Before relying on a pragma, run `clang -S -emit-llvm` and FileCheck the exact
loop metadata. Include a test showing what happens if an optimization consumes
that metadata.

### M9 tests

```text
undef-use                   independent-use behavior or explicit UNKNOWN
poison-propagation          arithmetic through branch/store/return
freeze                      one stable nondeterministic value per instruction
nsw-nuw-exact               boundary violations
shift-range                 amount >= bit width
sdiv-min-negone             signed division overflow
inbounds-gep                base, one-past, and out-of-object behavior
lifetime-start-end          live/dead transitions
```

Use small LangRef-derived `.ll` files. Do not derive expectations from Clang C
programs alone because source-level UB and LLVM-level poison are different.

### M10 tests

Run every semantic test twice with optimization off/on. Compare verdict, reason,
counterexample validity, and logical equivalence of output relations. Add
benchmarks for diamonds, loops with internal branches, and differing object
writes; record states, merges, AST nodes, solver queries, cache hits, and time.

### Global quality gates

Every change must pass:

```bash
cmake --build build --config Debug
ctest --test-dir build -C Debug --output-on-failure
cmake --build build-asan --config Debug
ctest --test-dir build-asan -C Debug --output-on-failure
```

CI additionally runs Release tests, formatting, `clang-tidy` on project code,
and a clean build against the pinned LLVM. Set deterministic solver seeds and
timeouts. No flaky test retries are accepted as success.

Coverage targets are secondary to semantic cases, but use line/branch coverage
to find untested opcode dispatch and error paths. Require every supported
instruction and every `UNKNOWN` reason to appear in at least one test.

Every `UNSAFE` must have a satisfiable model and replay when the error is
source-level executable. Every `SAFE` must have zero incomplete paths. Any
unsupported operation, solver timeout, state cap, loop bound, recursion bound,
or unresolved pointer blocks `SAFE`.

## Regression and CI Program

### Test corpus ownership

Organize the permanent corpus by semantic ownership, not by the bug reporter or
date:

```text
tests/unit/                 data structures and backend adapters
tests/ir/expr/              scalar LLVM instruction semantics
tests/ir/memory/            pointer, GEP, load/store, lifetime
tests/ir/ub/                poison, undef, freeze, flags
tests/e2e/c/                Clang C lowering and registration workflows
tests/e2e/cxx/              supported C++ ABI/lowering cases
tests/models/               libc/runtime builtin models
tests/replay/               native counterexample reproduction
tests/differential/         solver, KLEE, Alive2, and enumeration harnesses
benchmarks/micro/           controlled forks, arrays, aliases, merge shapes
benchmarks/programs/        approved larger examples with licenses recorded
```

Grow external coverage in controlled layers:

```text
KLEE regression subset      shared LLVM/intrinsic semantics and error behavior
SV-COMP subset              pinned C ReachSafety/BitVectors/Arrays/MemSafety tasks
Test-Comp subset            bug-finding and generated-test effectiveness
project-specific corpus     representative customer patterns, anonymized/licensed
```

Select only tasks whose architecture, property, runtime calls, concurrency, and
language features match `SUPPORT_MATRIX.md`. Translate each external task's
nondeterministic input and error convention through an explicit adapter. Keep
the upstream revision/tag, task YAML/property, expected verdict, architecture,
license, adapter version, and exclusions in a manifest. External suites measure
coverage and scalability; they do not replace small hand-written semantic tests.

Each supported opcode, intrinsic, model, verdict reason, and approximation entry
links to at least one test from `SUPPORT_MATRIX.md`. Generate a coverage report
for this mapping in CI; a new support claim without a test is a failure.

### Regression lifecycle

For every discovered defect:

1. Preserve the original bitcode, command line, tool versions, seed, query logs,
   JSON, and replay input in an issue artifact bundle.
2. Reduce it to the smallest stable `.ll` reproducer using `llvm-reduce` with an
   interestingness script that checks the wrong verdict/model/crash.
3. Add the reduced case as a failing regression test before the fix.
4. Fix the owning semantic layer, not only the specific output string.
5. Add a nearby negative/dual test, for example signed vs unsigned or in-bounds
   vs one-past, to prevent an overfitted repair.
6. Run the milestone suite, full semantic suite, sanitizer suite, and affected
   differential tests.
7. Keep the reproducer permanently and link its test name to the issue/change.

Never update an expected result merely because current output changed. Require
a semantic justification, and for formula changes prove old/new equivalence or
document the intended behavior change.

### CI tiers

```text
pre-commit / local (<2 min)
  formatting, compile touched targets, focused unit and lit tests

pull request required (<15 min target)
  Debug+Release, all unit/IR/e2e tests, STP, deterministic replay,
  support-matrix coverage, warnings-as-errors

pull request extended
  ASan+UBSan+LSan, Z3 cross-solver sample, full native replay,
  randomized property tests with fixed rotating seed set

nightly
  long solver cross-check, KLEE differential subset, Alive2 checks,
  fuzzing corpus, all benchmarks, leak/peak-memory trends

weekly or release
  dependency vulnerability/license scan, clean container rebuild,
  larger application corpus, 24-hour fuzz campaign, artifact audit
```

Use lit features such as `REQUIRES: stp`, `REQUIRES: z3`, and
`REQUIRES: alive2` for optional tools. An unavailable optional dependency may
mark its tests unsupported, but required STP tests may not silently skip.

### Golden outputs

Golden-test only stable interfaces:

```text
JSON schema/version, verdict/reason, symbol map, deterministic IDs
canonical SMT-LIB for small focused cases
selected diagnostics with normalized paths
```

Do not golden-test wall time, raw pointer values, unordered-map iteration order,
or solver-chosen model values when multiple models are valid. Validate arbitrary
models semantically. Canonicalize source paths and sort JSON collections.

### Differential oracles

Use each oracle only within its valid overlap:

```text
concrete enumeration  strongest oracle for pure small-width supported functions
native replay         confirms a concrete source-level error, not overall safety
STP vs Z3             validates solver translation and query outcomes
KLEE                  compares path/error behavior for shared supported IR subset
Alive2                validates local LLVM semantics/normalizations, not full calls
ASan/UBSan             confirms selected runtime memory/UB counterexamples
```

KLEE disagreement does not automatically mean this tool is wrong: first compare
LLVM versions, runtime models, UB policy, search bounds, and allocation behavior.
Classify every disagreement and keep resolved cases in a differential corpus.

### Fuzzing and property testing

Add fuzz targets progressively:

```text
Expr constructor/simplifier and serializer round trip
SMT-LIB writer/parser acceptance
Formal Expr -> STP/Z3 translation consistency
LLVM bitcode/module parser plus preflight validation
single-instruction and short straight-line IR executor
memory operation sequences over small objects
```

Do not feed unconstrained huge IR directly into an unbounded executor. Limit
module size, instruction count, AST depth, allocation size, states, queries, and
time. Crashes, assertions, sanitizer reports, non-determinism, and solver
disagreements are fuzz failures. Preserve and minimize corpus inputs.

Property checks should include:

```text
serialize/parse or print stability where applicable
simplify(e) equivalent to e
read(write(A,i,v),i) equals v
state fork isolation under copy-on-write
pack(unpack(v)) equals v for supported widths/endian
SAT model satisfies every original constraint
merge(states) relation equals disjunction of unmerged states
```

### Performance regression

Keep benchmark hardware, CPU governor, solver build, thread count, and timeout
stable. Compare distributions, not one run. Store raw JSON as CI artifacts and
track at least wall time, peak RSS, state count, query count, solver time, and AST
nodes. A benchmark verdict or completeness change is a correctness failure, not
a performance fluctuation.

Separate budgets:

```text
micro benchmark     low variance; gate pull requests at a reviewed threshold
program benchmark   noisy; report on pull requests, gate sustained regressions
stress benchmark    nightly only; detect scaling cliffs and memory leaks
```

### Dependency and LLVM upgrades

Upgrade LLVM, STP, Z3, or KLEE differential versions in dedicated changes:

1. Build old and new dependency matrices from clean environments.
2. Run the complete semantic and replay suites with both.
3. Compare normalized JSON verdicts, relations, unsupported reasons, and query
   outcomes; investigate every difference.
4. Regenerate Clang-produced fixture IR intentionally, while keeping hand-written
   IR tests stable where syntax permits.
5. Update support matrix, lock/container manifest, licenses, and version output.
6. Run benchmark comparison and document expected performance changes.

Never support multiple LLVM majors through scattered preprocessor checks unless
CI continuously tests every branch. Prefer a narrow compatibility adapter layer
or one pinned major per release line.

## Build and Run

Compile a subject:

```bash
clang -O0 -g -fno-discard-value-names -emit-llvm -c subject.c -o subject.bc
```

Run the plugin:

```bash
opt -load-pass-plugin ./build/lib/FormalPass.so \
  -passes=formal-exec \
  -formal-entry=formal_top \
  -formal-solver=stp \
  -formal-output-dir=results \
  -formal-loop-bound=32 \
  -formal-loop-limit-policy=unknown \
  -formal-max-call-depth=64 \
  subject.bc -disable-output
```

Production-style invocation through the same executor library:

```bash
formal-check \
  --entry=formal_top \
  --solver=stp \
  --output-dir=results \
  --loop-bound=32 \
  --max-call-depth=64 \
  subject.bc
```

Standardize process exit codes:

```text
0 SAFE
1 UNSAFE or UNSAFE-UB
2 UNKNOWN
3 invalid configuration or invalid verification task
4 internal failure
```

Emit `relation.smt2`, a JSON result, and a concise summary with status, paths,
bound hits, source locations, and counterexample input bytes.

Use a versioned output directory:

```text
results/
  result.json              authoritative machine-readable verdict and manifest
  relation.smt2            complete relation or labeled under-approximation
  queries/<query-id>.smt2  retained failures/disagreements, optional all queries
  counterexamples/<id>.json
  diagnostics.txt
  metrics.json
  replay/<id>/             generated wrapper/config and replay log
```

Write to temporary files and atomically rename on completion. `result.json`
contains a schema version, overall status, error list, incompleteness reasons,
input/output symbol map, target/tool/dependency versions, semantic options,
limits, seeds, support-manifest hash, and checksums for related artifacts. A
crashed/incomplete process must not leave a stale result that looks successful.

Maintain a generated support manifest in the JSON output listing supported
opcodes, intrinsics, external models, pointer assumptions, UB policy, limits,
and disabled optimizations. This makes a result auditable when capabilities
change between releases.

## Codex CLI Task Prompts

Run one milestone at a time. Before editing, ask Codex CLI to inspect the current
implementation and existing user changes. Every prompt must end with: run the
milestone's full test matrix, preserve failing artifacts, update support/status
documentation, and do not start the next milestone.

Use this workflow for every milestone:

```text
1. Start from a cleanly understood Git status; do not discard unrelated changes.
2. Record tool/dependency versions and run the existing suite as a baseline.
3. Ask Codex for the named milestone only, including its negative tests.
4. Inspect the diff, generated artifacts, unsupported cases, and test commands.
5. Run focused tests, full required regression, and the configured sanitizer job.
6. Start a fresh Codex CLI review session in read-only/review mode.
7. Fix every correctness finding and add a regression test.
8. Commit one milestone with support matrix and soundness-ledger updates.
```

Suggested implementation invocation:

```bash
codex "Read IMPLEMENTATION_PLAN.md and the current repository. Implement M<N>
only. Preserve user changes. Before editing, run and report the existing tests.
Meet every M<N> acceptance item and test listed in Test Architecture. Treat any
unsupported or incomplete behavior as UNKNOWN, never SAFE. After implementation,
run focused, full, and sanitizer tests; report exact commands, results, changed
files, remaining semantic gaps, and do not begin M<N+1>."
```

Then use a fresh session for independent review:

```bash
codex "Review the current M<N> implementation against IMPLEMENTATION_PLAN.md as
an LLVM and formal-verification correctness review. Do not edit files. Lead with
soundness bugs, LLVM semantic mismatches, solver/model errors, memory-lifetime
issues, accidental SAFE outcomes, and missing negative tests. Cite file and line.
Also inspect the actual test results and identify untested support claims."
```

Do not accept a milestone based only on Codex's summary. The repository's test
commands and machine-readable artifacts are the source of truth. If an optional
dependency is absent, Codex must report the skipped validation and cannot claim
the corresponding cross-check was completed.

### M0 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement or audit M0 only. Pin and report the
LLVM toolchain, verify input IR, validate the top-level function, and preserve
analyses correctly. Add every M0 test listed under Test Architecture. Build and
run CTest, then report exact commands and results. Do not begin M1.
```

### M1 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M1 only. Build the solver-independent
Expr, Array, Read, and persistent UpdateList AST with deterministic hash-consing,
width checks, simplification, and SMT-LIB output. Add all M1 unit/property tests
and sanitizer coverage. Do not implement STP or LLVM execution yet.
```

### M2 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M2 only. Add the Solver abstraction
and STP translation/model extraction, keeping SMT-LIB as an independent output.
Implement SAT/UNSAT/UNKNOWN/timeout propagation and all M2 tests. Do not start
the LLVM instruction executor.
```

### M3 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M3 only. Interpret the documented
scalar LLVM instructions using hand-written IR tests, query STP for branch
feasibility, and account for unsupported opcodes as UNKNOWN. Add exhaustive
i8 concrete-vs-symbolic differential tests. Do not add memory yet.
```

### M4 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M4 only. Add provenance-aware object
memory, persistent byte arrays, globals, alloca, load/store, DataLayout-based
GEP, pointer resolution, and memory errors. Implement every M4 test including
endianness and ASan/UBSan replay. Do not add registration or structs beyond what
is required to validate GEP.
```

### M5 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M5 only. Add formal input/output and
assume builtins on top of the normal memory resolver, closed relational output,
JSON artifacts, overlap/name/lifetime diagnostics, and bidirectional exhaustive
truth-table checks for small programs.
```

### M6 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M6 only. Complete aggregate and
structure semantics using byte memory and DataLayout, including padding,
unions, aggregate constants, memcpy, memmove, and memset. Add native layout
probe comparisons and every M6 test.
```

### M7 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M7 only. Add direct non-inlined calls,
frames, ABI-relevant attribute handling or explicit UNKNOWN, stack lifetimes,
and heap builtins/errors. Add all M7 tests, including Clang-generated sret/byval
IR and pointer alias cases.
```

### M8 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M8 only. Add dynamic loop activations,
per-loop metadata/default bounds, recursion/state/time limits, and complete
UNKNOWN accounting. Canonicalize multiple-latch loops or prove the counting
scheme correct with the full M8 test matrix.
```

### M9 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M9 only. Add sound LLVM undef, poison,
freeze, arithmetic flag, inbounds GEP, and lifetime semantics where feasible;
otherwise emit precise UNKNOWN. Use hand-written LangRef-derived IR tests and
Alive2 differential checks when available.
```

### M10 prompt

```text
Read IMPLEMENTATION_PLAN.md and implement M10 only after every baseline test
passes. Add optional query caching and conservative post-dominator merging,
then CUDD only if benchmarks justify it. Run every semantic test with the
optimization off and on, compare relations, and report state/query/AST metrics.
```

## Completion Definition

The initial release is complete when it analyzes the sample entry function,
models registered struct/array inputs and outputs, follows direct non-inlined
calls, honors loop bounds, detects listed memory errors, exports STP/SMT-LIB,
and never claims unqualified safety for bounded or unsupported execution.

Before tagging a release, also require:

```text
all required CI tiers green from a clean pinned environment
support matrix and soundness ledger complete and test-linked
STP models validated; representative queries agree with Z3
all UNSAFE source-level cases replay or document why replay is inapplicable
no sanitizer findings; no unresolved high-severity correctness issue
performance baseline captured; dependency licenses and versions recorded
CLI/JSON/SMT schema versioned; upgrade and compatibility notes written
```

## Authoritative References

Implementation decisions should be checked against the documentation for the
pinned versions. Useful upstream references include:

- LLVM Language Reference: <https://llvm.org/docs/LangRef.html>
- LLVM IR undefined behavior: <https://llvm.org/docs/UndefinedBehavior.html>
- LLVM loop terminology: <https://llvm.org/docs/LoopTerminology.html>
- LLVM new pass manager: <https://llvm.org/docs/NewPassManager.html>
- LLVM testing guide: <https://llvm.org/docs/TestingGuide.html>
- lit and FileCheck: <https://llvm.org/docs/CommandGuide/lit.html> and
  <https://llvm.org/docs/CommandGuide/FileCheck.html>
- STP repository and build/API information: <https://github.com/stp/stp>
- KLEE developer documentation and solver chain:
  <https://klee-se.org/releases/docs/v2.0/docs/developers-guide/> and
  <https://klee-se.org/docs/solver-chain/>
- Alive2: <https://github.com/AliveToolkit/alive2>
- Z3: <https://github.com/Z3Prover/z3>

When upstream behavior differs between the pinned release and current online
documentation, the pinned headers, tests, and release-specific documentation
govern the implementation.
