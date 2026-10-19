# M4 implementation and validation ledger

The sections dated 2026-10-09 below preserve the original prerequisite
failure and are historical. The deterministic test-clock repair described in
the 2026-10-10 follow-up removed that blocker. M4 implementation is now in
`lib/Memory.cpp` and `lib/Executor.cpp`; its current contract is
[M4_MEMORY.md](M4_MEMORY.md). The old statements below that M4 has not started
must not be read as current status.

## Latest prerequisite recheck (16:35–16:37 CST)

The renewed M4 request was audited against the entire current 1,708-line
`IMPLEMENTATION_PLAN.md`, both support/soundness documents, the working-tree
diff, M1 Expr/Array/UpdateList, M2 Solver, and M3 state/executor/scheduler code.
No production source or test expectations were changed.

All three existing builds still reported `ninja: no work to do`. The following
fresh full CTest runs were executed outside the sandbox, one CTest test at a
time. ASan/UBSan/LSan remained enabled in the sanitizer configuration:

```sh
cmake --build build-m3-debug --config Debug -j 2
cmake --build build-m3-release --config Release -j 2
cmake --build build-m3-sanitize --config Debug -j 2
ctest --test-dir build-m3-debug -C Debug --output-on-failure --stop-on-failure -j 1 --output-log baseline-m4-recheck-debug.log
ctest --test-dir build-m3-release -C Release --output-on-failure --stop-on-failure -j 1 --output-log baseline-m4-recheck-release.log
ctest --test-dir build-m3-sanitize -C Debug --output-on-failure --stop-on-failure -j 1 --output-log baseline-m4-recheck-sanitize.log
git diff --check
```

| Configuration | Exit | Passed | Failed | Existing future skips | Elapsed |
|---|---:|---:|---:|---:|---:|
| Debug | 0 | 25 | 0 | 2 | 36.96 s |
| Release | 0 | 25 | 0 | 2 | 34.92 s |
| ASan/UBSan/LSan | 0 | 25 | 0 | 2 | 69.68 s |

`git diff --check` exited 0. The two skips are
`FormalPass.UNSAFE.NativeReplay` (M5) and `FormalPass.UNKNOWN.LoopBound` (M8).
No required M0–M3 test was skipped. STP/Z3 checks, fixed-seed M1/M2 property
tests, M3 exhaustive native differential tests, executor units and all existing
lit tests passed in these runs. This is a report of observed results, not a
claim that the earlier failed prerequisite gate has been repaired.

Fresh logs, full CTest output and hashes of relevant sources are preserved in
[`artifacts/m4-baseline-recheck-20261009`](../artifacts/m4-baseline-recheck-20261009/).
The earlier failing logs and queries below are retained separately.

### M3 source audit

- Parallel PHIs: `lib/Executor.cpp:497` collects consecutive PHIs, resolves
  every incoming value against one copied `StackFrame`, then binds all results.
  `FormalExecutor.ParallelPHIsReadOnePreEntrySnapshot` tests a swap.
- Solver non-answers: `lib/Executor.cpp:227` distinguishes UNSAT from
  TIMEOUT/UNKNOWN/internal errors. `forkToDestinations` prunes only UNSAT and
  retains non-answers as constrained incomplete paths. Scripted solver tests
  passed for timeout, backend UNKNOWN, worker failure and internal errors.
- Unsupported instructions: the dispatch ends in `incomplete`, without
  advancing the program counter past an unsupported opcode. Existing M3 IR
  tests assert reason and instruction location.
- State copies: `StackFrame::Locals` is a value-owned `unordered_map`;
  `ConstraintManager` owns a `vector<ExprRef>`. Copies share immutable Exprs,
  not mutable containers. M1 UpdateList writes return new immutable heads.
- Determinism/resources: DFS pushes successors in reverse LLVM order, state
  IDs reset per execution, results sort by state ID, repeated blocks terminate,
  and per-state instruction/state scheduling limits are explicit. Existing
  repeated-run IR checks passed. This does not claim M4 memory isolation.

### Remaining prerequisite blocker

The unchanged M2 tests have now produced both failure and success. The six
earlier failures all use a 250 ms real deadline. The test injection points in
`lib/STPSolver.cpp:1804`, `1811`, `1920`, `1938`, and `2036` rely on the worker
reaching a particular phase before that deadline. The normal poll at line
1825 can instead kill a still-running worker, returning TIMEOUT at line 1852.
This is a concrete timing dependency in the test setup. The earlier run used
parallel CTest execution alongside other build configurations; the new
sanitizer run was serial and isolated. These observations are consistent with
worker startup/runtime contention, but do not prove that it was the sole cause
of the earlier failures or that production error precedence is defective.

The plan's line 1258 explicitly states: "No flaky test retries are accepted as
success." No root-cause fix was made, so the successful recheck cannot silently
replace the earlier failure with an acceptance claim. Under prerequisite item
1.6 of the renewed request, M4 remains stopped. Diagnosing/repairing the M2
timing tests or their execution environment is a separate prerequisite task;
the current request does not authorize bypassing this gate.

Soundness impact: no incorrect SAFE/UNSAFE result has been demonstrated by
these failures, but reliable validation of solver deadline/internal-error
precedence remains unresolved. Neither a wider timeout, a changed expected
status, nor a passing retry is accepted here as a repair.

### Requested M4 deliverables

No M4 design or implementation has been added: MemoryObject/ObjectState/
AddressSpace/PointerValue, pointer shadows, GEP, endian memory operations,
bounds/alignment/initialization checks, memory properties, replay and the
microbenchmark are all pending. Existing IR support is unchanged. No M4 test
or replay is claimed to have run. Only this status document and evidence
artifacts were written; no commit or push was created. The support matrix and
soundness ledger were not given new support claims. M4 is not satisfied and
the repository is not ready to enter M5.

## Original prerequisite failure

M4 implementation has not started. The requested prerequisite gate failed
before any source changes. Existing user changes were preserved. No M5 API,
aggregate execution, calls, or heap implementation was added.

## Commands and results

All three existing builds succeeded (`ninja: no work to do`):

```sh
cmake --build build-m3-debug --config Debug -j 2
cmake --build build-m3-release --config Release -j 2
cmake --build build-m3-sanitize --config Debug -j 2
```

Baseline commands:

```sh
ctest --test-dir build-m3-debug -C Debug --output-on-failure --stop-on-failure -j 2 --output-log /tmp/formal-m4-baseline-debug.log
ctest --test-dir build-m3-release -C Release --output-on-failure --stop-on-failure -j 2 --output-log baseline-m4-release.log
ctest --test-dir build-m3-sanitize -C Debug --output-on-failure --stop-on-failure -j 2 --output-log baseline-m4-sanitize.log
```

The sandbox sanitizer run failed because LeakSanitizer could not operate under
ptrace. The same sanitizer suite was therefore run with approved execution
outside the sandbox, with leak detection still enabled:

```sh
ctest --test-dir build-m3-sanitize -C Debug --output-on-failure --stop-on-failure -j 2 --output-log baseline-m4-sanitize-unsandboxed.log
```

This run exited 8: 23 CTest tests passed and `FormalSolver.Unit` failed.
The solver executable passed 42 of its 48 cases; six failed:

| Case | Assertion location in tests/SolverUnitTest.cpp | Observed failure |
|---|---|---|
| WorkerCompletionAtDeadlineCannotBeAccepted | 742 | Timeout detail lacks `worker completed` |
| MalformedResponseCannotBeMaskedByExpiredDeadline | 789 | Timeout instead of InternalError / BackendFailure |
| InvalidModelCannotBeMaskedByExpiredDeadline | 807 | Timeout instead of InternalError / ModelValidationFailed |
| InvalidModelDetectedAfterDeadlineIsStillInternal | 826 | Timeout instead of InternalError / ModelValidationFailed |
| WorkerInternalErrorAtDeadlineIsStillInternal | 844 | Timeout instead of InternalError / TranslationError |
| CompletedWorkerInternalErrorSurvivesLateDeadlinePoll | 862 | Timeout instead of InternalError / TranslationError |

All six use a 250 ms deadline. Whether the failures originate in timing-sensitive
test injection, worker startup/runtime overhead, or deadline/result handling
has not been established. They are not evidence of a memory safety finding,
and a later successful retry must not erase this failed baseline.

The unsandboxed run passed executor unit tests, exhaustive M3 native
differential tests, STP/Z3 cross-checks, and the aggregate M0/M1/M3 lit suite.
It did not finish every scheduled CTest entry because stop-on-failure was set.
Debug and Release runs were interrupted (exit 130) after the prerequisite
failure was confirmed; neither is claimed as a complete passing regression.

## Evidence and environment

Saved logs and the six timeout SMT queries are in
[`artifacts/m4-baseline-20261009`](../artifacts/m4-baseline-20261009/).
`sanitize-ctest.log` contains full test output; `sanitize-unsandboxed.log`
contains the failed CTest summary; `sanitize-sandbox.log` records the separate
ptrace limitation. `release-partial.log` records the interrupted Release run.

Observed versions:

- LLVM/Clang 20.0.0git, revision eccf4d44d346eee498b0ff709e625e3104448751.
- Pinned opt: `/tmp/formal-m3-tools-20261009/bin/opt`, Debug with assertions.
- STP 2.3.3, revision 1bdfe5076c3f78e2bb7fccc5efb1a2a36c4068e6, MiniSat.
- Z3 4.8.12.
- Sanitizers: ASan/UBSan with leak detection enabled by the existing CMake profile.

## Condition for resuming

Restore a reliable M0–M3 prerequisite baseline, including these six solver
deadline/error-precedence cases under ASan/UBSan/LSan. Diagnose and resolve the
failure in an authorized prerequisite task or supply a corrected baseline;
then run the full Debug, Release, sanitizer, and differential matrix without
flaky retries. Repairing M2 semantics or tests has not been undertaken within
this M4-only request. M4 acceptance remains entirely unverified.

## Follow-up: deterministic sanitizer fault-injection timing — 2026-10-10

The six failing M2 cases were repaired in the test-only `FormalSolverTest`
library. Modes 22 and 24–28 now use a controllable steady-clock view in the
parent solver process; each fault hook advances that clock to the configured
deadline at its intended phase. Mode 23, which tests an actual wall-clock
timeout, continues to use the real clock. Production `FormalSolver` builds do
not compile the test clock or its control API. The worker's own elapsed-time
measurement also remains real, so its response envelope is not changed by the
parent test clock.

The full sanitizer suite was rebuilt and run after reconfiguring the local
build cache to use the existing pinned LLVM tool directory. The old cache
pointed to a vanished `/tmp/formal-m3-tools-20261009/bin/opt`, which caused
unrelated FormalPass test startup failures until the cache and plugin were
regenerated.

```sh
cmake --build build-m3-sanitize -j2
ctest --test-dir build-m3-sanitize -R '^FormalSolver.Unit$' --output-on-failure
cmake --build build-m3-sanitize -j2
ctest --test-dir build-m3-sanitize --output-on-failure
git diff --check
```

Results: `FormalSolver.Unit` passed all 48 tests, including all six formerly
failing cases. The complete sanitizer CTest run passed all 25 executable tests;
two future-milestone capability gates were skipped as configured. Leak
detection remained enabled. The six-case direct invocation inside the sandbox
was not a valid test because LeakSanitizer aborts spawned worker processes in
that environment; the authoritative full CTest run was executed outside the
sandbox with the configured ASan/UBSan/LSan environment.

Debug was rebuilt and CTest was started. Its
`FormalPass.EndToEnd.IncompatibleClang` case passed after 205 seconds; the run
was interrupted while subsequent plugin tests were still running. Release was
reconfigured but its full suite was not rerun. The earlier recorded complete
Debug and Release baseline runs remain the last complete results for those
configurations. The M2 sanitizer timing blocker is resolved; M4 implementation
and acceptance remain pending.

## M4 completion check — 2026-10-10

The historical pending statement above is superseded. M4 object memory is
implemented as specified in [M4_MEMORY.md](M4_MEMORY.md). No M5 registration,
full-program SAFE verdict, function calls, heap, bulk-memory intrinsic, loop
unrolling, state merging, or CUDD work was added. Existing unrelated worktree
modifications were preserved; there was no commit or push.

The prerequisite audit read all 1,708 lines of `IMPLEMENTATION_PLAN.md`
(SHA-256 `c6918f4fcf2d6d126b27bb923edf89aaeea13c2cd9f6390b833b4d7cd891a4e3`),
the support/soundness documents, M1 Expr/Array/UpdateList, M2 Solver, and M3
state/executor/scheduler. Before M4 edits, the complete M0–M3 Debug and
Release CTests each passed 25 runnable tests with two future gates skipped:

```sh
ctest --test-dir build-m3-debug -C Debug --output-on-failure --stop-on-failure -j 1 --output-log artifacts/m4-prereq-debug-20261010.log
ctest --test-dir build-m3-release -C Release --output-on-failure --stop-on-failure -j 1 --output-log artifacts/m4-prereq-release-20261010.log
```

The M3 audit confirmed parallel PHI snapshot evaluation, solver nonanswers
retained as incomplete paths, unsupported opcodes terminated as UNKNOWN,
value-owned frame locals/constraints, deterministic DFS/state IDs, and
explicit state/instruction limits. The previous six M2 sanitizer timing
failures were repaired by the test-only clock described above before M4 work.

The final CTest environment uses a copy derived from the pinned LLVM 20
Debug/assertions `opt` with debug/symbol tables stripped, installed as
`/tmp/formal-m4-tools-20261010/opt`. The original 2.3 GiB binary is unchanged.
The basename `opt` is required by an existing M0 JSON test. The full suites
ran outside the sandbox because its seccomp profile denies Linux AF_ALG
(forcing a slow Debug software SHA-256 fallback), and LeakSanitizer needs
ptrace. This changes neither LLVM code nor test expectations.

```sh
strip --strip-all -o /tmp/formal-m4-opt-runtime-20261010 /home/dengshy/github/llvm-project/build/bin/opt
install -D -m 755 /tmp/formal-m4-opt-runtime-20261010 /tmp/formal-m4-tools-20261010/opt
cmake -S . -B build-m3-debug -DFORMAL_PINNED_OPT_EXECUTABLE=/tmp/formal-m4-tools-20261010/opt
cmake -S . -B build-m3-release -DFORMAL_PINNED_OPT_EXECUTABLE=/tmp/formal-m4-tools-20261010/opt
cmake -S . -B build-m3-sanitize -DFORMAL_PINNED_OPT_EXECUTABLE=/tmp/formal-m4-tools-20261010/opt
cmake --build build-m3-debug -j1
cmake --build build-m3-release -j1
cmake --build build-m3-sanitize -j1
ctest --test-dir build-m3-debug -C Debug --output-on-failure -j 2 --output-log m4-final-debug.log
ctest --test-dir build-m3-release -C Release --output-on-failure -j 2 --output-log m4-final-release.log
ctest --test-dir build-m3-sanitize -C Debug --output-on-failure -j 1 --output-log m4-final-sanitize.log
build-m3-debug/FormalMemoryUnitTests --gtest_filter='FormalMemory.FixedSeed*'
build-m3-debug/FormalMemoryBenchmark
git diff --check
```

Each final full CTest passed all 28 runnable tests with 0 failures; two
unchanged capability-gate tests were skipped: `FormalPass.UNSAFE.NativeReplay`
(M5) and `FormalPass.UNKNOWN.LoopBound` (M8). The full M0/M1 lit component
passed 36/36 IR tests in each configuration. The M4 unit, M4 IR lit, M0–M3
regression, STP/Z3 cross-check, and native M3 differential tests all passed.
The sanitizer CTest had `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1` and
`UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1` active. Logs are
`m4-final-{debug,release,sanitize}.log` in the repository root.

The fixed property seeds are `0x4d340001` (reference bytes/COW),
`0x4d340002` (integer widths, overlap and both endian orders), and
`0x4d340003` (StructLayout GEP); all three tests passed. Failures save a
minimal divergent byte or one-operation IR under `artifacts/m4-property-failure.*`.
The microbenchmark reported `stack_objects=128 forks=64 cow_copies=64`
with `elapsed_us=13557 peak_bytes=9830400` on the final Debug build.

The standalone replay harness was rebuilt with
`gcc -O0 -g -fsanitize=address,undefined -fno-sanitize-recover=all
tests/ir/m4/replay.c -o build-m3-debug/FormalM4Replay`. Inputs `oob 1`,
`null 0`, `misaligned 1`, and `readonly 2` each exited 1 and gave the
expected UBSan/ASan diagnostic in `artifacts/m4-replay-{oob,null,misaligned,readonly}.log`.
These concrete native witnesses complement SAT models; they do not validate
all symbolic paths or constitute M5 model-injection replay.

Known boundary: cross-provenance pointer comparisons that may depend on
one-past address coincidence, symbolic pointer-slot shadows, uncertain native
alignment, M9 poison/undef semantics, and unsupported LLVM families return
explicit UNKNOWN. No completed M4 execution is a program SAFE verdict. The
documented M4 subset and validation gate are complete; entering M5 is possible
only as new work to integrate preflight and the formal input/output relation.

## Pointer identity and model replay hardening — 2026-10-15

The historical 2026-10-10 replay statement above describes that earlier test
run. The current executor checks the exact bound object instance for public
pointer arguments, pointer comparisons, load/store and `memcpy`; numeric IDs
may still restart at 1 on each run without allowing a foreign pointer to
alias a current object. AddressSpace also rejects overlapping logical object
intervals and an ObjectState bound under a different object instance.

Every reported memory-error `PathResult` now retains a satisfying model when
the solver supplies one. The symbolic bounds and inbounds tests validate each
reported bounds-error model against its path constraints, derive the concrete
index, and run the corresponding four-byte native array write under
ASan/UBSan. GCC is required by the M4 test configuration so this check cannot
silently disappear. This is model-driven replay for the symbolic bounds cases,
not the general M5 model-injection runtime for arbitrary applications.

The current complete Debug, Release and Sanitizer CTests each passed 30/30
with zero failures and the two existing M5/M8 capability skips. Their logs are
`build-m3-{debug,release,sanitize}/Testing/Temporary/` followed by
`FormalM4PointerModelFinal{Debug,Release,Sanitize}.log`, respectively.

## Global-address and error-witness hardening — 2026-10-16

Distinct constant globals are no longer assumed to have distinct addresses
when either one permits LLVM constant merging via `unnamed_addr` or
`local_unnamed_addr`. Such pointer comparisons return explicit UNKNOWN;
self-comparison and significant-address globals keep their supported results.
For `externally_initialized` globals, the IR initializer is not installed as
an entry-state value. A read before a covering write returns UNKNOWN, while
an explicit write followed by a read still completes with the written value.

Executable memory errors now require a SAT witness model validated against
the error path constraints. Solver timeout/UNKNOWN prevents an `Error` report;
missing/invalid models and inconsistent UNSAT are INTERNAL_ERROR. Tests derive
model inputs and obtain native ASan/UBSan diagnostics for the four currently
executable error classes: null dereference, out of bounds, misaligned access,
and read-only write. `DEAD_OBJECT_ACCESS` has no executable lifetime
transition in this M4 subset; lifetime intrinsics remain UNKNOWN. This native
test harness is not the future M5 application-level model-injection runtime.

Validation: the Debug and Release full CTests passed all 28 runnable tests
with zero failures and the two existing M5/M8 skips after the executor
changes. The complete sanitizer CTest also passed 28 runnable tests with
zero failures and the same two skips. Following the final test-assertion
refinement, `FormalMemory.Unit` and `FormalExecutor.Lit.M4` passed again in
Debug, Release and sanitizer builds. Sanitizer tests must run outside the
ptrace-restricted sandbox because LeakSanitizer cannot operate there.

## Pointer-byte and GEP annotation hardening — 2026-10-16

Object pointer stores now mark the written bytes as address-sensitive, so an
integer load cannot expose the executor's logical object base as a target
address. Partial overwrites retain the affected marker, complete ordinary
overwrites clear it, and `llvm.memcpy` propagates it. Constant GEP `inrange`
returns UNKNOWN before its pointer can be used, including through a derived
GEP. An all-zero-index `inbounds` GEP of null remains valid, while a nonzero
index retains the conservative UNKNOWN result. Regression IR is in
`tests/ir/m4/address-bytes.ll` and `tests/ir/m4/gep-annotations.ll`.
The final Debug, Release and sanitizer full CTests each passed all 28 runnable
tests with zero failures; the two M5/M8 future capability gates remained
skipped. Sanitizer tests ran outside the ptrace-restricted sandbox.

## Null-valid address and byte-marker regression — 2026-10-16

`null_pointer_is_valid` is now checked on the current function before a
default-address-space null load, store, or nonzero `llvm.memcpy` is reported as
a dereference error. Address-zero memory is not modeled, so these paths are
UNKNOWN rather than ERROR; unmarked functions retain the usual null error.
`FormalMemory.Unit` directly checks this distinction, zero-length memcpy,
per-byte address-sensitivity markers across a COW fork and partial write, and
the observed result after copying pointer bytes, partially overwriting them,
or fully replacing them. The unmarked null load/store/memcpy error models are
also checked and replayed in the native ASan/UBSan harness.
`null-pointer-valid.ll` adds an IR-level regression.

At this stage, the model-driven native ASan/UBSan replay tests were
representative tests for the four executable error classes. They did not
establish per-reported-model coverage across M4 IR tests; the later gate
below addresses that gap. No M5 model-injection replay is claimed.

After this change, complete Debug, Release and ASan/UBSan/LSan CTests were
run against the rebuilt tree: each scheduled 30 tests, passed 28, failed 0,
and skipped the two unchanged M5/M8 future capability gates. The sanitizer
run was outside the ptrace-restricted sandbox so LeakSanitizer could operate.

## Zero-length memcpy parameter attributes — 2026-10-19

The zero-length `llvm.memcpy` early return now follows parameter-attribute
checks. `nonnull`/`dereferenceable` and other unmodeled constraints produce
UNKNOWN instead of a false completed path; `noundef` arguments are resolved
before completion, and zero-length alignment must be provable. Literal null
remains aligned, so its supported zero-length cases still complete. Both
call-site and intrinsic-declaration parameter attributes are inspected.
`tests/ir/m4/memcpy-attributes.ll` is verifier-accepted and exercises the
source/destination attributes, combined `noundef nonnull`, poison/undef,
alignment, nonzero length and preserved no-op cases. A direct C++ result test
checks the same zero-length boundary without relying on FileCheck text.

The native ASan/UBSan replay evidence at this point was representative. The
subsequent per-reported-model gate below closes that gap for the M4 test
corpus; it does not claim M5 model-injection replay.

The rebuilt Debug, Release and sanitizer configurations each completed the
full 30-test CTest matrix with 28 passes, zero failures and the same two M5/M8
future-gate skips. The sanitizer run was outside the ptrace-restricted
sandbox so LeakSanitizer could operate.

## Per-reported-model native replay gate — 2026-10-19

`FormalMemory.EveryReportedM4IRMemoryErrorModelIsNativelyReplayed` enumerates
M4 lit expectations with memory-error reasons and requires a one-to-one
ledger entry for every such IR entrypoint. It executes each entrypoint with
the real solver, checks every reported error path's SAT model against its path
constraints, concretizes any symbolic argument, and requires a corresponding
native ASan/UBSan failure. The native modes cover null, one-past, negative,
zero-size, crossing-end and far-offset accesses, misalignment, read-only
writes, and symbolic select/index paths. Existing direct C++ tests replay
each error path they report. A new lit error expectation without replay
mapping fails the unit test rather than silently reducing coverage.

The far-offset `align 1` i16 replay performs an unaligned bytewise read.
Its acceptance check requires an ASan invalid READ at the far address and
explicitly rejects a UBSan misalignment diagnostic; a generic `runtime
error` is insufficient evidence of the reported OOB class.

The gate covers all currently reported M4 test-corpus models. It does not
provide model injection for arbitrary binaries or claim M5 end-to-end replay.
After the final discovery-rule edit, complete Debug, Release and sanitizer
CTests each scheduled 30 tests, passed 28, failed 0, and skipped the two
existing M5/M8 future gates. Their saved outputs are
`build-m3-{debug,release,sanitize}/FormalM4OOBReplayFinal.log`.
The sanitizer and final Debug runs were outside the ptrace-restricted sandbox
so LeakSanitizer and the solver worker could operate normally.
