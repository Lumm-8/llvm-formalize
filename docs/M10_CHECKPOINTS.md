# M10 checkpoint plan

The pre-implementation baseline is built from the archived M9 commit in
`build-m10-checkpoints/baseline-source`. Build, test, and environment records
are written to `artifacts/m10/checkpoints/`. The working tree includes an
earlier M10 implementation and unrelated user changes. This audit uses the
archived source to keep the baseline independent of both.

The environment and baseline records are collected per configuration.

| Baseline build | Evidence |
| --- | --- |
| Debug | [76 passing tests](../artifacts/m10/checkpoints/baseline-debug-ctest.log) |
| Release | [76 passing tests](../artifacts/m10/checkpoints/baseline-release-ctest.log) |
| Debug with ASan, UBSan, and LSan | [76 passing tests](../artifacts/m10/checkpoints/baseline-sanitize-ctest.log) |

## Pre-implementation audit

| Component | Current invariant and checkpoint concern |
| --- | --- |
| Expr hash-consing | Node identity is scoped by the owning context. Interning enforces sort and depth limits. Commutative ordering and select normalization are already optional and need independent equivalence evidence. |
| Solver | Each STP request has a worker context and validates returned SAT models. The existing feasibility cache lives in the executor. There is no standalone decorator contract for configuration identity. |
| Execution state | Forks copy the PC, stack, path constraints, and diagnostics. The existing merger probes single-block sibling arms at their common join and processes each arm's PHIs first. |
| Address space and pointers | Object identities and update lists survive copy-on-write. Pointer alternatives have guards and object provenance. Initialization, address sensitivity, and pointer shadows are separate metadata that must survive any accepted merge. |
| Poison, undef, and freeze | Scalar poison is a separate predicate consumed by designated instructions. Supported finite freeze choices bind one value per SSA definition and path. A join must preserve that predicate and the correlation of later uses. |
| Loops and calls | Frames carry the call site, return continuation, stack object ownership, and dynamic loop activation counters. Replay depends on those identities and the configured bounds. |
| Registration and aggregation | Registration records ordered descriptors, guards, and covered bytes. Result aggregation distinguishes complete, incomplete, and erroneous paths. The relation writer can also reject an otherwise complete execution when its AST budget is exceeded. |

## A · Dual-mode verification

Use the baseline worklist, expression builder, and solver calls with every
optimization disabled. Run each configuration's complete CTest suite twice
and isolate the two modes' artifacts. Query cache, expression rewrite, and state
merge already have independent opt-in switches. The independent CUDD CMake
switch stays off while the backend is deferred.

Check verdicts, reasons, incomplete paths, and error identities. Validate each
counterexample against the constraints that produced it. Check output
relations by asking Z3 to satisfy the XOR over shared input and output symbols.
Record state and fork counts, merges, expression nodes and depth, update list
length, logical queries, backend queries, cache hits, solver time, wall time,
and peak RSS.

The CTest runner archives M5-style reports with the source module, execution
limits, relation, error-path formulas, and any exported witness. Earlier unit
and lit tests also run in both modes and retain their own test assertions.
The proof checker compares the disjunction of error paths for each error
identity and checks every exported witness with its own declared input
assignment. It retains a failing pair and the SMT query that rejected it.
The checkpoint smoke test passed for a registered relation and a divide by
zero witness. Replacing the SAFE relation with `false` produced a SAT XOR and
the proof checker rejected and retained the pair.

Release CTest passed all 78 tests in each mode. The verifier matched 4,740
reports, proved 4,737 relation XORs and 2,383 error-condition XORs UNSAT,
and validated 2,381 pairs of witnesses. A 1 ms resource test stopped at
different instructions in its two runs. The regression test requires the
same wall-clock cutoff verdict, reason, and path count, and permits the
cutoff location to vary. Error locations and other incomplete locations
remain part of comparison. See the [Release comparison record]
(../artifacts/m10/checkpoints/a-release-comparison.json).

## B · Bounded rewrites and query caching

The existing expression context hash-conses nodes and enforces sort, width,
and ownership checks. Audit commutative canonicalization and select rewrites
against APInt or solver oracles. Cover poison predicates and choices made by
`undef` and `freeze` in executor tests. Keep the rewrite set local and bounded.

The current cache is scoped to one executor run and uses interned feasibility
conjuncts. Establish the full cache identity from the normalized constraint
set, target, backend and version, options, timeout policy, target layout, and
executor semantics. Resolve SAT hits by validating the complete model against
the constraints and target. Retain only conclusive results and cover the
failure statuses with controlled solver tests.

The solver cache is recreated for every execution and every merge replay.
It keys the complete Boolean conjunction for `mayBeTrue`: the constraint
terms and target, with true terms and duplicates removed and the remainder
ordered by identity in the current expression context. Target triple and
data layout, executor and expression options, STP version, MiniSat,
compilation profile, solver options, and deadline policy namespace the key.
A hit uses the original constraints and target for SAT model validation.
The logical query count includes hits. Witness queries always use the
backend. A run-wide wall-clock limit, query export, or model audit hook
disables reuse.

Release passed 8 targeted CTests in each mode. Z3 proved all 87 output
relation XORs and 41 error-condition XORs UNSAT, and independently checked
41 pairs of witnesses. APInt tests exhaust widths 1–4 and cover signed
boundaries at widths 65 and 128. The M9 lit tests cover poison propagation
and per-use undef and per-definition freeze behavior in both modes.

Five measured runs followed five warmups per mode. On the commuted guard
fixture, median backend queries were 10 OFF, 8 with cache alone, and 3 with
cache and rewrite. Median wall time was 143.2, 112.2, and 53.2 ms, respectively,
and the combined mode reduced AST nodes from 24 to 20 at the same 23.75 MiB
peak RSS. On the independent-optimization fixture, cache alone reduced
backend queries from 10 to 3 and median wall time from 132.6 to 51.4 ms.
Rewrite alone reduced its AST nodes from 28 to 24, with no wall-time gain.
The commuted guard fixture measures the additional benefit of the rewrite
when used with the cache. See the [raw B benchmark samples]
(../artifacts/m10/checkpoints/b-release-benchmark.json).

## C · Conservative joins

The existing diamond implementation resolves PHIs on each incoming arm before
combining siblings. Audit the next instruction, dynamic frame identities,
return continuations, loop activations and counts, allocation and lifetime
state, registration order, and solver and special-value identity. Model
compatible differences with complementary guards. Preserve pointer candidate
provenance, pointer shadows, initialization masks, and aggregate padding. Test
both accepted joins and compatibility rejections before changing the merger.

Keep profitability bounded by guard and value depth, changed values, object
extent, and update list growth. Use the unoptimized run for failures or possible
baseline resource limits. Recheck all output relations and benchmark the three
named workloads after warming up each mode.

The Release check passed all 12 targeted tests in both modes. Z3 checked
147 pairs of relations and 28 pairs of error guards and witnesses.
The new cases exercise nested diamonds, equal and differing scalar values,
aggregate padding, pointer provenance, initialized bytes adjacent to a
pointer shadow, guarded poison, undef and freeze correlation, recursion,
registration changes, and incompatible allocation, lifetime and
initialization state. The original pointer-PHI failure is retained with
its SMT, JSON, metrics, and reduced IR.

The join requires the same next instruction, active layout, suspended
frames, dynamic loop history and activations, registration descriptors,
memory-read and allocation event history, and running status. Bytes and
aggregate payloads use guarded Select while initialization masks, shadows,
address-sensitivity metadata, and lifetime state must match. The probe
caps each arm at 16 instructions. A join caps input depth at 128, changed
locals at 16, changed local bitvector cost at 512 bits, pointer candidates
at four or the configured cap, and materialized object extent at 16 bytes
with at most 64 updates per changed object.

## D · CUDD decision

**Deferred.** Three of the four native merge benchmarks recorded zero
exact cache hits with all modes enabled. The loop recorded ten hits,
reducing 22 logical feasibility requests to 12 backend requests. The
environment has no CUDD package or benchmark evidence of an additional
BDD speedup. The independent
`FORMAL_ENABLE_CUDD` configure switch defaults OFF and rejects ON with a
deferred-backend diagnostic. The final semantic solver boundary stays STP.

## Commands

```sh
cmake --build "$BUILD" -j2
ctest --test-dir "$BUILD" -j2 --timeout 3600 --output-on-failure
ctest --test-dir "$BUILD" -L m10 --output-on-failure
python3 tests/run_m10_dual_mode.py --build "$BUILD" --output "$AUDIT_ROOT" --z3 "$Z3"
python3 tests/benchmark_m10.py --suite ab --executor "$EXECUTOR" --z3 "$Z3" --output ab.json
python3 tests/benchmark_m10.py --suite merge --executor "$EXECUTOR" --z3 "$Z3" --output merge.json
```

## Full M0–M10 verification

Each profile matched 4,782 reports, proved 4,779 relation XORs and 2,383
error-condition XORs UNSAT, and independently checked 2,381 witness pairs.
The corpus retained the same verdicts and incompleteness reasons.

| Profile | OFF | ON | SMT comparison |
|---|---|---|---|
| Debug | [79/79](../artifacts/m10/checkpoints/final-debug-off-ctest.log) | [79/79](../artifacts/m10/checkpoints/final-debug-on-ctest.log) | [JSON](../artifacts/m10/checkpoints/final-debug-comparison.json) |
| Release | [79/79](../artifacts/m10/checkpoints/final-release-off-ctest.log) | [79/79](../artifacts/m10/checkpoints/final-release-on-ctest.log) | [JSON](../artifacts/m10/checkpoints/final-release-comparison.json) |
| Debug with ASan, UBSan, and LSan | [79/79](../artifacts/m10/checkpoints/final-sanitize-off-ctest.log) | [79/79](../artifacts/m10/checkpoints/final-sanitize-on-ctest.log) | [JSON](../artifacts/m10/checkpoints/final-sanitize-comparison.json) |

Debug was rerun outside the sandbox so the pinned host integrity checks
could use kernel SHA-256. The partial sandbox run is retained under
`build-m10-checkpoints/final-debug-sandbox`. The sanitizer profile ran
outside the sandbox to permit LSan. All profiles exercised STP, with
Z3 4.8.12 as the independent oracle. The pinned dependencies and host
configuration are recorded in the [environment log]
(../artifacts/m10/checkpoints/environment.txt). Alive2 was unavailable.
