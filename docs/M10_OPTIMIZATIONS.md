# M10 optimization controls and metrics

The unoptimized executor is the default. The driver, plugin, and regression
runner expose independent controls.

| Optimization | Driver | Pass | Test environment |
|---|---|---|---|
| Bounded expression rewrite | `--expr-rewrite` | `-formal-expr-rewrite` | `FORMAL_EXPR_REWRITE=1` |
| Query cache | `--query-cache` | `-formal-query-cache` | `FORMAL_QUERY_CACHE=1` |
| Guarded joins | `--state-merge` | `-formal-state-merge` | `FORMAL_STATE_MERGE=1` |
| CUDD | Deferred | Deferred | Configure switch `FORMAL_ENABLE_CUDD=OFF` |

A driver flag with `--no-` or pass flag with `=false` overrides the
environment for that optimization. The feasibility cache is run-local.
The wall-time budget, solver query export and additional model auditing
disable cache reuse. Explicit wall-time and logical query budgets also
disable speculative joins. See the [soundness boundary](SOUNDNESS.md).

## Metrics

Registered execution emits these fields to `metrics.json`.

| Fields | Meaning |
|---|---|
| `states_created`, `forks`, `state_merges` | Scheduler counts for the retained execution |
| `ast_nodes`, `ast_depth` | Interned expression count and maximum constructed expression depth during execution |
| `max_update_list_length` | Largest persistent byte-update list observed during execution |
| `solver_queries`, `solver_cache_hits` | Retained logical feasibility and witness queries, and fulfilled cache hits |
| `solver_backend_queries`, `solver_time_ns` | Actual solver work, including abandoned speculative attempts |
| `wall_time_ns` | Executor elapsed time in nanoseconds including replay |
| `peak_resident_bytes` | Peak process RSS on Linux |
| `wall_ms` | Benchmark-only driver process elapsed time including startup and artifact publication |

`state_merges` counts accepted joins even when both branch states were
already created. Subsequent forks are where the state count typically drops.

## Release measurements

Each mode uses five warmups and five measured runs. Measurement order
alternates. In every round, Z3 proves the baseline and candidate relations
equivalent by rejecting their XOR. Wall time below is the process median.

| Fixture and comparison | States | STP queries | AST nodes | Max AST depth | Wall ms | Peak RSS MiB |
|---|---:|---:|---:|---:|---:|---:|
| Independent optimizations, OFF → cache | 2 → 2 | 10 → 3 | 28 → 28 | 5 → 5 | 132.6 → 51.4 | 23.63 → 23.75 |
| Commuted guards, cache → cache and rewrite | 2 → 2 | 8 → 3 | 24 → 20 | 5 → 5 | 112.2 → 53.2 | 23.75 → 23.75 |
| Diamonds, OFF → merge | 32 → 6 | 62 → 10 | 106 → 63 | 7 → 15 | 1092.1 → 154.0 | 24.00 → 23.88 |
| Loop with internal branches, OFF → merge | 32 → 6 | 188 → 22 | 71 → 69 | 8 → 16 | 3029.6 → 297.6 | 24.00 → 23.63 |
| Different writes to one object, OFF → merge | 32 → 6 | 62 → 10 | 106 → 63 | 7 → 15 | 1135.4 → 163.7 | 23.88 → 23.75 |
| Writes to two objects, OFF → merge | 32 → 6 | 62 → 10 | 103 → 75 | 7 → 20 | 1147.0 → 156.9 | 24.00 → 23.75 |

The guard fixture measures the additional query reduction from rewriting
when cache settings are held fixed. Rewrite alone on the independent
fixture reduced AST nodes from 28 to 24 and did not improve wall time.
Joins raise maximum depth by up to 13 on these fixtures. They reduce
total AST nodes and stay within the configured depth cap.

The raw samples contain all metrics for every mode.
[Cache and rewrite](../artifacts/m10/checkpoints/b-release-benchmark.json)
· [Guarded joins](../artifacts/m10/checkpoints/c-release-benchmark.json)

```sh
python3 tests/run_m10_dual_mode.py --build "$BUILD" --output "$AUDIT_ROOT" --z3 "$Z3"
python3 tests/benchmark_m10.py --suite ab --executor "$EXECUTOR" --z3 "$Z3" --output ab.json
python3 tests/benchmark_m10.py --suite merge --executor "$EXECUTOR" --z3 "$Z3" --output merge.json
```
