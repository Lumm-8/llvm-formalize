# The LLVM Compiler Infrastructure

[![OpenSSF Scorecard](https://api.securityscorecards.dev/projects/github.com/llvm/llvm-project/badge)](https://securityscorecards.dev/viewer/?uri=github.com/llvm/llvm-project)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/8273/badge)](https://www.bestpractices.dev/projects/8273)
[![libc++](https://github.com/llvm/llvm-project/actions/workflows/libcxx-build-and-test.yaml/badge.svg?branch=main&event=schedule)](https://github.com/llvm/llvm-project/actions/workflows/libcxx-build-and-test.yaml?query=event%3Aschedule)

Welcome to the LLVM project!

This repository contains the source code for LLVM, a toolkit for the
construction of highly optimized compilers, optimizers, and run-time
environments.

The LLVM project has multiple components. The core of the project is
itself called "LLVM". This contains all of the tools, libraries, and header
files needed to process intermediate representations and convert them into
object files. Tools include an assembler, disassembler, bitcode analyzer, and
bitcode optimizer.

C-like languages use the [Clang](https://clang.llvm.org/) frontend. This
component compiles C, C++, Objective-C, and Objective-C++ code into LLVM bitcode
-- and from there into object files, using LLVM.

Other components include:
the [libc++ C++ standard library](https://libcxx.llvm.org),
the [LLD linker](https://lld.llvm.org), and more.

## Getting the Source Code and Building LLVM

Consult the
[Getting Started with LLVM](https://llvm.org/docs/GettingStarted.html#getting-the-source-code-and-building-llvm)
page for information on building and running LLVM.

For information on how to contribute to the LLVM project, please take a look at
the [Contributing to LLVM](https://llvm.org/docs/Contributing.html) guide.

## Getting in touch

Join the [LLVM Discourse forums](https://discourse.llvm.org/), [Discord
chat](https://discord.gg/xS7Z362),
[LLVM Office Hours](https://llvm.org/docs/GettingInvolved.html#office-hours) or
[Regular sync-ups](https://llvm.org/docs/GettingInvolved.html#online-sync-ups).

The LLVM project has adopted a [code of conduct](https://llvm.org/docs/CodeOfConduct.html) for
participants to all modes of communication within the project.

## FormalPass M3 scalar-executor tests

M3 is a test-oriented, single-function scalar LLVM IR executor. It does not
support memory, calls, the registration API, ordinary Clang `-O0` stack IR, or
program-level `SAFE`/`UNSAFE` verdicts.

After configuring a build with the pinned LLVM and STP dependencies, run:

```sh
cmake --build <build-dir> --target FormalExecutorUnitTests FormalExecutorDifferentialTests FormalScalarExecutor
ctest --test-dir <build-dir> --output-on-failure -L '^m3$'
```

The focused IR suite can also be run through the `FormalExecutor.Lit.M3` CTest.

## Formal execution optimizations

Unoptimized execution is the default. Enable query caching with
`formal-check --query-cache` or the pass option `-formal-query-cache`.
Enable expression rewriting independently with `formal-check --expr-rewrite`
or the pass option `-formal-expr-rewrite`.
Enable state merging with `formal-check --state-merge` or
`-formal-state-merge` on the pass.
The optional CUDD build remains deferred, with `FORMAL_ENABLE_CUDD=OFF`.

For runs across the regression suite, set `FORMAL_QUERY_CACHE=1` and
`FORMAL_EXPR_REWRITE=1`, and `FORMAL_STATE_MERGE=1`. The driver accepts
`--no-query-cache`, `--no-expr-rewrite`, and `--no-state-merge` to override
those settings. The pass options accept `=false` for the same purpose.

State merging handles straight-line diamond arms with a common join after
PHIs. It guards scalar, poison, aggregate, and pointer values and small
compatible memory objects. Runs with an explicit wall-time or solver-query
budget use the baseline worklist. A speculative error, incomplete path, or
possible baseline state cap triggers a replay with merging disabled.

The query budget counts logical requests, including cache hits. Registered
execution records state and fork counts, merges, logical and backend
queries, cache hits, AST node count and maximum depth, maximum UpdateList
length, solver and wall time in nanoseconds, and peak resident bytes in
`metrics.json`. Backend query count and solver time include abandoned
attempts. Wall time measures executor work including replay.

Use the Z3 oracle to check both modes of a configured test profile.

```sh
python3 tests/run_m10_dual_mode.py --build "$BUILD" --output "$AUDIT_ROOT" --z3 "$Z3"
ctest --test-dir "$BUILD" -L m10 --output-on-failure
python3 tests/benchmark_m10.py --help
```

Each benchmark uses at least five warmups and five measured runs per mode.
The comparison records and benchmark samples are linked from
[M10_CHECKPOINTS.md](docs/M10_CHECKPOINTS.md).
