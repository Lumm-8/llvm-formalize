# M5 registered byte relations

M5 is enabled with `formal-check <module.ll> --entry formal_top
--output-dir <dir>`, or with `opt -load-pass-plugin FormalPass.so
-passes=formal-exec -formal-run-m5 -formal-entry=formal_top
-formal-output-dir=<dir> -disable-output <module.ll>`. The entry must be
`void ()`. Without `-formal-run-m5`, the plugin performs M0 preflight only;
its `VALIDATED` report is never a proof. The M5 driver and plugin use the same
`FormalExecutor` library as the M3/M4 test driver.

`runtime/formal_api.h` declares the input, output and assume markers. Each
registration describes a nonempty concrete byte slice of one live object in
the M4 address space. Input registration writes fresh, named symbolic bytes
at the call, before the next read. Output registration records a descriptor;
the bytes are read at every normal top-frame return before stack lifetime
ends. `__formal_assume` uses the public `void(int)` ABI (`void(i32)` on the
supported target) and constrains nonzero values; an infeasible
assumption excludes that path. A program with no feasible normal return has
the false relation. The supported `llvm.lifetime.end` form ends exactly one
complete object; capture of its registered output reports
`UNKNOWN-DEAD-REGISTERED-OUTPUT`.
`FORMAL_INPUT(x)` and `FORMAL_OUTPUT(x)` register `sizeof(x)` bytes;
`FORMAL_INPUT_BUF(ptr, size, name)` and `FORMAL_OUTPUT_BUF(ptr, size, name)`
register explicit slices. `formal_marker_runtime` supplies C-linkage stubs
for ordinary linking. The noinline/used attributes keep marker calls visible
in the documented Clang `-O0` pipeline. Debug types do not determine the
registered bytes.

Names are compile-time constant ASCII strings containing letters, digits,
underscores, dots or hyphens, with at most 128 bytes. The backing global must
be immutable. Names are unique within each registration kind on a path; input and
output may use the same name for an in-place transformation. Same-kind
overlap, including exact repeated calls and output aliasing without metadata,
is UNKNOWN. A load or
`llvm.memcpy` source read before input registration is tracked by object and
byte range; overlapping or unresolved earlier reads are UNKNOWN even when
the bytes were already initialized. Symbolic
sizes, zero sizes, unresolved or symbolic-offset pointers, conditional
pointer candidates, input slices that overlap pointer fields, and regions
beyond an object's bounds are UNKNOWN. Integer field slices of objects that
also contain concrete pointer fields are supported; registration leaves those
pointer fields unchanged. M7 adds modeled heap objects and ordinary direct
calls. `tests/ir/m5/heap-registration-blocked.ll` now checks a positive
malloc-backed region whose failure path is excluded by `__formal_assume`.
Other allocation and ABI behavior outside M7's explicit models remains
UNKNOWN. The M4 unsupported semantic gates continue to apply.
Builtin calls or declarations carrying `noreturn` are rejected before
registration or assumption effects. Builtins also require unrestricted
effective `memory(readwrite)` effects; narrower call-site or declaration
memory annotations are rejected before input bytes are changed. A top-level
entry carrying `noreturn` is rejected because its dynamic return is undefined
under LLVM semantics. Hand-written IR using a non-i32
`__formal_assume` signature returns UNKNOWN; preflight rejects it.

The result directory contains `result.json`, `relation.smt2`, `queries/`,
`counterexamples/`, `diagnostics.txt`, `metrics.json`, and `replay/`.
`relation.smt2` defines `|formal_relation|` over
`|input.<name>.<byte>|` and `|output.<name>.<byte>|`. It is a disjunction of
normal path constraints and equalities for the captured output bytes. Export
rejects free internal scalar symbols and array reads. Unused registered input
bytes still receive declarations. Each normal path contributes one complete
output tuple; errors contribute no normal disjunct. When no normal path
returns, the relation is false. If any path is incomplete, completed paths
form an explicit `UNDER_APPROXIMATION`. The driver prints `SAFE` only if every
reachable path completed and this closed relation was exported. Errors take
precedence over UNKNOWN; `UNSAFE` includes `counterexample.json` with named
input bytes and a solver-validated error model. Exit codes are 0 SAFE,
1 UNSAFE, 2 UNKNOWN, 3 invalid CLI/IR, and 4 internal or artifact error.

`formal_replay_runtime` loads the named bytes from `counterexample.json` at
input registration. Call `formal_replay_capture()` on each normal return path
while registered outputs remain alive, and use
`tools/generate_replay_main.py` to create a wrapper for the C-linkage
`void ()` entry. Capture copies registered stack and global output bytes
before their lifetimes end.
`__formal_assume(false)` returns an `OUTSIDE_DOMAIN` replay status. This
runtime covers the scalar/POD M5 subset; longjmp across nontrivial C++
destructors is outside the current replay contract.
The bounds-error replay test runs `formal-check` on IR generated from the same
source file used to build its sanitizer-enabled native executable. It checks a
captured normal input and an error witness under ASan/UBSan. A caller-supplied
replay binary has no automatic identity proof against arbitrary analyzed IR;
its result requires a corresponding build provenance check.

`result.json` schema version 1 has deterministic keys and ordered arrays.
`descriptors` lists `(kind, name, size, ordinal)` with one normal path's
object ID, byte offset, registration source location, path guards, and byte
symbols. `paths[].registrations` retains bindings for each path, including
path-dependent object IDs and offsets. `alias_overlaps` records input/output
overlap per state, object, offset and length. Same-kind output overlap is
rejected. `path_counts`, `semantic_options`, `limits`, `solver_version`,
LLVM/Clang and host-tool identifiers, and `artifacts_sha256` make the result
auditable. Content files are written through temporary files and atomically
renamed; `result.json` is invalidated first and published last. Run IDs,
solver-chosen model bytes, elapsed time and native addresses are not semantic
fields to compare across runs. The pinned STP build uses its deterministic
default search behavior and exposes no seed setting through this backend;
`semantic_options.solver_seed` records `pinned-STP-default`. Queries use a
fixed 30-second deadline unless the CLI explicitly overrides it.

M8 adds root `loop_limit_policy` and `unwinding_assumption`, and path
`loop_policy_error`, `unwinding_assumption`, `loop`, and `loop_bounds`.
Each loop object records its static `function:header` ID, dynamic frame and
activation IDs, source and value of the bound, actual traversals, and, on a
suppressed edge, the attempted traversal and CFG edge. Incomplete paths also
record `limit_name`, `limit_value`, `observed_value`, state, call stack,
and source/IR location. Root `incompleteness_reasons` counts stable reason
codes including `UNKNOWN-LOOP-BOUND`, `UNKNOWN-IRREDUCIBLE-CYCLE`,
`UNKNOWN-LOOP-METADATA-LOST`, state/instruction/call-depth/time/query caps,
solver timeout/UNKNOWN, unsupported operations, memory candidate/object caps,
and registration errors. A completed path beside any incomplete path is
`UNDER_APPROXIMATION`; explicit unwinding assumptions yield `CONDITIONAL`
only when no other incompleteness or feasible error exists.
`limits.require_local_loop_bound` records the opt-in requirement that every
reached loop retain a local bound annotation. Its failure is
`UNKNOWN-LOOP-METADATA-LOST` rather than a global-default fallback.
`limits.default_loop_backedge_bound` uses the same lossless representation as
`paths[].loop_bounds[].backedge_bound`: a JSON number through `INT64_MAX`, then
a decimal string through `UINT64_MAX`.

The `m5` CTests cover hand-written IR, closure, duplicate names, slices,
in-place output, globals, assumption pruning, descriptor disagreement,
pointer input rejection, lifetime diagnostics, a Clang `-O0 -g` program,
native stack capture, and an ASan/UBSan replay of a solver-derived bounds
counterexample. Five truth-table tests enumerate native byte outcomes,
including a two-output tuple, and prove soundness and precision separately
with STP UNSAT queries. The full-byte scalar case also checks SAT for the
native output and UNSAT for every other output at each of 256 fixed inputs.
A pinned `opt` integration test checks plugin artifact
publication after M0 preflight. `FormalExecutor.M5.ArtifactClosure` checks
under-approximation, false relations, SHA-256 checksums, aliases, closure,
STP reparsing, and frontend agreement. M7 additionally checks a live heap
region and an ASan replay of a solver-derived double-free witness.
