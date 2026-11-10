# M7 call and heap contract

The executor follows defined direct C calls with a separate SSA frame per
activation. Frames share one address space. Returning from a frame ends its
`alloca` and `byval` objects before execution resumes at the caller. A later
read or write through one of those pointers reports `DEAD_OBJECT_ACCESS`.
`--max-call-depth` bounds recursion and reports `UNKNOWN-CALL-DEPTH`.
Repeated calls have separate block-visit sets; loop analysis remains outside
this milestone.
Calls through exact, non-interposable function aliases, including alias chains,
resolve to the same defined function and use the ordinary frame, argument, and
return checks. [`alias-calls.ll`](../tests/ir/m7/alias-calls.ll) exercises scalar
returns and a write through a pointer into the caller's object.

The call validator accepts scalar, pointer, and modeled aggregate arguments
and returns. It checks `noundef` scalar operands when resolving them and checks
aggregate semantic bits against their known-bit masks at argument and return
boundaries. Unproved aggregate bits yield `UNKNOWN-POISON-ANNOTATION`; storage
padding is excluded, as required by the pinned LangRef. `nonnull`,
`dereferenceable`, and `align` require a single live object with a concrete
in-range aligned offset. `byval` receives an independent stack copy including
known-byte masks and pointer shadows. Its owner is the callee's dynamic frame,
including when allocated before entering that frame. `sret` uses the caller's object and
requires its full pointee range. `noalias` on `sret` requires a fresh caller
stack object not passed through another pointer argument or stored in memory.
`writable` checks object mutability; `dead_on_unwind` requires a `nounwind`
callee. `readonly` and `writeonly` are accepted only when a conservative scan
of the callee proves the claimed effect. Other parameter and return ABI
attributes, non-C calling conventions, variadic calls, indirect calls,
operand bundles, `musttail`, and unmodeled external calls produce `UNKNOWN`.
Defined `noreturn` functions are entered normally; reaching `ret` in one
reports `NORETURN_VIOLATION` instead of resuming the caller.
Matching integer `zeroext` and `signext` attributes are accepted on direct
calls; root entry integer extensions describe ABI bits outside the modeled IR
value and are accepted after type checking.
Defined calls with `nocapture`, `returned`, `dereferenceable_or_null`, or
unproved return attributes produce `UNKNOWN`. Function-wide `readnone` and
other restricted memory effects likewise require a transitive proof and
produce `UNKNOWN` in M7.

On x86_64 Linux, `malloc` and `calloc` create a fresh successful allocation.
`--allocation-failures` also forks a null failure; otherwise the model records
an explicit success assumption in each path's event history. In M5 proof mode,
the unmodeled failure also creates an incomplete path with reason
`UNKNOWN-ALLOCATION-FAILURE-ASSUMED`; the successful execution alone cannot
produce an unconditional `SAFE`. `UNSAFE` still takes precedence if a modeled
error is reachable. M4's `M4_EXECUTOR_COMPLETE` describes execution under the
recorded success assumption and is not an all-outcomes proof. The opt plugin
uses `-formal-allocation-failures` for the same choice. A successful zero-size
`malloc` has a unique nonnull zero-byte object that may be freed but not
dereferenced. `calloc` initializes every allocated byte to zero. `realloc`
with a positive concrete size copies `min(old,new)` bytes and ends the old
allocation; when failures are enabled, a null branch preserves the old one.
`free(NULL)` is a no-op. Freeing a stack/global/interior pointer reports
`INVALID_FREE`; freeing an ended heap object reports `DOUBLE_FREE`; dereferencing
one reports `USE_AFTER_FREE`. Allocation sizes must be concrete and within
`MaxObjectBytes`. `realloc(p,0)` is explicitly `UNKNOWN` because its behavior
depends on the C runtime version. The heap objects guarantee 16-byte alignment
only under the checked x86_64 Linux target triple.

The pinned x86_64 Itanium C++ ABI symbols `_Znwm`, `_Znam`, `_ZdlPv`,
`_ZdaPv`, `_ZdlPvm`, and `_ZdaPvm` are recognized. A successful `new` gets a
separate allocation kind, so `free(new_ptr)` and `delete(malloc_ptr)` report
invalid deallocations. Throwing `new` has a separate `UNKNOWN` failure path:
exceptions and exception-disabled termination are outside M7's runtime model,
so a program using it cannot be declared SAFE solely from its successful
allocation path. Other operator overloads remain `UNKNOWN`.

The external model catalog in
[`ExternalFunctionModel.h`](../include/formal/ExternalFunctionModel.h)
declares each recognized symbol's signature, object ranges, effects, normal
and error paths, symbolic-size support, initialization, and pointer-shadow
behavior. Preflight and execution share this catalog; execution checks the
declaration and call-site ABI before dispatch. The byte-level `memcpy`,
`memmove`, and `memset` intrinsics and external C
functions operate on concrete bounded regions. `memcpy` overlap is an error;
`memmove` snapshots the source before writing. Complete pointer shadows survive
copies. `abort` and glibc `__assert_fail` report error paths. Full-object
`llvm.lifetime.start/end` begins an uninitialized stack lifetime or ends it;
partial and ambiguous lifetimes produce `UNKNOWN`.

M5 preflight admits only the named external models and leaves signature and
attribute checking to the executor. Any other reachable declaration remains
`UNKNOWN`. Seven M7 heap replay tests compile their sources both as
pinned-Clang IR and as native ASan fixtures. They check exported witnesses
for double free, use-after-free, invalid stack free, interior free, use of
the old pointer after successful `realloc`, double delete, and `free(new)`.
The latter two call the pinned `_Znwm`/`_ZdlPv` ABI symbols explicitly; ordinary
Clang `new` expressions may carry additional unmodeled call-site attributes.
The `OverlapMemcpyReplay` test checks a one-byte-offset overlapping libc
`memcpy` call and requires ASan's `memcpy-param-overlap` report. The separate
[`UseAfterReturnReplay`](../tests/check_m7_stack_error_replay.py) test compiles
one source to pinned-Clang IR and a native ASan binary, checks the exported
`DEAD_OBJECT_ACCESS` witness, and requires a native
`stack-use-after-return` report. These tests establish concrete reproduction
for their listed errors; they do not claim native replay coverage for every
possible M7 error. Passing replay tests retain `counterexample.json`,
`result.json`, generated `program.ll`, a safe input and output, the ASan
diagnostic, and `verified.json` under each build's `m7-evidence/` directory.
[`AbortReplay` and `AssertReplay`](../tests/check_m7_termination_replay.py)
compile the same C++ source to pinned-Clang IR and native sanitizer binaries,
check the exported `choice=1` witnesses, and require native `SIGABRT`. The
assertion replay also checks the assertion text. Explicit termination does not
require an ASan memory-error diagnostic; both tests retain the witness, IR,
result, native stderr, and verification record under `m7-evidence/`.
The pinned-Clang `byval`/`sret` test compares the IR run with its native run,
including a pointer field inside a `byval` aggregate, and checks that the
retained IR snapshot exactly matches the current pinned-Clang output.
M5 `result.json` records each path's call stack and event history with dynamic
frame and allocation IDs, source locations, external model names, allocation
assumptions, releases, reallocations, lifetime ends, and heap errors. It also
records the configured and observed maximum call depth.
[`tests/ir/m7/direct.ll`](../tests/ir/m7/direct.ll) checks `byval` frame
ownership through `llvm.lifetime.end` and pointer provenance copied into a
private `byval` object; the pinned-Clang ABI fixture checks a real `byval`
mutation leaves the caller's bytes unchanged. The heap fixture
[`heap-options.ll`](../tests/ir/m7/heap-options.ll) checks concrete `calloc`
product overflow returns `UNKNOWN` and complete pointer shadows survive both
growth and shrinkage by `realloc`.
[`tests/check_m7_heap_events.py`](../tests/check_m7_heap_events.py) checks the
allocation, `realloc`, and `free` object-ID chain in `result.json`.
It also checks that default M5 results for `malloc`, `calloc`, and `realloc`
are `UNKNOWN` when failure is unmodeled, while explicitly enabling failure
forks permits `SAFE` for a program that handles null.
[`NoundefAggregate`](../tests/check_m7_noundef_aggregate.py) checks JSON
verdicts for undefined aggregate argument and return bits and for legal
undefined storage padding.
