# M4 object memory contract

M4 extends the single-function executor with object memory. The test driver
requires `--m4` for this mode and prints `M4_EXECUTOR_COMPLETE` for a
`Complete` result. This means only that every explored
path in this supported subset returned normally. It is **not** a program
`SAFE` verdict. There is no FORMAL_INPUT/FORMAL_OUTPUT registration or exported
input/output relation.

## Object and pointer model

Each dynamic `alloca` and each global gets a monotonically assigned object ID
for one executor run. IDs are reset at the start of the next run. The object
records `Global` or `Stack`, concrete allocated byte size, alignment,
allocation site, read-only flag, and a logical base. The logical base starts at
`0x10000`, rounds up to the object's alignment, and reserves `size + 1` bytes,
including for a zero-sized object. Overflow or exhaustion is UNKNOWN. Logical
bases are for pointer bytes and concrete lookup; they are **not** assumed to
equal native addresses and are not used to prove alignment.

`PointerValue` keeps guarded `(object identity, signed offset Expr)`
alternatives. Null is a separate alternative; an unflagged GEP from null
retains its numeric offset and compares by that address value. A numeric Expr
is retained for byte storage and null-derived comparisons. `select` can join guarded alternatives. Pointer `eq/ne`
compares offsets within one object; distinct nonzero object bases and object
bases versus null compare unequal. Cross-provenance comparisons involving
non-base or zero-sized pointers are UNKNOWN, because actual target addresses
could coincide at one-past boundaries (`UNKNOWN-POINTER-COMPARISON`). A pointer load without a compatible shadow
cannot recover provenance from its numeric bytes and is UNKNOWN. An object
identity from outside the state's AddressSpace is also UNKNOWN, not a guessed
target. A numeric ID is only a lookup key: the pointer's immutable object
instance must be the exact instance bound in the current state. This check also
applies to public pointer arguments, pointer comparisons and `memcpy`, so IDs
reused by another executor run cannot alias current objects.

`AddressSpace` maps IDs to `ObjectState` and has a sorted logical-base index.
Binding rejects overlapping logical intervals and a state whose object
instance differs from the object being bound, even if their IDs match.
Forking copies the maps and shares ObjectStates without active write handles.
`getWriteable` returns a scoped write handle. If a handle is still live at a
fork, the copied AddressSpace eagerly detaches only that object, so a write
through the pre-fork handle cannot mutate the fork. Otherwise the first write
to a shared ObjectState clones it. The ObjectState owns
an immutable M1 byte-array update list, per-byte initialization and known-bit
masks, per-byte address-sensitivity markers, pointer shadows, and per-state
live status. A top-frame return ends its
stack-object lifetimes. `unbind` removes the object and its base index.

Pointer shadow entries record start, byte extent, pointer width, address space,
PointerValue and numeric Expr. A full pointer store writes its numeric bytes
then installs the shadow at a concrete offset. A matching full pointer load
from a concrete slot offset recovers the PointerValue; symbolic slot offsets
without an exact shadow match are UNKNOWN. Every overlapping ordinary write invalidates the
shadow; a symbolic write conservatively invalidates all shadows in that object.
The shadow is isolated by the same object-level COW rule as the bytes. For a
bounded, concrete, nonvolatile `llvm.memcpy`, a fully copied pointer shadow is
relocated with its bytes. A partial copy does not create a shadow, and any
overlap with an existing destination shadow invalidates it. Subsequent pointer
loads without an exact shadow are UNKNOWN. Overlapping memcpy, symbolic length
or offsets, volatile memcpy, memmove and memset remain UNKNOWN.
Address-sensitive bytes written from an object pointer cannot be reinterpreted
as a definite integer: an overlapping integer load is UNKNOWN. The marker
survives partial ordinary writes and `memcpy`; a full ordinary overwrite
clears it. Pointer loads with an intact shadow keep their supported behavior.

## Layout, GEP and accesses

All sizes and alignments come from the current Module's DataLayout. Object
allocation and each GEP array step use `getTypeAllocSize`; load/store use
`getTypeStoreSize`. Scalable TypeSize is UNKNOWN. GEP begins at
`getSourceElementType`, applies the first index using that type's allocation
stride, descends through array elements, and uses `StructLayout` for constant
struct fields. Indices are signed and converted to the pointer index width;
arithmetic is checked in twice that width before narrowing. Symbolic offsets
remain Exprs. GEP constructs a pointer without reading memory. One-past
construction is allowed; every nonzero one-past access is OUT_OF_BOUNDS.
For plain `inbounds`, each intermediate offset is checked against `[0,size]`;
an infeasible precondition yields UNKNOWN rather than silently dropping poison.
An all-zero-index `inbounds` GEP is valid even with a null base; a nonzero
null-based `inbounds` GEP remains UNKNOWN. Constant GEP `inrange` is rejected
before producing a pointer because M4 does not yet track its restrictions on
derived accesses and comparisons. Other unsupported GEP nowrap flags are
UNKNOWN.

Every load/store resolves candidates first. Null, dead-object, `[offset,
offset+storeSize)` boundary, declared alignment, read-only, and initialized
bits are checked before byte pack/unpack. The boundary check compares the
offset with `objectSize - accessSize` only after confirming `accessSize <=
objectSize`, so verifier-side addition cannot wrap. Negative offsets are
rejected. Zero-sized objects admit pointer construction but no nonzero access;
LLVM has no zero-byte scalar load/store in this subset. A store with a
non-byte-multiple integer width marks only the value bits known: for example
an `i1` store permits a later `i1` load but an overlapping `i8` load is
UNKNOWN. Reading any required unknown bit is `UNINITIALIZED_READ` / UNKNOWN.
Multibyte pack/unpack follows DataLayout endianness.

For a function with `null_pointer_is_valid`, address-space-zero null may be
valid for loads and stores. M4 has no address-zero memory object, so a
nonzero load/store or `llvm.memcpy` using that null address returns
`POINTER_PROVENANCE_LOST` with UNKNOWN status, not `NULL_DEREFERENCE`. Without the
attribute, the existing null error remains. A zero-length `llvm.memcpy`
remains a no-op only after its call-argument attribute preconditions are
accounted for.

`llvm.memcpy` checks call-site and intrinsic-declaration parameter attributes
before the zero-length early return. Supported intrinsic attributes and
`noundef` on already modeled values retain their normal behavior. Unsupported
constraints such as `nonnull` or `dereferenceable` return
`UNKNOWN-POISON-ANNOTATION`, including at length zero. For a zero-length copy,
an `align` attribute greater than 1 is accepted for a literal null pointer
(numeric address zero is aligned); otherwise alignment must be modeled before
completion, so the path is UNKNOWN. This avoids treating a potentially poison
argument combined with `noundef` as an ordinary no-op.

Alignment is proved from the **guaranteed object alignment**, not the logical
base. If the object alignment is at least the IR access alignment, an offset
multiple of the access alignment succeeds and any other offset is a definite
`MISALIGNED_ACCESS`. If the object is guaranteed less alignment than the IR
access requires, M4 reports `UNKNOWN-ALIGNMENT-UNDETERMINED`: a native
allocation could happen to be more aligned than its minimum.

Candidate guards, valid-range guards, misalignment guards and initialization
guards are queried independently where symbolic. SAT valid candidates continue
with their guards in the path constraints. UNSAT candidates alone are pruned.
Timeout or solver UNKNOWN records an incomplete path. More provenance
alternatives than `MaxPointerCandidates` returns
`POINTER_RESOLUTION_INCOMPLETE`; no truncated set is certified complete.

## Supported IR and explicit rejection

| Family | M4 execution |
|---|---|
| `GlobalVariable` | Zero, integer scalar, nested fixed array/struct integer and pointer initializers, null/global pointer and constant GEP pointer initializers; `constant` globals are read-only |
| `alloca` | Concrete element count, fixed allocated type, checked byte multiplication, fresh dynamic ID |
| `load`, `store` | Non-atomic, non-volatile integer and pointer scalars in address space zero; widths i1/i8/i16/i24/i32/i64/i65/i128 tested |
| `getelementptr` | First index, nested arrays, constant struct field and padding offsets, symbolic array index, plain `inbounds` with checked preconditions |
| `icmp` / `select` / `phi` | Pointer `eq/ne`, guarded pointer select, and pointer PHI values |
| `llvm.memcpy` | Bounded concrete length and offsets, nonvolatile, disjoint source/destination ranges; full pointer shadows copied; zero-length no-op only after parameter-attribute checks |
| Globals | External globals, aliases, floating/vector/undef aggregate members and unsupported address spaces are UNKNOWN |
| Allocation and access | Symbolic alloca count, scalable size, non-default address spaces, vector GEP, arbitrary symbolic pointer bytes, failed pointer provenance, oversized object, and candidate cap are UNKNOWN |
| Calls and casts | Unsupported memcpy forms, memmove/memset/lifetime intrinsics, heap allocation, ordinary calls, `inttoptr`, and pointer/int reconstruction are UNKNOWN; no M5+ semantics are implied |
| LLVM special values | Poison, undef, freeze and unsupported flags remain UNKNOWN under the M9 gate |

The test-oriented `FormalScalarExecutor --m4` can execute M4 hand-written IR
directly. Without `--m4`, it retains the accepted M3 behavior. Only globals
reachable through the selected function's operands and their initializer
dependencies are bound; unrelated module globals do not affect the run. M0
preflight still has its independently documented target and
poison-annotation gates. An M4 library result is not an M0 validation result.
An `externally_initialized` global keeps its object and can be read after a
fully known program write, but its IR initializer is not treated as the entry
value; reads of still-unknown bytes are UNKNOWN. Comparisons of distinct
constant-global addresses are UNKNOWN when either global has `unnamed_addr`
or `local_unnamed_addr`, because LLVM may merge such addresses.

## Stable result reasons

Feasible errors are `NULL_DEREFERENCE`, `OUT_OF_BOUNDS`,
`READ_ONLY_WRITE`, `DEAD_OBJECT_ACCESS`, and `MISALIGNED_ACCESS`.
Incomplete reasons include `UNINITIALIZED_READ`, `POINTER_PROVENANCE_LOST`,
`POINTER_RESOLUTION_INCOMPLETE`, `UNSUPPORTED_ADDRESS_SPACE`,
`UNKNOWN-POINTER-COMPARISON`,
`SYMBOLIC_ALLOCATION_SIZE`, `UNKNOWN-SCALABLE-TYPE`,
`UNKNOWN-UNSUPPORTED-GLOBAL`, `UNKNOWN-INBOUNDS-GEP`,
`UNKNOWN-ALLOCATION-LIMIT`, and `UNKNOWN-ALIGNMENT-UNDETERMINED`.
Broken memory-model invariants are `INTERNAL_MEMORY_MODEL_ERROR`, never
ordinary UNKNOWN.

The executor counts created MemoryObjects/ObjectStates, COW copies, byte
read/write Exprs, pointer resolutions/candidates/forks/cap hits, resolution
solver queries and elapsed microseconds, AST nodes, maximum byte-array update
length, and process peak resident bytes. There is no state merge or complex
query cache.

## Test inventory and replay

`FormalMemory.Unit` covers COW, concrete lookup, pointer-shadow isolation,
candidate cap, timeout, SAT-only candidate continuation, repeated-run IDs,
foreign-pointer ID collisions, overlapping logical intervals, mergeable
global addresses, externally initialized entry values, symbolic `inbounds`
splits, `null_pointer_is_valid` load/store/memcpy, direct address-sensitive
byte marker and copy/partial-overwrite assertions, zero-length memcpy
parameter-attribute semantics, mandatory SAT models for executable memory errors,
and fixed-seed byte/IR/DataLayout differential properties (`0x4d340001`,
`0x4d340002`, and `0x4d340003`). On property failure, it saves a minimal one-operation IR or
first divergent byte under `artifacts/m4-property-failure.*`.
`FormalExecutor.Lit.M4` covers hand-written IR in `tests/ir/m4/`: globals,
pointer initializers, fixed-width store/load, both endiannesses, GEP arrays
and struct padding, symbolic index, aliasing, one-past, bounds, zero-size,
alignment, read-only, uninitialized bytes, pointer-shadow invalidation,
address spaces, scalable types, inbounds and unsupported calls/casts.
`regressions.ll` additionally checks non-inbounds null GEP pointer comparisons,
nonzero aggregate initializers and full/partial pointer memcpy.
`memcpy-attributes.ll` verifies its IR with LLVM `opt` and checks call-site
`nonnull`, `noundef`, `dereferenceable`, alignment and preserved no-op cases.
`FormalMemory.Benchmark` makes 64 state forks with 128 stack objects and
asserts that only the written ObjectState is cloned in each fork.

| Required cases | Primary evidence |
|---|---|
| 1–4: initializers, read-only and alloca counts | `basic.ll`, `scalars.ll` |
| 5–8: aliases, integer widths and both endian orders | `scalars.ll`, `big-endian.ll`, `MemoryUnitTest.cpp` |
| 9–15: exact end, one-past, null, negative and overflowing offsets | `basic.ll`, `scalars.ll` |
| 16–21: symbolic/nested/struct GEP, zero size and alignment | `symbolic.ll`, `scalars.ll`, `MemoryUnitTest.cpp` |
| 22–27: fork COW, pointer shadow, overlap and null round trip | `symbolic.ll`, `scalars.ll`, `MemoryUnitTest.cpp` |
| 28–30: SAT candidate, cap and solver timeout | `symbolic.ll`, `MemoryUnitTest.cpp` |
| 31–35: unsupported globals, address space, scalable, inbounds and memory calls | `external-global.ll`, `unsupported.ll`, `scalars.ll` |
| 36: repeat-run deterministic IDs/results | `MemoryUnitTest.cpp` |

`tests/ir/m4/replay.c` is a standalone native harness, not the M5 runtime.
Input `oob 1`, `null 0`, `misaligned 1`, and `readonly 2` each correspond to
a feasible M4 error class. Build it with GCC ASan/UBSan because this local
pinned Clang package lacks its sanitizer runtime libraries. The
`symbolic_oob` mode is compiled with ASan/UBSan; M4 test configuration
requires GCC so this replay cannot silently disappear from a green suite.
The unit tests now derive native replay inputs from solver models for the four
currently executable error classes: null dereference, bounds, misalignment,
and read-only write. The `null_pointer_is_valid` regression also replays
the unmarked null load, store and memcpy error models separately. Every such
`Error` path must carry a SAT model validated
against its path constraints; missing/invalid models become INTERNAL_ERROR,
while witness-query timeout/UNKNOWN remains UNKNOWN. The older concrete
witnesses remain supplementary. `DEAD_OBJECT_ACCESS` is defined but has no
executable lifetime transition in the current M4 subset (lifetime intrinsics
are UNKNOWN). `EveryReportedM4IRMemoryErrorModelIsNativelyReplayed`
additionally discovers every M4 lit entry expected to report a memory error,
requires an exact replay-ledger match, validates each reported path model and
replays every such path under native ASan/UBSan. Its far-offset `align 1` i16
case uses an unaligned bytewise read and requires an ASan invalid READ
diagnostic, not a UBSan misalignment diagnostic. The current M4 test-corpus
criterion is therefore covered; M5 model injection and arbitrary
application-level replay remain outside this subset.
