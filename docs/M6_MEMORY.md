# M6 aggregate and byte-memory policy

M6 represents aggregate SSA values as their `DataLayout` allocation-size byte
sequence, with a known-bit mask and pointer provenance shadow for each relevant
range. Struct fields use `StructLayout` offsets; array elements use allocation
size as their stride. A typed aggregate load discards padding bytes, their
initialization masks, and any pointer shadow in padding. A typed aggregate
store fills padding with undef bytes, even when the destination padding was
initialized before the store. Typed scalar extraction requires the scalar's
bits to be initialized. A byte load of padding after an aggregate store
returns `UNKNOWN` (`UNINITIALIZED_READ`).

An aggregate constant initializes its named fields. Ordinary struct padding is
left uninitialized, including after a typed aggregate store. Bytewise
`memcpy` and `memmove` retain padding initialization because they copy object
bytes rather than aggregate SSA values. A zero global
initializer initializes the whole allocated object to zero, as in the existing
global-memory policy. `memcpy` and `memmove` copy byte values, known-bit masks,
address sensitivity, and complete pointer shadows. `memmove` snapshots the
source before writing the destination. A partial overwrite invalidates a
pointer shadow; a later pointer load or extraction returns `UNKNOWN` if its
provenance cannot be recovered. `memset` initializes every written byte and
invalidates overlapping pointer shadows.

Input registration assigns symbolic bytes only to the explicitly registered
range. Registering field slices leaves intervening padding untouched;
registering an entire object intentionally includes padding. The flexible-array
boundary is the allocated memory object's size, not the zero-length member's
nominal size. M6 handles intrinsic `llvm.memcpy`, `llvm.memmove`, and
`llvm.memset` with concrete bounded lengths and resolvable object offsets.
Overlapping `llvm.memcpy` reports `OVERLAPPING_MEMCPY` as undefined behavior;
overlapping `llvm.memmove` is supported.
Union tests observe the object representation through byte access; M6 does not
infer a C or C++ active union member or authorize language-level inactive-member
reads.

`FormalExecutor.M6.NativeBytes` compares 75 defined native bytes with
hand-written IR and checks typed-copy padding as `UNKNOWN` without using its
incidental native value as an oracle. `FormalExecutor.M6.CompiledIR` uses the
pinned Clang to compile four byte-copy probes from the same C++ source as the
native executable, then compares their executor returns with native defined
bytes. `FormalExecutor.M6.Layout` compares DataLayout with native layout.

Unmodeled forms remain `UNKNOWN`; M7 adds defined direct calls, stack frames,
and named runtime models. See [M7_CALLS_HEAP.md](M7_CALLS_HEAP.md).
