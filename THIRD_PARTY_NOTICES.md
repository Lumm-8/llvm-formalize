# Third-Party Notices

FormalPass M0/M1/M2 uses LLVM public APIs and links LLVM support libraries. LLVM is
distributed under the Apache License v2.0 with LLVM Exceptions; see the pinned
revision in `dependencies.lock.yaml` and the upstream LLVM license files.

The unit tests build the vendored LLVM copy of GoogleTest 1.14.0 from
`third-party/unittest/googletest`. Its BSD 3-Clause license is preserved at
`third-party/unittest/googletest/LICENSE.TXT`. The vendored copy contains LLVM's
documented printing adaptations listed in its `README.LLVM`.

M2 links STP 2.3.3 revision
`1bdfe5076c3f78e2bb7fccc5efb1a2a36c4068e6` through its public C API. STP is
distributed under the MIT license; the upstream copyright/license notice is
preserved in its installed public header and source distribution. The locked
build links MiniSat revision `14c78206cd12d1d36b7e042fa758747c135670a4`
as its SAT backend and includes STP's pinned ABC dependency; their upstream
license notices must accompany binary distributions.

No KLEE source or internal library is copied or linked. Formal Expr remains the
only project IR; all STP handles are private, query-scoped implementation
details. The `stp` executable is also invoked by tests as an independent
SMT-LIB parser/oracle.

The CI test image installs the Ubuntu `z3` 4.8.12-1 package and invokes its
executable only as an independent SMT-LIB differential oracle. Z3 is distributed
under the MIT license and is not linked into FormalPass production targets.
