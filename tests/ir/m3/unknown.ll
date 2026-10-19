; RUN: %scalar-executor %s --entry div_zero | %FileCheck %s --check-prefix=DIVZERO
; RUN: %scalar-executor %s --entry sdiv_overflow | %FileCheck %s --check-prefix=SDIV
; RUN: %scalar-executor %s --entry overshift | %FileCheck %s --check-prefix=SHIFT
; RUN: %scalar-executor %s --entry symbolic_div | %FileCheck %s --check-prefix=SYMDIV
; RUN: %scalar-executor %s --entry symbolic_sdiv | %FileCheck %s --check-prefix=SYMSDIV
; RUN: %scalar-executor %s --entry symbolic_shift | %FileCheck %s --check-prefix=SYMSHIFT
; RUN: %scalar-executor %s --entry flagged | %FileCheck %s --check-prefix=FLAG
; RUN: %scalar-executor %s --entry flagged_nuw | %FileCheck %s --check-prefix=NUW
; RUN: %scalar-executor %s --entry exact | %FileCheck %s --check-prefix=EXACT
; RUN: %scalar-executor %s --entry poison_annotation | %FileCheck %s --check-prefix=ANNOTATION
; RUN: %scalar-executor %s --entry frozen | %FileCheck %s --check-prefix=FREEZE
; RUN: %scalar-executor %s --entry poison_value | %FileCheck %s --check-prefix=POISON
; RUN: %scalar-executor %s --entry undef_value | %FileCheck %s --check-prefix=UNDEF
; RUN: %scalar-executor %s --entry constant_expr | %FileCheck %s --check-prefix=CONST
; RUN: %scalar-executor %s --entry memory | %FileCheck %s --check-prefix=MEMORY
; RUN: %scalar-executor %s --entry load_op | %FileCheck %s --check-prefix=LOAD
; RUN: %scalar-executor %s --entry store_op | %FileCheck %s --check-prefix=STORE
; RUN: %scalar-executor %s --entry gep_op | %FileCheck %s --check-prefix=GEP
; RUN: %scalar-executor %s --entry ptr_cast | %FileCheck %s --check-prefix=CAST
; RUN: %scalar-executor %s --entry call_op | %FileCheck %s --check-prefix=CALL
; RUN: %scalar-executor %s --entry invoke_op | %FileCheck %s --check-prefix=INVOKE
; RUN: %scalar-executor %s --entry atomic_op | %FileCheck %s --check-prefix=ATOMIC
; RUN: %scalar-executor %s --entry floating | %FileCheck %s --check-prefix=FLOAT
; RUN: %scalar-executor %s --entry vector | %FileCheck %s --check-prefix=VECTOR
; RUN: %scalar-executor %s --entry aggregate | %FileCheck %s --check-prefix=AGGREGATE
; RUN: %scalar-executor %s --entry indirect | %FileCheck %s --check-prefix=OPCODE
; RUN: %scalar-executor %s --entry loop | %FileCheck %s --check-prefix=LOOP
; RUN: %scalar-executor %s --entry timed --timeout-ms 0 | %FileCheck %s --check-prefix=TIMEOUT
; RUN: %scalar-executor %s --entry reached_unreachable | %FileCheck %s --check-prefix=UNREACHABLE

define i8 @div_zero() {
entry:
  %r = udiv i8 1, 0
  ret i8 %r
}
; DIVZERO: M3 status UNKNOWN
; DIVZERO: reason=UNKNOWN-DIVIDE-BY-ZERO

define i8 @sdiv_overflow() {
entry:
  %r = sdiv i8 -128, -1
  ret i8 %r
}
; SDIV: M3 status UNKNOWN
; SDIV: reason=UNKNOWN-SDIV-OVERFLOW

define i8 @overshift() {
entry:
  %r = shl i8 1, 8
  ret i8 %r
}
; SHIFT: reason=UNKNOWN-OVERSHIFT

define i8 @symbolic_div(i8 %x, i8 %y) {
entry:
  %r = udiv i8 %x, %y
  ret i8 %r
}
; SYMDIV: M3 status UNKNOWN
; SYMDIV: M3 paths 2
; SYMDIV-DAG: status=COMPLETED
; SYMDIV-DAG: reason=UNKNOWN-DIVIDE-BY-ZERO

define i8 @symbolic_sdiv(i8 %x, i8 %y) {
entry:
  %r = sdiv i8 %x, %y
  ret i8 %r
}
; SYMSDIV: M3 status UNKNOWN
; SYMSDIV: M3 paths 3
; SYMSDIV-DAG: reason=UNKNOWN-DIVIDE-BY-ZERO
; SYMSDIV-DAG: reason=UNKNOWN-SDIV-OVERFLOW
; SYMSDIV-DAG: status=COMPLETED

define i8 @symbolic_shift(i8 %x, i8 %amount) {
entry:
  %r = lshr i8 %x, %amount
  ret i8 %r
}
; SYMSHIFT: M3 status UNKNOWN
; SYMSHIFT: M3 paths 2
; SYMSHIFT-DAG: status=COMPLETED
; SYMSHIFT-DAG: reason=UNKNOWN-OVERSHIFT

define i8 @flagged(i8 %x) {
entry:
  %r = add nsw i8 %x, 1
  ret i8 %r
}
; FLAG: reason=UNKNOWN-NSW-NUW

define i8 @flagged_nuw(i8 %x) {
entry:
  %r = add nuw i8 %x, 1
  ret i8 %r
}
; NUW: reason=UNKNOWN-NSW-NUW

define i8 @exact(i8 %x) {
entry:
  %r = udiv exact i8 %x, 2
  ret i8 %r
}
; EXACT: reason=UNKNOWN-EXACT

define i8 @poison_annotation(i8 %x) {
entry:
  %r = or disjoint i8 %x, 1
  ret i8 %r
}
; ANNOTATION: reason=UNKNOWN-POISON-ANNOTATION

define i8 @frozen(i8 %x) {
entry:
  %r = freeze i8 %x
  ret i8 %r
}
; FREEZE: reason=UNKNOWN-FREEZE

define i8 @poison_value() {
entry:
  ret i8 poison
}
; POISON: reason=UNKNOWN-POISON

define i8 @undef_value() {
entry:
  ret i8 undef
}
; UNDEF: reason=UNKNOWN-UNDEF

@external_byte = external global i8

define i64 @constant_expr() {
entry:
  ret i64 ptrtoint (ptr @external_byte to i64)
}
; CONST: reason=UNKNOWN-UNSUPPORTED-CONSTANT
; CONST-SAME: location=constant_expr:entry:ret#0

define i8 @memory() {
entry:
  %p = alloca i8
  ret i8 0
}
; MEMORY: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION
; MEMORY-SAME: location=memory:entry:alloca#0

define i8 @load_op() {
entry:
  %r = load i8, ptr @external_byte
  ret i8 %r
}
; LOAD: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION
; LOAD-SAME: location=load_op:entry:load#0

define void @store_op(i8 %x) {
entry:
  store i8 %x, ptr @external_byte
  ret void
}
; STORE: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION
; STORE-SAME: location=store_op:entry:store#0

define i8 @gep_op() {
entry:
  %r = getelementptr i8, ptr @external_byte, i64 1
  ret i8 0
}
; GEP: reason=UNKNOWN-UNSUPPORTED-MEMORY-INSTRUCTION
; GEP-SAME: location=gep_op:entry:getelementptr#0

define i64 @ptr_cast() {
entry:
  %r = ptrtoint ptr null to i64
  ret i64 %r
}
; CAST: reason=UNKNOWN-UNSUPPORTED-CAST
; CAST-SAME: location=ptr_cast:entry:ptrtoint#0

declare i8 @external_call()

define i8 @call_op() {
entry:
  %r = call i8 @external_call()
  ret i8 %r
}
; CALL: reason=UNKNOWN-UNSUPPORTED-CALL
; CALL-SAME: location=call_op:entry:call#0

declare i32 @__gxx_personality_v0(...)

define i8 @invoke_op() personality ptr @__gxx_personality_v0 {
entry:
  %r = invoke i8 @external_call() to label %ok unwind label %lpad
ok:
  ret i8 %r
lpad:
  %lp = landingpad { ptr, i32 } cleanup
  resume { ptr, i32 } %lp
}
; INVOKE: reason=UNKNOWN-UNSUPPORTED-EXCEPTION-INSTRUCTION
; INVOKE-SAME: location=invoke_op:entry:invoke#0

define i8 @atomic_op() {
entry:
  %r = load atomic i8, ptr @external_byte seq_cst, align 1
  ret i8 %r
}
; ATOMIC: reason=UNKNOWN-UNSUPPORTED-ATOMIC-INSTRUCTION
; ATOMIC-SAME: location=atomic_op:entry:load#0

define i32 @floating() {
entry:
  %x = fadd float 1.0, 2.0
  %r = fptoui float %x to i32
  ret i32 %r
}
; FLOAT: reason=UNKNOWN-UNSUPPORTED-FLOATING-POINT
; FLOAT-SAME: location=floating:entry:fadd#0

define i8 @vector() {
entry:
  %x = add <2 x i8> <i8 1, i8 2>, <i8 3, i8 4>
  %r = extractelement <2 x i8> %x, i32 0
  ret i8 %r
}
; VECTOR: reason=UNKNOWN-UNSUPPORTED-VECTOR
; VECTOR-SAME: location=vector:entry:add#0

define i8 @aggregate() {
entry:
  %x = insertvalue { i8, i8 } poison, i8 1, 0
  %r = extractvalue { i8, i8 } %x, 0
  ret i8 %r
}
; AGGREGATE: reason=UNKNOWN-UNSUPPORTED-AGGREGATE
; AGGREGATE-SAME: location=aggregate:entry:insertvalue#0

define void @indirect() {
entry:
  indirectbr ptr blockaddress(@indirect, %target), [label %target]
target:
  ret void
}
; OPCODE: reason=UNKNOWN-UNSUPPORTED-INSTRUCTION
; OPCODE-SAME: location=indirect:entry:indirectbr#0

define void @loop() {
entry:
  br label %body
body:
  br label %body
}
; LOOP: reason=UNKNOWN-M3-LOOP

define i8 @timed(i8 %x) {
entry:
  %c = icmp eq i8 %x, 0
  br i1 %c, label %yes, label %no
yes:
  ret i8 1
no:
  ret i8 0
}
; TIMEOUT: M3 status UNKNOWN
; TIMEOUT: M3 paths 2
; TIMEOUT: reason=UNKNOWN-SOLVER-TIMEOUT
; TIMEOUT: reason=UNKNOWN-SOLVER-TIMEOUT
; TIMEOUT: M3 stats states=2 forks=1 pruned=0 instructions=2 queries=2

define void @reached_unreachable() {
entry:
  unreachable
}
; UNREACHABLE: M3 status ERROR
; UNREACHABLE: reason=REACHED-UNREACHABLE
