; Derived from pinned LLVM LangRef (revision eccf4d44d346eee498b0ff709e625e3104448751):
; udiv/sdiv/srem, shl, add flags, freeze, poison/undef, inbounds GEP.
; RUN: %scalar-executor %s --m5 --entry guarded_div --output-dir %t.guarded | %FileCheck %s --check-prefix=GUARDED
; RUN: %not %scalar-executor %s --m5 --entry zero_div --output-dir %t.zero | %FileCheck %s --check-prefix=ZERO
; RUN: %not %scalar-executor %s --m5 --entry signed_overflow --output-dir %t.signed | %FileCheck %s --check-prefix=SIGNED
; RUN: %not %scalar-executor %s --m5 --entry signed_div_overflow --output-dir %t.sdiv | %FileCheck %s --check-prefix=SDIV
; RUN: %not %scalar-executor %s --m5 --entry symbolic_div --output-dir %t.symbolic | %FileCheck %s --check-prefix=SYMBOLIC
; RUN: %scalar-executor %s --m5 --entry overshift --output-dir %t.shift | %FileCheck %s --check-prefix=SHIFT
; RUN: %not %scalar-executor %s --m5 --entry symbolic_shift_branch --output-dir %t.shiftbranch | %FileCheck %s --check-prefix=SHIFTBRANCH
; RUN: %scalar-executor %s --m5 --entry flagged --output-dir %t.flag | %FileCheck %s --check-prefix=FLAG
; RUN: %scalar-executor %s --m5 --entry flagged_nuw --output-dir %t.nuw | %FileCheck %s --check-prefix=NUW
; RUN: %not %scalar-executor %s --m5 --entry symbolic_nsw_branch --output-dir %t.nswbranch | %FileCheck %s --check-prefix=NSWBRANCH
; RUN: %not %scalar-executor %s --m5 --entry width_i1_nuw --output-dir %t.i1 | %FileCheck %s --check-prefix=I1NUW
; RUN: %not %scalar-executor %s --m5 --entry width_i24_nsw --output-dir %t.i24 | %FileCheck %s --check-prefix=I24NSW
; RUN: %not %scalar-executor %s --m5 --entry width_i64_nsw --output-dir %t.i64 | %FileCheck %s --check-prefix=I64NSW
; RUN: %not %scalar-executor %s --m5 --entry width_i128_nsw --output-dir %t.i128 | %FileCheck %s --check-prefix=I128NSW
; RUN: %not %scalar-executor %s --m5 --entry width_add_over_limit --output-dir %t.addlimit | %FileCheck %s --check-prefix=ADDLIMIT
; RUN: %not %scalar-executor %s --m5 --entry width_mul_over_limit --output-dir %t.mullimit | %FileCheck %s --check-prefix=MULLIMIT
; RUN: %scalar-executor %s --m5 --entry unselected_poison_select --output-dir %t.selectok | %FileCheck %s --check-prefix=SELECTOK
; RUN: %scalar-executor %s --m5 --entry unselected_poison_phi --output-dir %t.phiok | %FileCheck %s --check-prefix=PHIOK
; RUN: %scalar-executor %s --m5 --entry exact_div --output-dir %t.exact | %FileCheck %s --check-prefix=EXACT
; RUN: %not %scalar-executor %s --m5 --entry symbolic_exact_branch --output-dir %t.exactbranch | %FileCheck %s --check-prefix=EXACTBRANCH
; RUN: %not %scalar-executor %s --m5 --entry undef_store --output-dir %t.undef | %FileCheck %s --check-prefix=UNDEF
; RUN: %not %scalar-executor %s --m5 --entry undef_independent --output-dir %t.undefuses | %FileCheck %s --check-prefix=UNDEFUSES
; RUN: %not %scalar-executor %s --m5 --entry poison_branch --output-dir %t.poison | %FileCheck %s --check-prefix=POISON
; RUN: %not %scalar-executor %s --m5 --entry poison_store --output-dir %t.poisonstore | %FileCheck %s --check-prefix=PSTORE
; RUN: %not %scalar-executor %s --m5 --entry poison_arithmetic --output-dir %t.poisonarith | %FileCheck %s --check-prefix=PARITH
; RUN: %scalar-executor %s --entry poison_return | %FileCheck %s --check-prefix=PRETURN
; RUN: %scalar-executor %s --entry poison_noundef_return | %FileCheck %s --check-prefix=NOUNDEFRET
; RUN: %not %scalar-executor %s --m5 --entry poison_noundef_call --output-dir %t.noundefcall | %FileCheck %s --check-prefix=NOUNDEFCALL
; RUN: %not %scalar-executor %s --m5 --entry symbolic_noundef_call --output-dir %t.symboliccall | %FileCheck %s --check-prefix=SYMBOLICCALL
; RUN: %not %scalar-executor %s --m5 --entry callsite_noundef_call --output-dir %t.callsitecall | %FileCheck %s --check-prefix=CALLSITECALL
; RUN: %scalar-executor %s --m5 --entry poison_ignored_call --output-dir %t.ignoredcall | %FileCheck %s --check-prefix=IGNOREDCALL
; RUN: %not %scalar-executor %s --m5 --entry poison_used_call --output-dir %t.usedcall | %FileCheck %s --check-prefix=USEDCALL
; RUN: %scalar-executor %s --m5 --entry frozen --output-dir %t.freeze | %FileCheck %s --check-prefix=FREEZE
; RUN: %not %scalar-executor %s --m5 --entry frozen --max-states 100 --output-dir %t.freezecap | %FileCheck %s --check-prefix=FREEZECAP
; RUN: %scalar-executor %s --m5 --entry frozen_reused --output-dir %t.freezereused | %FileCheck %s --check-prefix=FREEZEREUSED
; RUN: %scalar-executor %s --m5 --entry frozen_undef --output-dir %t.freezeundef | %FileCheck %s --check-prefix=FREEZEUNDEF
; RUN: %scalar-executor %s --m5 --entry frozen_independent --output-dir %t.freezeindependent | %FileCheck %s --check-prefix=FREEZEINDEPENDENT
; RUN: %scalar-executor %s --m5 --entry frozen_loop --output-dir %t.freezeloop | %FileCheck %s --check-prefix=FREEZELOOP
; RUN: %scalar-executor %s --m5 --entry frozen_recursive --output-dir %t.freezerecursive | %FileCheck %s --check-prefix=FREEZERECURSIVE
; RUN: %scalar-executor %s --m5 --entry frozen_store --output-dir %t.freezestore | %FileCheck %s --check-prefix=FREEZESTORE
; RUN: %not %scalar-executor %s --m5 --entry frozen_wide --output-dir %t.freezewide | %FileCheck %s --check-prefix=FREEZEWIDE
; RUN: %scalar-executor %s --m5 --entry frozen_conditional --output-dir %t.freezeconditional | %FileCheck %s --check-prefix=FREEZECONDITIONAL
; RUN: %not %scalar-executor %s --m5 --entry frozen_conditional --max-solver-queries 0 --output-dir %t.freezequery | %FileCheck %s --check-prefix=FREEZEQUERY
; RUN: %scalar-executor %s --m5 --entry defined_freeze --output-dir %t.definedfreeze | %FileCheck %s --check-prefix=DEFINEDFREEZE
; RUN: %scalar-executor %s --m5 --entry one_past --output-dir %t.onepast | %FileCheck %s --check-prefix=ONEPAST
; RUN: %not %scalar-executor %s --m5 --entry one_past_load --output-dir %t.onepastload | %FileCheck %s --check-prefix=ONEPASTLOAD
; RUN: %not %scalar-executor %s --m5 --entry one_past_store --output-dir %t.onepaststore | %FileCheck %s --check-prefix=ONEPASTSTORE
; RUN: %not %scalar-executor %s --m5 --entry past_end --output-dir %t.past | %FileCheck %s --check-prefix=PAST
; RUN: %not %scalar-executor %s --m5 --entry wide_index --output-dir %t.wide | %FileCheck %s --check-prefix=WIDE
; RUN: %not %scalar-executor %s --m5 --entry outside_reenter --output-dir %t.reenter | %FileCheck %s --check-prefix=REENTER
; RUN: python3 %S/../../check_m9_langref_artifacts.py %t.symbolic %t.undef %t.undefuses %t.poisonstore %t.past %t.wide %t.reenter

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
@x = private constant [2 x i8] c"x\00"
@y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)

define void @guarded_div() {
entry:
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %v = load i8, ptr %src, align 1
  %zero = icmp eq i8 %v, 0
  br i1 %zero, label %skip, label %divide
skip:
  store i8 0, ptr %dst, align 1
  ret void
divide:
  %r = udiv i8 12, %v
  store i8 %r, ptr %dst, align 1
  ret void
}
; GUARDED: M5 status SAFE

define void @zero_div() {
  %dst = alloca i8, align 1
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %r = udiv i8 1, 0
  store i8 %r, ptr %dst, align 1
  ret void
}
; ZERO: M5 status UNSAFE-UB
; ZERO: reason=LLVM-UB-DIVIDE-BY-ZERO

define void @signed_overflow() {
  %r = srem i8 -128, -1
  ret void
}
; SIGNED: M5 status UNSAFE-UB
; SIGNED: reason=LLVM-UB-SREM-OVERFLOW

define void @signed_div_overflow() {
  %r = sdiv i8 -128, -1
  ret void
}
; SDIV: M5 status UNSAFE-UB
; SDIV: reason=LLVM-UB-SDIV-OVERFLOW

define void @symbolic_div() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %v = load i8, ptr %src, align 1
  %r = udiv i8 12, %v
  store i8 %r, ptr %dst, align 1
  ret void
}
; SYMBOLIC: M5 status UNSAFE-UB
; SYMBOLIC: reason=LLVM-UB-DIVIDE-BY-ZERO

define void @overshift() {
  %r = shl i8 1, 8
  ret void
}
; SHIFT: M5 status SAFE

define void @symbolic_shift_branch() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %amount = load i8, ptr %src, align 1
  %v = shl i8 1, %amount
  %zero = icmp eq i8 %v, 0
  br i1 %zero, label %yes, label %no
yes:
  store i8 1, ptr %dst, align 1
  ret void
no:
  store i8 0, ptr %dst, align 1
  ret void
}
; SHIFTBRANCH: M5 status UNSAFE-UB
; SHIFTBRANCH: reason=LLVM-UB-POISON-CONSUMED

define void @flagged() {
  %r = add nsw i8 127, 1
  ret void
}
; FLAG: M5 status SAFE

define void @flagged_nuw() {
  %r = add nuw i8 255, 1
  ret void
}
; NUW: M5 status SAFE

; LangRef add nsw: overflow poisons the result; consuming it as a branch
; condition is immediate UB. For x != 127, the normal path remains feasible.
define void @symbolic_nsw_branch() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %v = load i8, ptr %src, align 1
  %sum = add nsw i8 %v, 1
  %zero = icmp eq i8 %sum, 0
  br i1 %zero, label %yes, label %no
yes:
  store i8 1, ptr %dst, align 1
  ret void
no:
  store i8 0, ptr %dst, align 1
  ret void
}
; NSWBRANCH: M5 status UNSAFE-UB
; NSWBRANCH: reason=LLVM-UB-POISON-CONSUMED

define void @width_i1_nuw() {
  %v = add nuw i1 1, 1
  br i1 %v, label %done, label %done
done:
  ret void
}
; I1NUW: reason=LLVM-UB-POISON-CONSUMED

define void @width_i24_nsw() {
  %v = add nsw i24 8388607, 1
  %c = icmp eq i24 %v, 0
  br i1 %c, label %done, label %done
done:
  ret void
}
; I24NSW: reason=LLVM-UB-POISON-CONSUMED

define void @width_i64_nsw() {
  %v = add nsw i64 9223372036854775807, 1
  %c = icmp eq i64 %v, 0
  br i1 %c, label %done, label %done
done:
  ret void
}
; I64NSW: reason=LLVM-UB-POISON-CONSUMED

define void @width_i128_nsw() {
  %v = add nsw i128 170141183460469231731687303715884105727, 1
  %c = icmp eq i128 %v, 0
  br i1 %c, label %done, label %done
done:
  ret void
}
; I128NSW: reason=LLVM-UB-POISON-CONSUMED

; LangRef select propagates poison only from the chosen arm.
define void @unselected_poison_select() {
  %bad = add nsw i8 127, 1
  %v = select i1 false, i8 %bad, i8 5
  %ok = icmp eq i8 %v, 5
  br i1 %ok, label %done, label %fail
fail:
  unreachable
done:
  ret void
}
; SELECTOK: M5 status SAFE

; A PHI uses only the predecessor actually taken.
define void @unselected_poison_phi() {
entry:
  br i1 false, label %poisoned, label %good
poisoned:
  %bad = add nsw i8 127, 1
  br label %join
good:
  br label %join
join:
  %v = phi i8 [ %bad, %poisoned ], [ 5, %good ]
  %ok = icmp eq i8 %v, 5
  br i1 %ok, label %done, label %fail
fail:
  unreachable
done:
  ret void
}
; PHIOK: M5 status SAFE

define void @exact_div() {
  %r = udiv exact i8 3, 2
  ret void
}
; EXACT: M5 status SAFE

define void @symbolic_exact_branch() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %v = load i8, ptr %src, align 1
  %q = udiv exact i8 %v, 2
  %zero = icmp eq i8 %q, 0
  br i1 %zero, label %yes, label %no
yes:
  store i8 1, ptr %dst, align 1
  ret void
no:
  store i8 0, ptr %dst, align 1
  ret void
}
; EXACTBRANCH: M5 status UNSAFE-UB
; EXACTBRANCH: reason=LLVM-UB-POISON-CONSUMED

define void @undef_store() {
  %dst = alloca i8, align 1
  store i8 undef, ptr %dst, align 1
  ret void
}
; UNDEF: M5 status UNKNOWN
; UNDEF: reason=UNKNOWN-UNDEF

define void @undef_independent() {
  %a = icmp eq i8 undef, 0
  %b = icmp eq i8 undef, 1
  %both = and i1 %a, %b
  br i1 %both, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; UNDEFUSES: M5 status UNKNOWN
; UNDEFUSES: reason=UNKNOWN-UNDEF

define void @poison_branch() {
  br i1 poison, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; POISON: M5 status UNSAFE-UB
; POISON: reason=LLVM-UB-POISON-CONSUMED

define void @poison_store() {
  %p = alloca i8, align 1
  store i8 poison, ptr %p, align 1
  ret void
}
; PSTORE: M5 status UNKNOWN
; PSTORE: reason=UNKNOWN-POISON

define void @poison_arithmetic() {
  %v = add i8 poison, 1
  %condition = icmp eq i8 %v, 0
  br i1 %condition, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; PARITH: M5 status UNSAFE-UB
; PARITH: reason=LLVM-UB-POISON-CONSUMED

define i8 @poison_return() {
  ret i8 poison
}
; PRETURN: M3 status M3_EXECUTOR_COMPLETE
; PRETURN: return=poison

define noundef i8 @poison_noundef_return() {
  ret i8 poison
}
; NOUNDEFRET: reason=LLVM-UB-POISON-CONSUMED

define internal void @noundef_sink(i8 noundef %x) {
  ret void
}

define void @poison_noundef_call() {
  call void @noundef_sink(i8 poison)
  ret void
}
; NOUNDEFCALL: M5 status UNSAFE-UB
; NOUNDEFCALL: reason=LLVM-UB-POISON-CONSUMED

; The nsw overflow is feasible only for x=127; the defined call still runs
; for all other inputs (LangRef poison/noundef parameter rules).
define void @symbolic_noundef_call() {
  %src = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  %v = load i8, ptr %src, align 1
  %sum = add nsw i8 %v, 1
  call void @noundef_sink(i8 %sum)
  ret void
}
; SYMBOLICCALL: M5 status UNSAFE-UB
; SYMBOLICCALL: reason=LLVM-UB-POISON-CONSUMED

define internal void @ignored_arg(i8 %x) {
  ret void
}

define void @poison_ignored_call() {
  call void @ignored_arg(i8 poison)
  ret void
}
; IGNOREDCALL: M5 status SAFE

define internal void @branch_on_arg(i1 %x) {
  br i1 %x, label %done, label %done
done:
  ret void
}

define void @poison_used_call() {
  call void @branch_on_arg(i1 poison)
  ret void
}
; USEDCALL: M5 status UNSAFE-UB
; USEDCALL: reason=LLVM-UB-POISON-CONSUMED

define void @callsite_noundef_call() {
  call void @ignored_arg(i8 noundef poison)
  ret void
}
; CALLSITECALL: M5 status UNSAFE-UB
; CALLSITECALL: reason=LLVM-UB-POISON-CONSUMED

define void @frozen() {
  %v = freeze i8 poison
  ret void
}
; FREEZE: M5 status SAFE
; FREEZECAP: M5 status UNKNOWN
; FREEZECAP: reason=UNKNOWN-STATE-LIMIT

define void @frozen_reused() {
  %v = freeze i8 poison
  %a = add i8 %v, 1
  %b = add i8 %v, 1
  %same = icmp eq i8 %a, %b
  br i1 %same, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; FREEZEREUSED: M5 status SAFE

define void @frozen_undef() {
  %v = freeze i1 undef
  %same = icmp eq i1 %v, %v
  br i1 %same, label %done, label %done
done:
  ret void
}
; FREEZEUNDEF: M5 status SAFE

define void @frozen_independent() {
  %a = freeze i1 poison
  %b = freeze i1 poison
  %same = icmp eq i1 %a, %b
  br i1 %same, label %done, label %done
done:
  ret void
}
; FREEZEINDEPENDENT: M5 status SAFE

define void @frozen_loop() {
entry:
  br label %loop
loop:
  %i = phi i2 [ 0, %entry ], [ %next, %loop ]
  %previous = phi i1 [ false, %entry ], [ %choice, %loop ]
  %choice = freeze i1 poison
  %next = add i2 %i, 1
  %again = icmp ult i2 %next, 2
  br i1 %again, label %loop, label %done
done:
  %same = icmp eq i1 %previous, %choice
  br i1 %same, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; FREEZELOOP: M5 status SAFE

define internal i1 @freeze_recur(i1 %base) {
  %choice = freeze i1 poison
  br i1 %base, label %done, label %recurse
recurse:
  %child = call i1 @freeze_recur(i1 true)
  %same = icmp eq i1 %choice, %child
  ret i1 %same
done:
  ret i1 %choice
}

define void @frozen_recursive() {
  %v = call i1 @freeze_recur(i1 false)
  br i1 %v, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; FREEZERECURSIVE: M5 status SAFE

define void @frozen_store() {
  %p = alloca i1, align 1
  %v = freeze i1 poison
  store i1 %v, ptr %p, align 1
  %loaded = load i1, ptr %p, align 1
  %same = icmp eq i1 %v, %loaded
  br i1 %same, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; FREEZESTORE: M5 status SAFE

define void @frozen_wide() {
  %v = freeze i16 poison
  ret void
}
; FREEZEWIDE: M5 status UNKNOWN
; FREEZEWIDE: reason=UNKNOWN-FREEZE

define void @frozen_conditional() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %v = load i8, ptr %src, align 1
  %bit = trunc i8 %v to i1
  %sum = add nuw i1 %bit, true
  %f = freeze i1 %sum
  %result = zext i1 %f to i8
  store i8 %result, ptr %dst, align 1
  %zero = icmp eq i1 %f, false
  br i1 %zero, label %yes, label %no
yes:
  ret void
no:
  ret void
}
; Pinned LangRef freeze: even x gives the defined value 1; odd x poisons
; add nuw, so this dynamic freeze may choose either 0 or 1, stably.
; FREEZECONDITIONAL: M5 status SAFE
; FREEZEQUERY: M5 status UNKNOWN
; FREEZEQUERY: reason=UNKNOWN-SOLVER-QUERY-LIMIT

; LangRef freeze: a defined operand is returned unchanged; both uses agree.
define void @defined_freeze() {
  %src = alloca i8, align 1
  %dst = alloca i8, align 1
  call void @__formal_register_input(ptr %src, i64 1, ptr @x)
  call void @__formal_register_output(ptr %dst, i64 1, ptr @y)
  %v = load i8, ptr %src, align 1
  %f = freeze i8 %v
  %same = icmp eq i8 %f, %f
  br i1 %same, label %done, label %bad
bad:
  unreachable
done:
  store i8 %f, ptr %dst, align 1
  ret void
}
; DEFINEDFREEZE: M5 status SAFE

define void @one_past() {
  %p = alloca [1 x i8], align 1
  %q = getelementptr inbounds [1 x i8], ptr %p, i64 0, i64 1
  %eq = icmp eq ptr %q, %q
  br i1 %eq, label %done, label %done
done:
  ret void
}
; ONEPAST: M5 status SAFE

; LangRef permits one-past construction, but no byte of the object is
; accessible at that address.
define void @one_past_load() {
  %p = alloca [1 x i8], align 1
  %q = getelementptr inbounds [1 x i8], ptr %p, i64 0, i64 1
  %v = load i8, ptr %q, align 1
  ret void
}
; ONEPASTLOAD: M5 status UNSAFE
; ONEPASTLOAD: reason=OUT_OF_BOUNDS {{.*}}location={{.*}}:load#

define void @one_past_store() {
  %p = alloca [1 x i8], align 1
  %q = getelementptr inbounds [1 x i8], ptr %p, i64 0, i64 1
  store i8 7, ptr %q, align 1
  ret void
}
; ONEPASTSTORE: M5 status UNSAFE
; ONEPASTSTORE: reason=OUT_OF_BOUNDS {{.*}}location={{.*}}:store#

define void @past_end() {
  %p = alloca [1 x i8], align 1
  %q = getelementptr inbounds [1 x i8], ptr %p, i64 0, i64 2
  ret void
}
; PAST: M5 status UNKNOWN
; PAST: reason=UNKNOWN-INBOUNDS-GEP

define void @wide_index() {
  %p = alloca [1 x i8], align 1
  %q = getelementptr inbounds i8, ptr %p, i128 18446744073709551616
  ret void
}
; WIDE: M5 status UNKNOWN
; WIDE: reason=UNKNOWN-INBOUNDS-GEP

define void @outside_reenter() {
  %p = alloca [1 x i8], align 1
  %outside = getelementptr i8, ptr %p, i64 2
  %inside = getelementptr inbounds i8, ptr %outside, i64 -2
  ret void
}
; REENTER: M5 status UNKNOWN
; REENTER: reason=UNKNOWN-INBOUNDS-GEP

; The source types are valid. The internal nsw/nuw checks need one extra
; bit for add/sub and twice the source width for mul/shl.
define void @width_add_over_limit() {
  %r = add nsw i1048576 1, 2
  ret void
}
; ADDLIMIT: M5 status UNKNOWN
; ADDLIMIT: reason=UNKNOWN-EXPRESSION-WIDTH-LIMIT

define void @width_sub_over_limit() {
  %r = sub nuw i1048576 2, 1
  ret void
}

define void @width_mul_over_limit() {
  %r = mul nsw nuw i524289 2, 3
  ret void
}
; MULLIMIT: M5 status UNKNOWN
; MULLIMIT: reason=UNKNOWN-EXPRESSION-WIDTH-LIMIT

define void @width_shl_over_limit() {
  %r = shl nsw nuw i524289 1, 1
  ret void
}

define void @width_at_limit() {
  %a = add nsw i1048575 1, 2
  %b = sub nuw i1048575 2, 1
  %c = mul nsw nuw i524288 2, 3
  %d = shl nsw nuw i524288 1, 1
  %e = add i1048576 1, 2
  ret void
}
