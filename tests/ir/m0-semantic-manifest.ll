; RUN: rm -rf %t.eh && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=eh_entry -formal-output-dir=%t.eh %s -disable-output 2>&1 | %FileCheck %s --check-prefix=EH
; RUN: rm -rf %t.trap && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=trap_entry -formal-output-dir=%t.trap %s -disable-output 2>&1 | %FileCheck %s --check-prefix=TRAP
; RUN: rm -rf %t.intrinsic && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=unknown_intrinsic_entry -formal-output-dir=%t.intrinsic %s -disable-output 2>&1 | %FileCheck %s --check-prefix=INTRINSIC
; RUN: rm -rf %t.fp && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=fp_entry -formal-output-dir=%t.fp %s -disable-output 2>&1 | %FileCheck %s --check-prefix=FP
; RUN: rm -rf %t.vector && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=fixed_vector_entry -formal-output-dir=%t.vector %s -disable-output 2>&1 | %FileCheck %s --check-prefix=VECTOR
; RUN: rm -rf %t.freeze && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=freeze_entry -formal-output-dir=%t.freeze %s -disable-output 2>&1 | %FileCheck %s --check-prefix=FREEZE
; RUN: rm -rf %t.flags && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=flagged_entry -formal-output-dir=%t.flags %s -disable-output 2>&1 | %FileCheck %s --check-prefix=FLAGS
; RUN: rm -rf %t.poison && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=poison_entry -formal-output-dir=%t.poison %s -disable-output 2>&1 | %FileCheck %s --check-prefix=POISON
; RUN: rm -rf %t.disjoint && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=disjoint_entry -formal-output-dir=%t.disjoint %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DISJOINT
; RUN: rm -rf %t.samesign && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=samesign_entry -formal-output-dir=%t.samesign %s -disable-output 2>&1 | %FileCheck %s --check-prefix=SAMESIGN
; RUN: rm -rf %t.nneg && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=nneg_entry -formal-output-dir=%t.nneg %s -disable-output 2>&1 | %FileCheck %s --check-prefix=NNEG
; RUN: rm -rf %t.gepflags && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=gep_flags_entry -formal-output-dir=%t.gepflags %s -disable-output 2>&1 | %FileCheck %s --check-prefix=GEPFLAGS
; RUN: rm -rf %t.exact && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=exact_entry -formal-output-dir=%t.exact %s -disable-output 2>&1 | %FileCheck %s --check-prefix=EXACT
; RUN: rm -rf %t.nuw && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=nuw_entry -formal-output-dir=%t.nuw %s -disable-output 2>&1 | %FileCheck %s --check-prefix=NUW
; RUN: rm -rf %t.inbounds && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=inbounds_entry -formal-output-dir=%t.inbounds %s -disable-output 2>&1 | %FileCheck %s --check-prefix=INBOUNDS
; RUN: rm -rf %t.poisonmd && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=poison_metadata_entry -formal-output-dir=%t.poisonmd %s -disable-output 2>&1 | %FileCheck %s --check-prefix=POISONMD
; RUN: rm -rf %t.retattr && %not %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=return_attribute_entry -formal-output-dir=%t.retattr %s -disable-output 2>&1 | %FileCheck %s --check-prefix=RETATTR

; EH: UNKNOWN [EXCEPTION_HANDLING]
; TRAP: UNKNOWN [UNSUPPORTED_INTRINSIC]
; TRAP-SAME: llvm.trap
; INTRINSIC: UNKNOWN [UNSUPPORTED_INTRINSIC]
; INTRINSIC-SAME: llvm.assume
; FP: UNKNOWN [FLOATING_POINT]
; VECTOR: UNKNOWN [FIXED_VECTOR]
; FREEZE: UNKNOWN [FREEZE_INSTRUCTION]
; FLAGS: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; POISON: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; DISJOINT: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; SAMESIGN: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; NNEG: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; GEPFLAGS: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; EXACT: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; NUW: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; INBOUNDS: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; POISONMD: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]
; RETATTR: UNKNOWN [UNSUPPORTED_VALUE_SEMANTICS]

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare i32 @__gxx_personality_v0(...)
declare void @may_throw()
declare void @llvm.trap()
declare void @llvm.assume(i1)

@poison_metadata_byte = global i8 0


define void @eh_entry() personality ptr @__gxx_personality_v0 {
entry:
  invoke void @may_throw() to label %done unwind label %exception
done:
  ret void
exception:
  %landing = landingpad { ptr, i32 } cleanup
  resume { ptr, i32 } %landing
}

define void @trap_entry() {
  call void @llvm.trap()
  unreachable
}

define void @unknown_intrinsic_entry() {
  call void @llvm.assume(i1 true)
  ret void
}

define void @fp_entry() {
  %value = fadd float 1.0, 2.0
  ret void
}

define void @fixed_vector_entry() {
  %value = add <4 x i32> zeroinitializer, zeroinitializer
  ret void
}

define void @freeze_entry() {
  %value = freeze i32 poison
  ret void
}

define void @flagged_entry() {
  %value = add nsw i32 1, 2
  ret void
}

define void @poison_entry() {
  %value = add i32 poison, 1
  ret void
}

define void @disjoint_entry() {
  %value = or disjoint i32 1, 2
  ret void
}

define void @samesign_entry() {
  %value = icmp samesign ult i32 1, 2
  ret void
}

define void @nneg_entry() {
  %value = zext nneg i8 1 to i32
  ret void
}

define void @gep_flags_entry() {
  %nuw = getelementptr nuw i8, ptr null, i64 1
  %nusw = getelementptr nusw i8, ptr null, i64 1
  ret void
}

define void @exact_entry() {
  %value = udiv exact i32 4, 2
  ret void
}

define void @nuw_entry() {
  %value = add nuw i32 1, 2
  ret void
}

define void @inbounds_entry() {
  %value = getelementptr inbounds i8, ptr @poison_metadata_byte, i64 0
  ret void
}

define void @poison_metadata_entry() {
  %value = load i8, ptr @poison_metadata_byte, !range !0
  ret void
}

define i32 @return_attribute_callee() {
  ret i32 1
}

define void @return_attribute_entry() {
  %value = call range(i32 0, 2) i32 @return_attribute_callee()
  ret void
}

!0 = !{i8 0, i8 2}
