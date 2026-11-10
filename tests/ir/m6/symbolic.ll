; RUN: %scalar-executor %s --m5 --entry symbolic_memset --output-dir %t.set | %FileCheck %s --check-prefix=SET
; RUN: %scalar-executor %s --m5 --entry symbolic_struct_copy --output-dir %t.copy | %FileCheck %s --check-prefix=COPY
; RUN: %scalar-executor %s --m5 --entry undef_insert_output --output-dir %t.undef | %FileCheck %s --check-prefix=UNDEF
; RUN: %scalar-executor %s --m5 --entry flexible_registered --output-dir %t.flex | %FileCheck %s --check-prefix=FLEX
; RUN: %not %scalar-executor %s --m5 --entry flexible_registration_oob --output-dir %t.flex-oob | %FileCheck %s --check-prefix=FLEX-OOB
; RUN: %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-run-m5 -formal-entry=undef_insert_output -formal-output-dir=%t.plugin -disable-output %s 2>&1 | %FileCheck %s --check-prefix=PLUGIN

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"
%U = type { i8, i32 }
%F = type { i32, [0 x i8] }
@name_x = private constant [2 x i8] c"x\00"
@name_y = private constant [2 x i8] c"y\00"
declare void @__formal_register_input(ptr, i64, ptr)
declare void @__formal_register_output(ptr, i64, ptr)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define void @symbolic_memset() {
  %source = alloca i8, align 1
  %target = alloca [4 x i8], align 1
  call void @__formal_register_input(ptr %source, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %target, i64 4, ptr @name_y)
  %value = load i8, ptr %source, align 1
  call void @llvm.memset.p0.i64(ptr %target, i8 %value, i64 4, i1 false)
  ret void
}
; SET: M5 status SAFE

define void @symbolic_struct_copy() {
  %source = alloca %U, align 4
  %target = alloca %U, align 4
  %source_field = getelementptr %U, ptr %source, i64 0, i32 1
  %target_field = getelementptr %U, ptr %target, i64 0, i32 1
  call void @__formal_register_input(ptr %source_field, i64 4, ptr @name_x)
  call void @__formal_register_output(ptr %target_field, i64 4, ptr @name_y)
  call void @llvm.memcpy.p0.p0.i64(ptr %target, ptr %source, i64 8, i1 false)
  ret void
}
; COPY: M5 status SAFE

define void @undef_insert_output() {
  %target = alloca %U, align 4
  %field = getelementptr %U, ptr %target, i64 0, i32 1
  call void @__formal_register_output(ptr %field, i64 4, ptr @name_y)
  %a = insertvalue %U undef, i8 9, 0
  %b = insertvalue %U %a, i32 77, 1
  store %U %b, ptr %target, align 4
  ret void
}
; UNDEF: M5 status SAFE
; PLUGIN: FormalPass: M5 status SAFE

define void @flexible_registered() {
  %object = alloca [8 x i8], align 4
  %output = alloca i8, align 1
  %trailing = getelementptr %F, ptr %object, i64 0, i32 1, i64 3
  call void @__formal_register_input(ptr %trailing, i64 1, ptr @name_x)
  call void @__formal_register_output(ptr %output, i64 1, ptr @name_y)
  %value = load i8, ptr %trailing, align 1
  store i8 %value, ptr %output, align 1
  ret void
}
; FLEX: M5 status SAFE

define void @flexible_registration_oob() {
  %object = alloca %F, align 4
  %trailing = getelementptr %F, ptr %object, i64 0, i32 1, i64 3
  call void @__formal_register_input(ptr %trailing, i64 1, ptr @name_x)
  ret void
}
; FLEX-OOB: M5 status UNKNOWN
