; RUN: rm -rf %t.dir && %opt -load-pass-plugin=%plugin -passes=formal-exec -formal-entry=formal_top -formal-output-dir=%t.dir %s -disable-output 2>&1 | %FileCheck %s --check-prefix=DIAG
; RUN: %FileCheck %s --check-prefix=JSON < %t.dir/runs/run-*/result.json

; DIAG: FormalPass: LLVM version '20.0.0git'
; DIAG: FormalPass: entry function 'formal_top'
; DIAG: FormalPass: output directory '{{.*}}m0-valid.ll.tmp.dir'
; DIAG: FormalPass: TargetTriple 'x86_64-unknown-linux-gnu'
; DIAG: FormalPass: DataLayout 'e-m:e-p:64:64-i64:64-n8:16:32:64-S128'
; JSON: "actual_llvm_revision": "eccf4d44d346eee498b0ff709e625e3104448751"
; JSON: "data_layout": "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
; JSON: "host_opt_path": "{{.*}}opt"
; JSON: "host_opt_sha256": "{{[0-9a-f]+}}"
; JSON: "host_opt_sha256_basis": "runtime-sha256-of-running-executable-image"
; JSON: "input_module_sha256": "{{[0-9a-f]+}}"
; JSON: "pinned_llvm_revision": "eccf4d44d346eee498b0ff709e625e3104448751"
; JSON: "pinned_llvm_version": "20.0.0git"
; JSON: "reason": "VALID"
; JSON: "run_id": "run-{{[A-Za-z0-9]+}}"
; JSON: "status": "VALIDATED"
; JSON: "target_triple": "x86_64-unknown-linux-gnu"
; JSON: "verdict": null

target datalayout = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define void @formal_top() {
entry:
  ret void
}

!llvm.ident = !{!0}
!0 = !{!"clang version 20.0.0git (https://github.com/llvm/llvm-project.git eccf4d44d346eee498b0ff709e625e3104448751)"}
