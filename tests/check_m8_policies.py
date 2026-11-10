#!/usr/bin/env python3
"""Loop truncation policies never produce an unconditional SAFE."""

import argparse
import json
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--ir", required=True)
    parser.add_argument("--scalar-executor", required=True)
    parser.add_argument("--structure-ir", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m8-policies-") as temporary:
        root = pathlib.Path(temporary)
        source = pathlib.Path(args.ir).read_text()
        assert "br label %header, !llvm.loop !0" in source
        ir = root / "unannotated.ll"
        ir.write_text(source.replace(
            "br label %header, !llvm.loop !0", "br label %header"))
        for policy, status, code, conditional in (
                ("unknown", "UNKNOWN", 2, False),
                ("error", "INVALID_TASK", 3, False),
                ("assume-exit", "CONDITIONAL", 2, True)):
            output = root / policy
            run = subprocess.run([
                args.formal_check, str(ir), "--entry", "formal_top",
                "--allow-unversioned-ir", "--loop-bound", "2",
                "--loop-limit-policy", policy, "--output-dir", str(output),
            ], capture_output=True, text=True)
            assert run.returncode == code, (policy, run.returncode, run.stderr)
            result = json.loads((output / "result.json").read_text())
            assert result["status"] == status, result
            assert result["relation_completeness"] == "UNDER_APPROXIMATION", result
            assert result["unwinding_assumption"] is conditional, result
            assert result["paths"][0]["reason"] == "UNKNOWN-LOOP-BOUND", result
            if conditional:
                assert result["relation_metadata"]["unwinding_assumption"] is True
                assert "formal-unwinding-assumption: true" in (
                    output / "relation.smt2").read_text()
            scalar = subprocess.run([
                args.scalar_executor, args.structure_ir, "--entry",
                "infinite_loop", "--loop-bound", "0",
                "--loop-limit-policy", policy,
            ], capture_output=True, text=True)
            scalar_status = {
                "unknown": "UNKNOWN", "error": "INVALID_TASK",
                "assume-exit": "CONDITIONAL",
            }[policy]
            assert f"M3 status {scalar_status}" in scalar.stdout, scalar.stdout
            assert scalar.returncode == (0 if policy == "unknown" else code)


if __name__ == "__main__":
    main()
