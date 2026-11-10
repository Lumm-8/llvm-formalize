#!/usr/bin/env python3
"""Require an actual sanitizer replay from the exported M5 input model."""

import argparse
import json
import os
import pathlib
import subprocess
from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--fixture", required=True)
    parser.add_argument("--clang", required=True)
    parser.add_argument("--source", required=True)
    parser.add_argument("--include", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m5-error-") as root:
        root = pathlib.Path(root)
        ir = root / "replay_subject.ll"
        subprocess.run(
            [args.clang, "-O0", "-g", "-fno-strict-overflow",
             "-fno-discard-value-names", "-S",
             "-emit-llvm", "-I", args.include, args.source, "-o", str(ir)],
            check=True, capture_output=True, text=True)
        artifacts = root / "artifacts"
        symbolic = subprocess.run(
            [args.executor, str(ir), "--m5", "--entry", "replayErrorTop",
             "--output-dir", str(artifacts)], capture_output=True, text=True)
        assert symbolic.returncode == 1, symbolic.stderr + symbolic.stdout
        assert "M5 status UNSAFE" in symbolic.stdout, symbolic.stdout
        result = json.loads((artifacts / "result.json").read_text())
        assert result["status"] == "UNSAFE", result
        assert result["entry"] == "replayErrorTop", result
        witness_path = artifacts / "counterexample.json"
        witness = json.loads(witness_path.read_text())
        assert witness["reason"] == "OUT_OF_BOUNDS", witness
        assert witness["inputs"] == [{"name": "x", "bytes": [1]}], witness
        replay_env = os.environ.copy()
        replay_env["ASAN_OPTIONS"] = "detect_leaks=0:abort_on_error=1"
        # Let the expected UBSan bounds diagnostic continue to the expected
        # ASan stack overflow report for this single native replay.
        replay_env["UBSAN_OPTIONS"] = "halt_on_error=0:print_stacktrace=1"
        safe_input = root / "safe-input.json"
        safe_input.write_text('{"inputs":[{"name":"x","bytes":[0]}]}\n')
        safe_output = root / "safe-output.json"
        safe_replay = subprocess.run(
            [args.fixture, str(safe_input), str(safe_output)],
            capture_output=True, text=True, env=replay_env)
        assert safe_replay.returncode == 0, safe_replay.stdout + safe_replay.stderr
        assert json.loads(safe_output.read_text()) == {
            "status": "CAPTURED", "outputs": [{"name": "y", "bytes": [0]}]}
        replay = subprocess.run(
            [args.fixture, str(witness_path), str(root / "captured.json")],
            capture_output=True, text=True, env=replay_env)
        assert replay.returncode != 0, replay.stdout + replay.stderr
        assert "runtime error: index 1 out of bounds" in replay.stderr, replay.stderr
        assert "stack-buffer-overflow" in replay.stderr, replay.stderr
        print("same-source Clang IR error input x=1 reproduced by native ASan and UBSan")


if __name__ == "__main__":
    main()
