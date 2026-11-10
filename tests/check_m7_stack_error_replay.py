#!/usr/bin/env python3
"""Replay an interprocedural stack lifetime witness with native ASan."""

import argparse
import json
import os
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory
from m7_replay_evidence import save_replay_evidence


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--fixture", required=True)
    parser.add_argument("--clang", required=True)
    parser.add_argument("--source", required=True)
    parser.add_argument("--include", required=True)
    parser.add_argument("--evidence-dir")
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m7-stack-replay-") as root:
        root = pathlib.Path(root)
        ir = root / "stack.ll"
        compiled = subprocess.run(
            [args.clang, "-O0", "-g", "-fno-strict-overflow",
             "-fno-discard-value-names", "-S", "-emit-llvm", "-I",
             args.include, args.source, "-o", str(ir)],
            capture_output=True, text=True)
        assert compiled.returncode == 0, compiled.stderr
        artifacts = root / "artifacts"
        symbolic = subprocess.run(
            [args.executor, str(ir), "--entry", "replayUseAfterReturnTop",
             "--output-dir", str(artifacts)],
            capture_output=True, text=True)
        assert symbolic.returncode == 1, symbolic.stdout + symbolic.stderr
        report = json.loads((artifacts / "result.json").read_text())
        assert report["status"] == "UNSAFE", report
        error = next(path for path in report["paths"] if
                     path["reason"] == "DEAD_OBJECT_ACCESS")
        assert error["source"]["opcode"] == "load", error
        assert error["call_stack"][0]["function"] == "replayUseAfterReturnTop"
        assert "readEscaped" in error["call_stack"][-1]["function"]
        allocations = {event["object_id"]: event for event in error["events"]
                       if event["kind"] == "stack_allocation"}
        assert any(event["kind"] == "lifetime_end" and
                   event["object_id"] in allocations and
                   event["frame_id"] ==
                   allocations[event["object_id"]]["frame_id"]
                   for event in error["events"]), error
        witness_path = artifacts / "counterexample.json"
        witness = json.loads(witness_path.read_text())
        assert witness["reason"] == "DEAD_OBJECT_ACCESS", witness
        assert witness["inputs"] == [
            {"name": "choice", "bytes": [1]}], witness
        environment = os.environ.copy()
        environment["ASAN_OPTIONS"] = (
            "detect_stack_use_after_return=1:detect_leaks=0:abort_on_error=1")
        environment["UBSAN_OPTIONS"] = "halt_on_error=1"
        safe_input = root / "safe-input.json"
        safe_input.write_text(
            '{"inputs":[{"name":"choice","bytes":[0]}]}\n')
        safe_output = root / "safe-output.json"
        safe = subprocess.run(
            [args.fixture, str(safe_input), str(safe_output)],
            capture_output=True, text=True, env=environment)
        assert safe.returncode == 0, safe.stdout + safe.stderr
        assert json.loads(safe_output.read_text()) == {
            "status": "CAPTURED", "outputs": [
                {"name": "output", "bytes": [0]}]}
        replay = subprocess.run(
            [args.fixture, str(witness_path), str(root / "captured.json")],
            capture_output=True, text=True, env=environment)
        assert replay.returncode != 0, replay.stdout + replay.stderr
        assert "stack-use-after-return" in replay.stderr, replay.stderr
        save_replay_evidence(root, args.evidence_dir, "stack.ll",
                             "replayUseAfterReturnTop", "DEAD_OBJECT_ACCESS",
                             "stack-use-after-return", replay)
        print("M7 DEAD_OBJECT_ACCESS witness reproduced by native ASan "
              "stack-use-after-return")


if __name__ == "__main__":
    main()
