#!/usr/bin/env python3
"""Replay a solver witnessed M7 error through the same native source."""

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
    parser.add_argument("--entry", required=True)
    parser.add_argument("--reason", required=True)
    parser.add_argument("--model", default="free")
    parser.add_argument("--allow-nonheap-error", action="store_true")
    parser.add_argument("--asan-phrase", required=True)
    parser.add_argument("--evidence-dir")
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m7-heap-replay-") as root:
        root = pathlib.Path(root)
        ir = root / "heap.ll"
        compiled = subprocess.run(
            [args.clang, "-O0", "-g", "-fno-strict-overflow",
             "-fno-discard-value-names", "-S", "-emit-llvm", "-I",
             args.include, args.source, "-o", str(ir)],
            capture_output=True, text=True)
        assert compiled.returncode == 0, compiled.stderr
        artifacts = root / "artifacts"
        symbolic = subprocess.run(
            [args.executor, str(ir), "--m5", "--entry",
             args.entry, "--output-dir", str(artifacts)],
            capture_output=True, text=True)
        assert symbolic.returncode == 1, symbolic.stderr + symbolic.stdout
        assert "M5 status UNSAFE" in symbolic.stdout, symbolic.stdout
        result = json.loads((artifacts / "result.json").read_text())
        assert result["status"] == "UNSAFE", result
        error_path = next(
            path for path in result["paths"] if path["reason"] == args.reason
        )
        assert result["max_call_depth_observed"] >= 1, result
        assert error_path["call_stack"][0]["function"] == args.entry
        assert all(frame["frame_id"] > 0 for frame in error_path["call_stack"])
        if args.reason == "USE_AFTER_FREE":
            assert error_path["call_stack"][-1]["function"] == "readUnchecked"
        assert any(event["kind"] == "external_model" and
                   event["model"] == args.model
                   for event in error_path["events"])
        if not args.allow_nonheap_error:
            assert any(event["kind"] == "heap_error" and
                       event["object_id"] > 0
                       for event in error_path["events"])
        witness_path = artifacts / "counterexample.json"
        witness = json.loads(witness_path.read_text())
        assert witness["reason"] == args.reason, witness
        assert witness["inputs"] == [
            {"name": "choice", "bytes": [1]}], witness
        replay_env = os.environ.copy()
        replay_env["ASAN_OPTIONS"] = (
            "detect_leaks=0:abort_on_error=1:alloc_dealloc_mismatch=1")
        replay_env["UBSAN_OPTIONS"] = "halt_on_error=1"
        safe_input = root / "safe-input.json"
        safe_input.write_text(
            '{"inputs":[{"name":"choice","bytes":[0]}]}\n')
        safe_output = root / "safe-output.json"
        safe = subprocess.run(
            [args.fixture, str(safe_input), str(safe_output)],
            capture_output=True, text=True, env=replay_env)
        assert safe.returncode == 0, safe.stdout + safe.stderr
        assert json.loads(safe_output.read_text()) == {
            "status": "CAPTURED", "outputs": [
                {"name": "output", "bytes": [0]}]}
        replay = subprocess.run(
            [args.fixture, str(witness_path), str(root / "captured.json")],
            capture_output=True, text=True, env=replay_env)
        assert replay.returncode != 0, replay.stdout + replay.stderr
        assert args.asan_phrase in replay.stderr, replay.stderr
        save_replay_evidence(root, args.evidence_dir, "heap.ll", args.entry,
                             args.reason, args.asan_phrase, replay)
        print(f"M7 {args.reason} witness choice=1 reproduced by native ASan")


if __name__ == "__main__":
    main()
