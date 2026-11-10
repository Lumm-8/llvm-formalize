#!/usr/bin/env python3
"""Replay a modeled abort or assertion witness through its native source."""

import argparse
import json
import os
import pathlib
import shutil
import signal
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--fixture", required=True)
    parser.add_argument("--clang", required=True)
    parser.add_argument("--source", required=True)
    parser.add_argument("--include", required=True)
    parser.add_argument("--entry", required=True)
    parser.add_argument("--reason", required=True)
    parser.add_argument("--model", required=True)
    parser.add_argument("--stderr-phrase")
    parser.add_argument("--evidence-dir", required=True)
    args = parser.parse_args()
    evidence = pathlib.Path(args.evidence_dir)
    evidence.mkdir(parents=True, exist_ok=True)
    (evidence / "verified.json").unlink(missing_ok=True)
    with RetainedTemporaryDirectory(prefix="formal-m7-termination-replay-") as root:
        root = pathlib.Path(root)
        ir = root / "termination.ll"
        compiled = subprocess.run(
            [args.clang, "-O0", "-g", "-fno-strict-overflow",
             "-fno-discard-value-names", "-S", "-emit-llvm", "-I",
             args.include, args.source, "-o", str(ir)],
            capture_output=True, text=True)
        assert compiled.returncode == 0, compiled.stderr
        artifacts = root / "artifacts"
        symbolic = subprocess.run(
            [args.executor, str(ir), "--m5", "--entry", args.entry,
             "--output-dir", str(artifacts)], capture_output=True, text=True)
        assert symbolic.returncode == 1, symbolic.stdout + symbolic.stderr
        report = json.loads((artifacts / "result.json").read_text())
        assert report["status"] == "UNSAFE", report
        error = next(path for path in report["paths"] if
                     path["reason"] == args.reason)
        assert error["source"]["opcode"] == "call", error
        assert error["call_stack"][0]["function"] == args.entry, error
        assert any(event["kind"] == "external_model" and
                   event["model"] == args.model for event in error["events"])
        witness_path = artifacts / "counterexample.json"
        witness = json.loads(witness_path.read_text())
        assert witness["reason"] == args.reason, witness
        assert witness["inputs"] == [
            {"name": "choice", "bytes": [1]}], witness
        environment = os.environ.copy()
        environment["ASAN_OPTIONS"] = "detect_leaks=0:abort_on_error=1"
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
        assert replay.returncode == -signal.SIGABRT, (
            replay.returncode, replay.stdout, replay.stderr)
        if args.stderr_phrase:
            assert args.stderr_phrase in replay.stderr, replay.stderr
        for source, target in (
                (ir, "program.ll"),
                (artifacts / "result.json", "result.json"),
                (witness_path, "counterexample.json"),
                (safe_input, "safe-input.json"),
                (safe_output, "safe-output.json")):
            shutil.copy2(source, evidence / target)
        (evidence / "native.stderr.txt").write_text(
            replay.stderr, encoding="utf-8")
        (evidence / "verified.json").write_text(json.dumps({
            "entry": args.entry,
            "reason": args.reason,
            "model": args.model,
            "native_signal": "SIGABRT",
            "native_exit_code": replay.returncode,
            "stderr_phrase": args.stderr_phrase,
            "verified": True,
        }, indent=2) + "\n", encoding="utf-8")
    print(f"M7 {args.reason} witness choice=1 reproduced by native SIGABRT")


if __name__ == "__main__":
    main()
