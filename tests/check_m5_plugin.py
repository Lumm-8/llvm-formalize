#!/usr/bin/env python3
"""Exercise M0 preflight and M5 relation export in the pinned opt process."""

import argparse
import json
import pathlib
import subprocess
from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--clang", required=True)
    parser.add_argument("--opt", required=True)
    parser.add_argument("--plugin", required=True)
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--source", required=True)
    parser.add_argument("--include", required=True)
    parser.add_argument("--stp", required=True)
    parser.add_argument("--entry", default="formal_top")
    parser.add_argument("--expect-calls", action="store_true")
    args = parser.parse_args()

    with RetainedTemporaryDirectory(prefix="formal-m5-plugin-") as temporary:
        root = pathlib.Path(temporary)
        ir = root / "program.ll"
        subprocess.run([args.clang, "-O0", "-g", "-S", "-emit-llvm",
                        "-I", args.include, args.source, "-o", str(ir)],
                       check=True, capture_output=True, text=True)
        output = root / "artifacts"
        run = subprocess.run([
            args.opt, "-load-pass-plugin", args.plugin,
            "-passes=formal-exec", "-formal-run-m5",
            f"-formal-entry={args.entry}", f"-formal-output-dir={output}",
            "-disable-output", str(ir)], capture_output=True, text=True)
        assert run.returncode == 0, (run.returncode, run.stderr)
        assert "FormalPass: M5 status SAFE" in run.stderr, run.stderr
        directories = list((output / "runs").iterdir())
        assert len(directories) == 1, directories
        directory = directories[0]
        preflight = json.loads((directory / "preflight.json").read_text())
        result = json.loads((directory / "result.json").read_text())
        assert preflight["status"] == "VALIDATED", preflight
        assert preflight["run_id"] == result["run_id"]
        assert result["status"] == "SAFE", result
        if args.expect_calls:
            assert result["max_call_depth_observed"] >= 3, result
            for path in result["paths"]:
                entered = [event["model"] for event in path["events"]
                           if event["kind"] == "call_enter"]
                assert entered == ["write_result", "add_one"], path
        assert result["relation_metadata"]["inputs"] == [
            {"name": "x", "ordinal": 0, "size": 1}]
        assert result["relation_metadata"]["outputs"] == [
            {"name": "y", "ordinal": 1, "size": 1}]
        relation = directory / "relation.smt2"
        driver_output = root / "formal-check"
        driver = subprocess.run([args.formal_check, str(ir), "--entry",
                                 args.entry, "--output-dir",
                                 str(driver_output)], capture_output=True,
                                text=True)
        assert driver.returncode == 0, (driver.returncode, driver.stderr)
        driver_result = json.loads((driver_output / "result.json").read_text())
        assert relation.read_bytes() == (driver_output / "relation.smt2").read_bytes()
        for key in ("status", "relation_completeness", "entry",
                    "target_triple", "data_layout", "descriptors", "paths",
                    "path_counts", "alias_overlaps", "relation_metadata"):
            assert result[key] == driver_result[key], key
        answer = subprocess.run([args.stp, "--SMTLIB2", str(relation)],
                                check=True, capture_output=True,
                                text=True).stdout.strip().lower()
        assert answer == "sat", answer


if __name__ == "__main__":
    main()
