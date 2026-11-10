#!/usr/bin/env python3
"""Validate Clang's loop metadata and its consumption by loop-unroll."""

import argparse
import json
import pathlib
import re
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--clang", required=True)
    parser.add_argument("--opt", required=True)
    parser.add_argument("--filecheck", required=True)
    parser.add_argument("--source", required=True)
    parser.add_argument("--check", required=True)
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--formal-source", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m8-pragma-") as temporary:
        root = pathlib.Path(temporary)
        original = root / "original.ll"
        transformed = root / "unrolled.ll"
        subprocess.run([
            args.clang, "-O0", "-Xclang", "-disable-O0-optnone",
            "-S", "-emit-llvm", args.source, "-o", str(original),
        ], check=True, capture_output=True, text=True)
        subprocess.run([
            args.filecheck, args.check, "--input-file", str(original),
        ], check=True, capture_output=True, text=True)
        subprocess.run([
            args.opt, "-passes=loop-unroll", "-S", str(original),
            "-o", str(transformed),
        ], check=True, capture_output=True, text=True)
        text = transformed.read_text()
        assert "llvm.loop.unroll.count" not in text, text
        assert "llvm.loop.unroll.disable" in text, text

        # Execute the actual transformed Clang IR, not a hand-written stand-in.
        formal_original = root / "formal-original.ll"
        formal_unrolled = root / "formal-unrolled.ll"
        subprocess.run([
            args.clang, "-O0", "-Xclang", "-disable-O0-optnone",
            "-S", "-emit-llvm", args.formal_source, "-o", str(formal_original),
        ], check=True, capture_output=True, text=True)
        subprocess.run([
            args.filecheck, args.check, "--input-file", str(formal_original),
        ], check=True, capture_output=True, text=True)
        subprocess.run([
            args.opt, "-passes=loop-unroll", "-S", str(formal_original),
            "-o", str(formal_unrolled),
        ], check=True, capture_output=True, text=True)
        optimized_text = formal_unrolled.read_text()
        assert "llvm.loop.unroll.count" not in optimized_text
        assert "llvm.loop.unroll.disable" in optimized_text

        def check_unknown(ir, name, require_local=False):
            output = root / name
            command = [
                args.formal_check, str(ir), "--entry", "formal_top",
                "--allow-unversioned-ir", "--loop-bound", "100",
                "--output-dir", str(output),
            ]
            if require_local:
                command.append("--require-local-loop-bound")
            process = subprocess.run(command, capture_output=True, text=True)
            assert process.returncode == 2, (process.stdout, process.stderr)
            result = json.loads((output / "result.json").read_text())
            assert result["status"] == "UNKNOWN", result
            assert result["incompleteness_reasons"] == {
                "UNKNOWN-LOOP-METADATA-LOST": 1}, result
            assert result["limits"]["require_local_loop_bound"] is require_local

        check_unknown(formal_unrolled, "optimized")
        stripped = root / "formal-stripped.ll"
        stripped_text, removed = re.subn(
            r", !llvm\.loop ![0-9]+", "", optimized_text)
        assert removed > 0
        stripped.write_text(stripped_text)
        check_unknown(stripped, "stripped", require_local=True)


if __name__ == "__main__":
    main()
