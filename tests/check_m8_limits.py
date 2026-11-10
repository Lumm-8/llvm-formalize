#!/usr/bin/env python3
"""No resource-truncated run may claim an unconditional SAFE result."""

import argparse
import json
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--ir", required=True)
    args = parser.parse_args()
    cases = (
        ("recursive_top", ["--max-call-depth", "2"],
         "UNKNOWN-CALL-DEPTH"),
        ("break_loop", ["--max-instructions", "2"],
         "UNKNOWN-INSTRUCTION-LIMIT"),
        ("multiple_exits", ["--max-states", "1"],
         "UNKNOWN-STATE-LIMIT"),
        ("forked_loop", ["--max-solver-queries", "1"],
         "UNKNOWN-SOLVER-QUERY-LIMIT"),
        ("infinite_loop", ["--max-time-ms", "1",
                           "--loop-bound", "100000000",
                           "--max-instructions", "100000000"],
         "UNKNOWN-WALL-TIME-LIMIT"),
    )
    with RetainedTemporaryDirectory(prefix="formal-m8-limits-") as temp:
        root = pathlib.Path(temp)
        for entry, options, reason in cases:
            output = root / entry
            process = subprocess.run([
                args.formal_check, args.ir, "--entry", entry,
                "--allow-unversioned-ir", "--output-dir", str(output),
                *options,
            ], capture_output=True, text=True)
            assert process.returncode == 2, (entry, process.stdout, process.stderr)
            result = json.loads((output / "result.json").read_text())
            assert result["status"] == "UNKNOWN", (entry, result)
            assert reason in result["incompleteness_reasons"], (entry, result)
            assert result["relation_completeness"] == "UNDER_APPROXIMATION", result
            assert any(path["reason"] == reason and path["limit_name"]
                       for path in result["paths"]), (entry, result)
            print(f"{entry}: {reason}")


if __name__ == "__main__":
    main()
