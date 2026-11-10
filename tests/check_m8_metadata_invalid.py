#!/usr/bin/env python3
"""Malformed or conflicting local bounds cannot silently use the default."""

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
    with RetainedTemporaryDirectory(prefix="formal-m8-invalid-metadata-") as temp:
        for entry in ("malformed", "duplicate", "out_of_range", "negative",
                      "conflicting_latches"):
            output = pathlib.Path(temp) / entry
            process = subprocess.run([
                args.formal_check, args.ir, "--entry", entry,
                "--allow-unversioned-ir", "--loop-bound", "10",
                "--output-dir", str(output),
            ], capture_output=True, text=True)
            assert process.returncode == 2, (entry, process.stdout, process.stderr)
            result = json.loads((output / "result.json").read_text())
            assert result["status"] == "UNKNOWN", (entry, result)
            assert result["incompleteness_reasons"] == {
                "UNKNOWN-LOOP-UNSUPPORTED": 1}, (entry, result)
            path = result["paths"][0]
            assert path["loop"]["header"] == "header", (entry, path)
            assert path["loop"]["suppressed_edge"] == {
                "from": "entry", "to": "header"}, (entry, path)


if __name__ == "__main__":
    main()
