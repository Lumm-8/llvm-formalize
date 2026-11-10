#!/usr/bin/env python3
"""Check native input injection, assume domain, and top-frame output capture."""

import argparse
import json
import pathlib
import subprocess
from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixture", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m5-replay-") as root:
        root = pathlib.Path(root)
        for input_value, expected_status, expected_output, expected_exit in (
                (3, "CAPTURED", 4, 0),
                (5, "OUTSIDE_DOMAIN", None, 2)):
            counterexample = root / "counterexample.json"
            captured = root / "captured.json"
            counterexample.write_text(json.dumps({"inputs": [
                {"name": "Input", "bytes": [input_value]}]}))
            process = subprocess.run([args.fixture, str(counterexample),
                                      str(captured)], capture_output=True, text=True)
            assert process.returncode == expected_exit, process.stderr
            result = json.loads(captured.read_text())
            assert result["status"] == expected_status, result
            if expected_output is not None:
                assert result["outputs"] == [
                    {"name": "Output", "bytes": [expected_output]}], result
        print("replay input injection, assume, and stack snapshot passed")


if __name__ == "__main__":
    main()
