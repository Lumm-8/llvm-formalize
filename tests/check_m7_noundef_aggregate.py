#!/usr/bin/env python3
"""Keep undefined aggregate semantic bits out of SAFE artifacts."""

import json
import pathlib
import subprocess
import sys

from m5_test_temp import RetainedTemporaryDirectory


def main(executor, ir):
    cases = (
        ("noundef_undef_aggregate_argument", 2, "UNKNOWN",
         "UNKNOWN-POISON-ANNOTATION"),
        ("noundef_undef_aggregate_return", 2, "UNKNOWN",
         "UNKNOWN-POISON-ANNOTATION"),
        ("noundef_partial_aggregate", 2, "UNKNOWN",
         "UNKNOWN-POISON-ANNOTATION"),
        ("noundef_callsite_return", 2, "UNKNOWN",
         "UNKNOWN-POISON-ANNOTATION"),
        ("noundef_aggregate_padding", 0, "SAFE", "NORMAL-RETURN"),
    )
    with RetainedTemporaryDirectory(prefix="formal-m7-noundef-") as root:
        for entry, expected_exit, expected_status, expected_reason in cases:
            output = pathlib.Path(root) / entry
            run = subprocess.run(
                [executor, ir, "--entry", entry, "--output-dir", str(output),
                 "--allow-unversioned-ir"], capture_output=True, text=True)
            assert run.returncode == expected_exit, run.stdout + run.stderr
            report = json.loads((output / "result.json").read_text())
            assert report["status"] == expected_status, report
            assert report["paths"][0]["reason"] == expected_reason, report
            if expected_status == "UNKNOWN":
                assert report["path_counts"]["normal"] == 0, report
                assert report["path_counts"]["incomplete"] == 1, report
    print("M7 noundef aggregate semantic bits cannot yield SAFE")


if __name__ == "__main__":
    main(*sys.argv[1:])
