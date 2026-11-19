#!/usr/bin/env python3
"""Check that LangRef UB has a feasible witness and special values block SAFE."""

import json
import pathlib
import sys


symbolic = pathlib.Path(sys.argv[1])
report = json.loads((symbolic / "result.json").read_text())
witness = json.loads((symbolic / "counterexample.json").read_text())
assert report["status"] == "UNSAFE-UB", report
assert witness["reason"] == "LLVM-UB-DIVIDE-BY-ZERO", witness
assert witness["semantic_kind"] == "LLVM_UB", witness
assert witness["replay_class"] == "IR_ONLY", witness
assert witness["inputs"] == [{"name": "x", "bytes": [0]}], witness
assert any(path["status"] == "ERROR" and
           path["reason"] == witness["reason"] for path in report["paths"])
assert any(path["status"] == "COMPLETED" for path in report["paths"])

expected = (
    "UNKNOWN-UNDEF", "UNKNOWN-UNDEF",
    "UNKNOWN-POISON",
    "UNKNOWN-INBOUNDS-GEP", "UNKNOWN-INBOUNDS-GEP",
    "UNKNOWN-INBOUNDS-GEP",
)
for directory, reason in zip(sys.argv[2:], expected, strict=True):
    root = pathlib.Path(directory)
    report = json.loads((root / "result.json").read_text())
    assert report["status"] == "UNKNOWN", (root, report)
    assert reason in report["incompleteness_reasons"], (root, report)
    assert report["counterexample_file"] == "", (root, report)
