#!/usr/bin/env python3
"""Check partial relations, aliases, closure, hashes, and STP reparse."""

import argparse
import hashlib
import json
import pathlib
import re
import subprocess
from m5_test_temp import RetainedTemporaryDirectory


def run(executor, ir, entry, directory, expected_code):
    result = subprocess.run([executor, ir, "--m5", "--entry", entry,
                             "--output-dir", str(directory)],
                            capture_output=True, text=True)
    assert result.returncode == expected_code, (entry, result)
    report = json.loads((directory / "result.json").read_text())
    assert report["schema_version"] == 1
    assert report["entry"] == entry
    assert set(report["path_counts"]) == {
        "normal", "error", "incomplete", "internal_error"}
    assert (directory / "queries").is_dir()
    assert (directory / "counterexamples").is_dir()
    assert (directory / "replay").is_dir()
    for name, digest in report["artifacts_sha256"].items():
        assert hashlib.sha256((directory / name).read_bytes()).hexdigest() == digest
    return report


def check_relation(directory, stp, answer):
    path = directory / "relation.smt2"
    source = path.read_text()
    defined = set(re.findall(r"\((?:declare|define)-fun\s+\|([^|]+)\|", source))
    used = set(re.findall(r"\|([^|]+)\|", source))
    assert used <= defined, sorted(used - defined)
    parsed = subprocess.run([stp, "--SMTLIB2", str(path)],
                            capture_output=True, text=True, check=True)
    assert parsed.stdout.strip().lower() == answer, parsed


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--edge-ir", required=True)
    parser.add_argument("--scalar-ir", required=True)
    parser.add_argument("--assume-ir", required=True)
    parser.add_argument("--stp", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m5-artifacts-") as root:
        root = pathlib.Path(root)
        partial = root / "partial"
        report = run(args.executor, args.edge_ir, "partial_path", partial, 2)
        assert report["status"] == "UNKNOWN"
        assert report["relation_completeness"] == "UNDER_APPROXIMATION"
        assert report["path_counts"]["normal"] == 1
        assert report["path_counts"]["incomplete"] == 1
        check_relation(partial, args.stp, "sat")

        error_unknown = root / "error-unknown"
        report = run(args.executor, args.edge_ir, "error_and_unknown",
                     error_unknown, 1)
        assert report["status"] == "UNSAFE"
        assert report["relation_completeness"] == "UNDER_APPROXIMATION"
        assert report["path_counts"]["error"] == 1
        assert report["path_counts"]["incomplete"] == 1
        assert (error_unknown / "counterexamples" / "witness.json").is_file()
        check_relation(error_unknown, args.stp, "unsat")

        inout = root / "inout"
        report = run(args.executor, args.scalar_ir, "inout", inout, 0)
        assert report["status"] == "SAFE"
        assert report["alias_overlaps"]
        assert report["alias_overlaps"][0]["input"] == "x"
        assert report["alias_overlaps"][0]["output"] == "x"
        check_relation(inout, args.stp, "sat")
        invalid = subprocess.run([args.executor, str(root / "missing.ll"),
                                  "--m5", "--entry", "inout",
                                  "--output-dir", str(inout)],
                                 capture_output=True, text=True)
        assert invalid.returncode == 3
        assert not (inout / "result.json").exists()

        empty = root / "empty"
        report = run(args.executor, args.assume_ir, "empty_domain", empty, 0)
        assert report["path_counts"]["normal"] == 0
        assert report["relation_completeness"] == "COMPLETE"
        check_relation(empty, args.stp, "unsat")

        consistent = root / "consistent"
        report = run(args.executor, args.edge_ir, "consistent_paths",
                     consistent, 0)
        validated = root / "validated"
        driver = subprocess.run([args.formal_check, args.edge_ir, "--entry",
                                 "consistent_paths", "--output-dir",
                                 str(validated), "--allow-unversioned-ir"],
                                capture_output=True, text=True)
        assert driver.returncode == 0, driver
        validated_report = json.loads((validated / "result.json").read_text())
        assert (consistent / "relation.smt2").read_bytes() == (
            validated / "relation.smt2").read_bytes()
        for key in ("status", "relation_completeness", "descriptors",
                    "paths", "alias_overlaps", "relation_metadata"):
            assert report[key] == validated_report[key], key
        check_relation(consistent, args.stp, "sat")


if __name__ == "__main__":
    main()
