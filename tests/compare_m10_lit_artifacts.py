#!/usr/bin/env python3
"""Compare formal execution artifacts from two isolated lit runs."""

import argparse
import json
import pathlib
import re

from check_m10_optimizations import compare, solve


def read_cases(root):
    cases = {}
    for report_file in sorted(root.rglob("result.json")):
        report = json.loads(report_file.read_text())
        if report.get("format") != "formal-m5-result-v1":
            continue
        relative = report_file.parent.relative_to(root).as_posix()
        # opt publishes to a generated run directory below the fixture's
        # output path. The fixture path and entry identify the run.
        key = (re.sub(r"/runs/run-[^/]+$", "/runs", relative), report["entry"])
        assert key not in cases, (key, report_file)
        cases[key] = report_file.parent, report
    assert cases, root
    return cases


def relation(directory, report):
    name = report["relation_file"]
    return (directory / name).read_text() if name else None


def read_witness(directory, report):
    name = report["counterexample_file"]
    return json.loads((directory / name).read_text()) if name else None


def validate_witness(z3, witness, inputs):
    query = witness["path_constraints_smt2"]
    query, check, tail = query.partition("(check-sat)")
    assert check and not tail.strip(), witness
    declarations = set(re.findall(r"^\(declare-fun (\|[^|]+\|) ", query, re.M))
    assigned = set()
    for region in inputs:
        for offset, value in enumerate(region["bytes"]):
            symbol = f"|input.{region['name']}.{offset}|"
            assert isinstance(value, int) and 0 <= value <= 255, witness
            if symbol in declarations:
                assert symbol not in assigned, witness
                assigned.add(symbol)
                query += f"(assert (= {symbol} (_ bv{value} 8)))\n"
    assert assigned == declarations, (declarations - assigned, witness)
    solve(z3, query + check, "sat")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--baseline", required=True, type=pathlib.Path)
    parser.add_argument("--optimized", required=True, type=pathlib.Path)
    parser.add_argument("--z3", required=True)
    args = parser.parse_args()
    baseline = read_cases(args.baseline)
    optimized = read_cases(args.optimized)
    assert baseline.keys() == optimized.keys(), (
        baseline.keys() - optimized.keys(), optimized.keys() - baseline.keys())
    relations = witnesses = 0
    for key, (left_dir, left) in baseline.items():
        right_dir, right = optimized[key]
        left_relation = relation(left_dir, left)
        right_relation = relation(right_dir, right)
        try:
            compare(args.z3, (left, None, left_relation),
                    (right, None, right_relation))
            before = read_witness(left_dir, left)
            after = read_witness(right_dir, right)
            assert (before is None) == (after is None)
            if before is not None:
                assert before["reason"] == after["reason"], (before, after)
                before_inputs = before["inputs"]
                after_inputs = after["inputs"]
                schemas = [
                    {region["name"]: len(region["bytes"]) for region in inputs}
                    for inputs in (before_inputs, after_inputs)
                ]
                assert schemas[0] == schemas[1], schemas
                validate_witness(args.z3, before, before_inputs)
                validate_witness(args.z3, after, after_inputs)
        except AssertionError as error:
            raise AssertionError(key, error.args) from error
        relations += left_relation is not None
        witnesses += before is not None
    print(f"Compared {len(baseline)} reports and {relations} relations. "
          f"Validated {witnesses} witness pairs.")


if __name__ == "__main__":
    main()
