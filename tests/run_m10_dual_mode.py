#!/usr/bin/env python3
"""Run a complete CTest profile twice and verify archived formal results."""

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from check_m10_optimizations import SMTCheckFailed, compare, solve
from compare_m10_lit_artifacts import read_witness, relation, validate_witness


SWITCHES = ("FORMAL_EXPR_REWRITE", "FORMAL_QUERY_CACHE", "FORMAL_STATE_MERGE")
REQUIRED_METRICS = {"states_created", "forks", "state_merges", "ast_nodes",
                    "ast_depth", "max_update_list_length", "solver_queries",
                    "solver_backend_queries", "solver_cache_hits",
                    "solver_time_ns", "wall_time_ns", "peak_resident_bytes"}


def run_profile(build, root, jobs, ctest_regex):
    return_codes = []
    command = ["ctest", "--test-dir", build, "-j", str(jobs), "--timeout",
               "3600", "--output-on-failure"]
    if ctest_regex:
        command.extend(["-R", ctest_regex, "--no-tests=error"])
    for mode, enabled in (("off", "0"), ("on", "1")):
        destination = root / mode
        destination.mkdir(parents=True)
        env = dict(os.environ, FORMAL_TEST_ARTIFACT_ROOT=str(destination),
                   **dict.fromkeys(SWITCHES, enabled))
        with (destination / "ctest.log").open("w") as log:
            result = subprocess.run(command, env=env, stdout=log,
                                    stderr=subprocess.STDOUT)
        shutil.copyfile(build / "Testing/Temporary/LastTest.log",
                        destination / "LastTest.log")
        return_codes.append(result.returncode)
    if any(return_codes):
        raise SystemExit(f"CTest failed {return_codes}. Artifacts at {root}")


def read_runs(root):
    groups = defaultdict(list)
    for entry in sorted(root.glob("*/*/result.json")):
        directory = entry.parent
        report = json.loads(entry.read_text())
        source = (directory / "input.ll").read_text()
        source = re.sub(r"^(?:; ModuleID =|source_filename =).*\n", "", source,
                        flags=re.M)
        key = (directory.parent.name, report["entry"],
               hashlib.sha256(source.encode()).hexdigest(),
               json.dumps({key: report[key] for key in (
                   "limits", "loop_limit_policy", "semantic_options",
                   "overlap_policy")}, sort_keys=True))
        metrics = json.loads((directory / "metrics.json").read_text())
        assert REQUIRED_METRICS <= metrics.keys(), directory
        groups[key].append((directory, report, metrics))
    assert groups, f"No formal results archived under {root}"
    return groups


def error_disjunction(paths, declarations):
    clauses = []
    for path in paths:
        predicates = []
        for line in path["query"].splitlines():
            if line.startswith("(declare-fun "):
                name = line.split(" ", 2)[1]
                assert declarations.setdefault(name, line) == line, line
            elif line.startswith("(assert "):
                assert line.endswith(")"), line
                predicates.append(line[8:-1])
            else:
                assert line.startswith((";", "(set-logic ", "(check-sat)")), line
        clauses.append("(and true " + " ".join(predicates) + ")")
    return "(or false " + " ".join(clauses) + ")"


def error_groups(directory):
    groups = defaultdict(list)
    for path in json.loads((directory / "errors.json").read_text()):
        source = path["source"]
        key = (path["reason"], source["function"], source["block"],
               source["instruction_ordinal"], source["opcode"])
        groups[key].append(path)
    return groups


def compare_runs(z3, baseline, optimized):
    left_dir, left, _ = baseline
    right_dir, right, _ = optimized
    compare(z3, (left, None, relation(left_dir, left)),
            (right, None, relation(right_dir, right)))
    left_errors, right_errors = error_groups(left_dir), error_groups(right_dir)
    assert left_errors.keys() == right_errors.keys(), (left_dir, right_dir)
    for key, paths in left_errors.items():
        declarations = {}
        before = error_disjunction(paths, declarations)
        after = error_disjunction(right_errors[key], declarations)
        solve(z3, "\n".join(["(set-logic QF_AUFBV)", *declarations.values(),
                             f"(assert (xor {before} {after}))", "(check-sat)"]),
              "unsat")
    witnesses = []
    for directory, report in ((left_dir, left), (right_dir, right)):
        witness = read_witness(directory, report)
        if witness is not None:
            validate_witness(z3, witness, witness["inputs"])
        witnesses.append(witness)
    assert (witnesses[0] is None) == (witnesses[1] is None)
    return bool(left["relation_file"]), witnesses[0] is not None, len(left_errors)


def compare_group(root, z3, key, baseline, optimized):
    counts = dict(reports=0, relations=0, witness_pairs=0, error_sets=0)
    remaining = list(optimized)
    assert len(baseline) == len(remaining), key
    for before in baseline:
        failures = []
        for index, after in enumerate(remaining):
            try:
                relations, witnesses, errors = compare_runs(z3, before, after)
            except AssertionError as error:
                failures.append((after[0], error))
                continue
            remaining.pop(index)
            counts["reports"] += 1
            counts["relations"] += relations
            counts["witness_pairs"] += witnesses
            counts["error_sets"] += errors
            break
        else:
            directory = root / "failed-pairs" / before[0].parent.name / before[0].name
            directory.mkdir(parents=True, exist_ok=True)
            for name, source in (("off", before[0]),
                                 ("on", failures[0][0])):
                shutil.copytree(source, directory / name, dirs_exist_ok=True)
            for index, (_, error) in enumerate(failures):
                if isinstance(error, SMTCheckFailed):
                    (directory / f"check-{index}.smt2").write_text(error.query)
            raise AssertionError(key, before[0], failures)
    return counts


def verify(root, z3, jobs):
    left, right = (read_runs(root / mode) for mode in ("off", "on"))
    assert left.keys() == right.keys(), (left.keys() - right.keys(),
                                        right.keys() - left.keys())
    totals = dict(reports=0, relations=0, witness_pairs=0, error_sets=0)
    with ThreadPoolExecutor(max_workers=jobs) as workers:
        def check(key):
            return compare_group(root, z3, key, left[key], right[key])

        for counts in workers.map(check, left):
            for name, count in counts.items():
                totals[name] += count
    (root / "comparison.json").write_text(json.dumps(totals, indent=2) + "\n")
    print(json.dumps(totals, indent=2))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--build", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--z3", required=True)
    parser.add_argument("--jobs", type=int, default=2)
    parser.add_argument("--ctest-regex")
    parser.add_argument("--compare-only", action="store_true")
    args = parser.parse_args()
    root = args.output.resolve()
    if not args.compare_only:
        root.mkdir(parents=True)
        run_profile(args.build.resolve(), root, args.jobs, args.ctest_regex)
    verify(root, args.z3, args.jobs)


if __name__ == "__main__":
    main()
