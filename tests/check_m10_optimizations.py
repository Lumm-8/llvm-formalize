#!/usr/bin/env python3
"""Compare M10 modes with the baseline using the exported SMT relations."""

import argparse
import json
import os
import pathlib
import subprocess
import time
from collections import Counter

from m5_test_temp import RetainedTemporaryDirectory


class SMTCheckFailed(AssertionError):
    def __init__(self, query, expected, result):
        super().__init__(expected, result)
        self.query = query


def solve(z3, query, expected):
    try:
        result = subprocess.run([z3, "-in", "-smt2"], input=query, text=True,
                                capture_output=True, timeout=30)
    except subprocess.TimeoutExpired as error:
        raise SMTCheckFailed(query, expected, str(error)) from error
    if result.returncode != 0 or result.stdout.strip() != expected:
        raise SMTCheckFailed(query, expected, (result.returncode, result.stdout,
                                               result.stderr))


def equivalent(z3, left, right):
    declarations = {}
    definitions = []
    for name, relation in (("baseline", left), ("optimized", right)):
        lines = [line for line in relation.splitlines()
                 if line and not line.startswith(";")]
        assert lines[0] == "(set-logic QF_BV)", relation
        assert lines[-2:] == ["(assert |formal_relation|)", "(check-sat)"], relation
        assert lines[-3].startswith("(define-fun |formal_relation| "), relation
        for line in lines[1:-3]:
            assert line.startswith("(declare-fun "), line
            symbol = line.split(" ", 2)[1]
            assert declarations.setdefault(symbol, line) == line, line
        definitions.append(lines[-3].replace("|formal_relation|", name, 1))
    solve(z3, "\n".join(["(set-logic QF_BV)", *declarations.values(),
                         *definitions,
                         "(assert (xor baseline optimized))",
                         "(check-sat)\n"]), "unsat")


def read_run(executor, ir, directory, flags, verdict, entry="formal_top",
             include_wall_time=False):
    command = [executor, str(ir), "--entry", entry, "--m5",
               "--output-dir", str(directory), *flags]
    start = time.perf_counter_ns()
    environment = dict(os.environ, FORMAL_QUERY_CACHE="0", FORMAL_EXPR_REWRITE="0",
                       FORMAL_STATE_MERGE="0")
    result = subprocess.run(command, text=True, capture_output=True, timeout=60,
                            env=environment)
    elapsed_ms = (time.perf_counter_ns() - start) / 1e6
    check_exit(result, verdict)
    report, metrics, relation = read_artifacts(directory, verdict)
    if include_wall_time:
        metrics["wall_ms"] = elapsed_ms
    return report, metrics, relation


def check_exit(result, verdict):
    assert result.returncode == {"SAFE": 0, "UNSAFE-UB": 1, "UNKNOWN": 2}[verdict], (
        result.args, result.returncode, result.stdout, result.stderr)


def read_artifacts(directory, verdict):
    report = json.loads((directory / "result.json").read_text())
    assert report["verdict"] == verdict, report
    metrics = json.loads((directory / "metrics.json").read_text())
    filename = report["relation_file"]
    relation = (directory / filename).read_text() if filename else None
    return report, metrics, relation


def plugin_run(args, directory, flags):
    command = [args.opt, "-load-pass-plugin", args.plugin, "-passes=formal-exec",
               "-formal-run-m5", "-formal-allow-unversioned-ir",
               "-formal-entry=formal_top", f"-formal-output-dir={directory}",
               *["-formal-" + flag.removeprefix("--") for flag in flags],
               "-disable-output", str(args.ir)]
    environment = dict(os.environ, FORMAL_QUERY_CACHE="0", FORMAL_EXPR_REWRITE="0",
                       FORMAL_STATE_MERGE="0")
    check_exit(subprocess.run(command, text=True, capture_output=True, timeout=60,
                              env=environment),
               "SAFE")
    run, = (directory / "runs").iterdir()
    return read_artifacts(run, "SAFE")


def failure_identity(path, max_wall_time_ms):
    status, reason = path["status"], path["reason"]
    if max_wall_time_ms and status == "INCOMPLETE" and (
            reason == "UNKNOWN-WALL-TIME-LIMIT"):
        return status, reason, None
    location = tuple(path["source"][field] for field in (
        "function", "block", "instruction_ordinal", "opcode"))
    return status, reason, location


def compare(z3, baseline, optimized):
    left, _, left_relation = baseline
    right, _, right_relation = optimized
    for key in ("verdict", "reason", "incompleteness_reasons",
                "relation_completeness", "unwinding_assumption"):
        assert left.get(key) == right.get(key), (key, left.get(key), right.get(key))
    metadata = [{key: value for key, value in
                 report.get("relation_metadata", {}).items() if key != "paths"}
                for report in (left, right)]
    assert metadata[0] == metadata[1], metadata
    for key in ("error", "incomplete", "internal_error"):
        assert left["path_counts"][key] == right["path_counts"][key], key
    failures = [Counter(
        failure_identity(path, report["limits"]["max_wall_time_ms"])
        for path in report["paths"] if path["status"] != "COMPLETED")
        for report in (left, right)]
    assert failures[0] == failures[1], failures
    if left_relation is None or right_relation is None:
        assert left_relation is None and right_relation is None
    else:
        equivalent(z3, left_relation, right_relation)


def read_witness(z3, directory):
    witness = json.loads((directory / "counterexample.json").read_text())
    assert witness["reason"] == "LLVM-UB-DIVIDE-BY-ZERO", witness
    assert witness["inputs"] == [{"name": "x", "bytes": [0]}], witness
    query = witness["path_constraints_smt2"]
    query, check, tail = query.partition("(check-sat)")
    assert check and not tail.strip(), witness
    solve(z3, query + "(assert (= |input.x.0| (_ bv0 8)))\n" + check, "sat")
    return witness["reason"], witness["inputs"]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--z3", required=True)
    parser.add_argument("--ir", required=True, type=pathlib.Path)
    parser.add_argument("--opt")
    parser.add_argument("--plugin")
    parser.add_argument("--baseline-only", action="store_true")
    args = parser.parse_args()
    if bool(args.opt) != bool(args.plugin):
        parser.error("--opt and --plugin must be used together")
    with RetainedTemporaryDirectory(prefix="formal-m10-") as temp:
        root = pathlib.Path(temp)
        modes = {
            "baseline": [],
        }
        if not args.baseline_only:
            modes.update({
                "cache": ["--query-cache"],
                "rewrite": ["--expr-rewrite"],
                "both": ["--query-cache", "--expr-rewrite"],
            })
        for verdict, entry, limits in (
                ("SAFE", "formal_top", []),
                ("UNKNOWN", "formal_top", ["--max-solver-queries", "4"]),
                ("UNSAFE-UB", "unsafe_top", [])):
            directories = {name: root / f"{verdict.lower()}-{name}"
                           for name in modes}
            runs = {name: read_run(args.executor, args.ir,
                                   directories[name], limits + flags,
                                   verdict, entry)
                    for name, flags in modes.items()}
            baseline = runs["baseline"]
            for name in runs:
                compare(args.z3, baseline, runs[name])
            if not args.baseline_only:
                for _, metrics, _ in runs.values():
                    assert metrics["solver_queries"] == baseline[1]["solver_queries"]
                    assert metrics["solver_queries"] == (
                        metrics["solver_backend_queries"] +
                        metrics["solver_cache_hits"]), metrics
            if verdict == "UNKNOWN":
                assert baseline[0]["incompleteness_reasons"] == {
                    "UNKNOWN-SOLVER-QUERY-LIMIT": 4,
                }, baseline[0]
            elif verdict == "SAFE":
                expected = """(set-logic QF_BV)
(declare-fun |input.x.0| () (_ BitVec 8))
(declare-fun |output.y.0| () (_ BitVec 8))
(define-fun |formal_relation| () Bool (= |output.y.0| (ite (bvult |input.x.0| (_ bv128 8)) (_ bv17 8) (_ bv42 8))))
(assert |formal_relation|)
(check-sat)
"""
                equivalent(args.z3, baseline[2], expected)
                if args.baseline_only:
                    continue
                metrics = {name: run[1] for name, run in runs.items()}
                assert metrics["cache"]["solver_cache_hits"] > 0, metrics
                assert (metrics["cache"]["solver_backend_queries"] <
                        metrics["baseline"]["solver_backend_queries"]), metrics
                assert (metrics["rewrite"]["ast_nodes"] <
                        metrics["baseline"]["ast_nodes"]), metrics
                assert metrics["rewrite"]["solver_cache_hits"] == 0, metrics
                if args.opt:
                    for name, flags in modes.items():
                        plugin = plugin_run(args, root / f"plugin-{name}", flags)
                        compare(args.z3, runs[name], plugin)
                        for key in ("solver_queries", "solver_backend_queries",
                                    "solver_cache_hits", "ast_nodes"):
                            assert plugin[1][key] == metrics[name][key], (name, key)
            elif verdict == "UNSAFE-UB":
                witnesses = [read_witness(args.z3, directory)
                             for directory in directories.values()]
                assert all(witness == witnesses[0] for witness in witnesses)


if __name__ == "__main__":
    main()
