#!/usr/bin/env python3
"""Record correctness checks and warmed M10 benchmark samples as JSON."""

import argparse
import hashlib
import json
import pathlib
import statistics

from check_m10_optimizations import compare, read_run
from m5_test_temp import RetainedTemporaryDirectory


def measure(args, root, ir, modes, entry):
    samples = {name: [] for name in modes}
    for index in range(args.warmups + args.samples):
        runs = {}
        # Reverse the order every other round to reduce run-order bias.
        order = list(modes) if index % 2 == 0 else list(reversed(modes))
        for name in order:
            run = read_run(args.executor, ir, root / f"{index}-{name}",
                           modes[name], "SAFE", entry, include_wall_time=True)
            runs[name] = run
            if index >= args.warmups:
                samples[name].append(run[1])
        for name in order:
            if name != "baseline":
                compare(args.z3, runs["baseline"], runs[name])
    return {
        "fixture": ir.name,
        "fixture_sha256": hashlib.sha256(ir.read_bytes()).hexdigest(),
        "entry": entry,
        "warmups_per_mode": args.warmups,
        "modes": {
            name: {
                "flags": modes[name],
                "samples": rows,
                "median": {key: statistics.median(row[key] for row in rows)
                           for key in rows[0]},
            }
            for name, rows in samples.items()
        },
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--z3", required=True)
    parser.add_argument("--output", required=True, type=pathlib.Path)
    parser.add_argument("--samples", default=5, type=int)
    parser.add_argument("--warmups", default=5, type=int)
    parser.add_argument("--baseline-only", action="store_true")
    parser.add_argument("--suite", choices=("ab", "merge"), default="ab")
    args = parser.parse_args()
    if args.samples < 5 or args.warmups < 5:
        parser.error("--samples and --warmups must be at least 5")
    fixtures = pathlib.Path(__file__).resolve().parent / "benchmarks" / "m10"
    with RetainedTemporaryDirectory(prefix="formal-m10-benchmark-") as temp:
        modes = {
            "baseline": [],
        }
        if args.suite == "ab" and not args.baseline_only:
            modes.update({
                "cache": ["--query-cache"],
                "rewrite": ["--expr-rewrite"],
                "cache_rewrite": ["--query-cache", "--expr-rewrite"],
            })
        if args.suite == "merge" and not args.baseline_only:
            modes.update({
                "merge": ["--state-merge"],
                "all": ["--query-cache", "--expr-rewrite", "--state-merge"],
            })
        cases = {"independent-optimizations": "formal_top"}
        if args.suite == "ab":
            cases["commuted-guards"] = "formal_top"
        if args.suite == "merge":
            cases = {"diamonds": "diamond_top", "loop-branches": "loop_top",
                     "object-writes": "writes_top",
                     "distinct-object-writes": "distinct_writes_top"}
        results = {name: measure(args, pathlib.Path(temp) / name,
                                 fixtures / f"{name}.ll", modes, entry)
                   for name, entry in cases.items()}
        args.output.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
