#!/usr/bin/env python3
"""Compare guarded joins and check baseline replay at resource limits."""

import argparse
import pathlib

from check_m10_optimizations import compare, read_run, read_witness
from m5_test_temp import RetainedTemporaryDirectory


def run_modes(args, root, fixture, entry, verdict, limits):
    modes = {"off": [], "merge": ["--state-merge"],
             "all": ["--state-merge", "--query-cache", "--expr-rewrite"]}
    runs = {}
    for name, flags in modes.items():
        directory = root / name
        runs[name] = read_run(args.executor, args.fixtures / fixture,
                              directory, limits + flags, verdict, entry)
        if verdict == "UNSAFE-UB":
            read_witness(args.z3, directory)
    for name in ("merge", "all"):
        compare(args.z3, runs["off"], runs[name])
    return {name: run[1] for name, run in runs.items()}


def check_relation_depth(args, root, fixture, entry, completed_paths,
                         check_all=True):
    baseline = read_run(args.executor, fixture, root / "off", [], "UNKNOWN", entry)
    assert baseline[0]["path_counts"]["normal"] == completed_paths, baseline[0]
    assert "expression depth limit exceeded" in baseline[0]["reason"], baseline[0]
    assert baseline[2] is None
    modes = {"merge": ["--state-merge"]}
    if check_all:
        modes["all"] = ["--state-merge", "--query-cache", "--expr-rewrite"]
    for mode, flags in modes.items():
        candidate = read_run(args.executor, fixture, root / mode, flags,
                             "UNKNOWN", entry)
        compare(args.z3, baseline, candidate)
        assert candidate[0]["path_counts"]["normal"] == completed_paths, candidate[0]
        assert candidate[1]["state_merges"] == 0, candidate[1]


def write_wide_relation_fixture(fixture, source, width, arm_stores=0):
    header = source.split("define void @diamond_top", 1)[0]
    additions = []
    value = "%x"
    for index in range(120):
        next_value = f"%add{index}"
        additions.append(f"  {next_value} = add i8 {value}, 1\n")
        value = next_value
    scratch = "  %scratch = alloca i8, align 1\n" if arm_stores else ""
    stores = "  store i8 42, ptr %scratch, align 1\n" * arm_stores
    fixture.write_text(header + f"""\
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)

define void @wide_top() {{
entry:
{scratch}  %input = alloca i8, align 1
  %output = alloca [{width} x i8], align 1
  call void @__formal_register_input(ptr %input, i64 1, ptr @input_name)
  call void @__formal_register_output(ptr %output, i64 {width}, ptr @output_name)
  call void @llvm.memset.p0.i64(ptr %output, i8 17, i64 {width}, i1 false)
  %x = load i8, ptr %input, align 1
{''.join(additions)}  %test = icmp ult i8 {value}, 128
  br i1 %test, label %left, label %right
left:
{stores}  br label %join
right:
{stores}  br label %join
join:
  ret void
}}
""")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--z3", required=True)
    parser.add_argument("--fixtures", required=True, type=pathlib.Path)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m10-merge-") as temporary:
        root = pathlib.Path(temporary)
        source = (args.fixtures / "diamonds.ll").read_text()
        fixture = root / "wide-relation.ll"
        write_wide_relation_fixture(fixture, source, 930)
        check_relation_depth(args, root / "wide-depth", fixture, "wide_top", 2)

        # Each store contributes true guards. Merging discards those guards,
        # but in baseline export they follow a symbolic branch constraint.
        fixture = root / "discarded-arm-constraints.ll"
        write_wide_relation_fixture(fixture, source, 294, arm_stores=14)
        check_relation_depth(args, root / "discarded-arm-constraints", fixture,
                             "wide_top", 2, check_all=False)

        fixture = root / "wide-relation-within-limit.ll"
        write_wide_relation_fixture(fixture, source, 250)
        metrics = run_modes(args, root / "wide-within-limit", fixture,
                            "wide_top", "SAFE", [])
        assert metrics["merge"]["state_merges"] == 1, metrics
        assert metrics["all"]["state_merges"] == 1, metrics

        for fixture, entry in (("diamonds.ll", "diamond_top"),
                               ("loop-branches.ll", "loop_top"),
                               ("object-writes.ll", "writes_top"),
                               ("distinct-object-writes.ll", "distinct_writes_top")):
            metrics = run_modes(args, root / entry, fixture, entry, "SAFE", [])
            for name in ("merge", "all"):
                assert metrics[name]["state_merges"] == 5, metrics
                assert metrics[name]["states_created"] < metrics["off"]["states_created"]
                assert metrics[name]["solver_queries"] < metrics["off"]["solver_queries"]
        for entry, verdict, expected_merges in (
                ("nested_join", "SAFE", 1),
                ("equal_scalar", "SAFE", 1),
                ("pointer_phi", "SAFE", 1),
                ("preserved_shadow", "SAFE", 1),
                ("aggregate_phi", "SAFE", 1),
                ("poison_phi", "SAFE", 1),
                ("freeze_identity", "SAFE", 2),
                ("nested_calls", "SAFE", 3),
                ("initialization_differs", "SAFE", 0),
                ("allocations_differ", "SAFE", 0),
                ("lifetimes_differ", "SAFE", 0),
                ("descriptors_differ", "UNKNOWN", 0),
                ("undef_on_one_edge", "UNKNOWN", 0)):
            limits = ["--allocation-failures"] if entry == "lifetimes_differ" else []
            metrics = run_modes(args, root / entry, "merge-compatibility.ll",
                                entry, verdict, limits)
            for name in ("merge", "all"):
                assert metrics[name]["state_merges"] == expected_merges, (entry, metrics)
        for name, limits in (("states", ["--max-states", "8"]),
                             ("queries", ["--max-solver-queries", "8"])):
            metrics = run_modes(args, root / name, "diamonds.ll", "diamond_top",
                                "UNKNOWN", limits)
            assert all(row["state_merges"] == 0 for row in metrics.values())
        metrics = run_modes(args, root / "loop-bound", "loop-branches.ll",
                            "loop_top", "UNKNOWN", ["--loop-bound", "2"])
        assert all(row["state_merges"] == 0 for row in metrics.values())
        assert (metrics["merge"]["solver_backend_queries"] >
                metrics["off"]["solver_backend_queries"]), metrics
        metrics = run_modes(args, root / "unsafe", "diamonds.ll",
                            "unsafe_after_join", "UNSAFE-UB", [])
        assert all(row["state_merges"] == 0 for row in metrics.values())
        assert (metrics["merge"]["solver_backend_queries"] >
                metrics["off"]["solver_backend_queries"]), metrics

        # Three diamond choices followed by a finite freeze produce 2048
        # baseline clauses, which exceed the relation disjunction depth limit.
        fixture = root / "relation-depth.ll"
        fixture.write_text(source.split("  %bit3 =", 1)[0] + """\
  %choice = freeze i8 undef
  %out = xor i8 %choice, %result2
  store i8 %out, ptr %output, align 1
  ret void
}
""")
        check_relation_depth(args, root / "depth", fixture, "diamond_top", 2048)


if __name__ == "__main__":
    main()
