#!/usr/bin/env python3
"""Check natural-loop structure against dynamic frame and activation history."""

import argparse
import json
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def run(executor, ir, root, entry, bound):
    output = root / entry
    process = subprocess.run([
        executor, ir, "--entry", entry, "--allow-unversioned-ir",
        "--loop-bound", str(bound), "--output-dir", str(output),
    ], capture_output=True, text=True)
    result = json.loads((output / "result.json").read_text())
    return process, result


def histories(path):
    return path["loop_bounds"]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--ir", required=True)
    args = parser.parse_args()
    cases = (("break_loop", 1, [1], 1),
             ("continue_loop", 2, [2], 1),
             ("multiple_exits", 1, [1], 2),
             ("nested_loops", 2, [2, 2, 2], 1),
             ("repeated_callee", 1, [1, 1], 1),
             ("recursive_top", 1, [1, 1, 1], 1))
    with RetainedTemporaryDirectory(prefix="formal-m8-structure-") as temp:
        root = pathlib.Path(temp)
        for entry, bound, expected, path_count in cases:
            process, result = run(args.formal_check, args.ir, root,
                                  entry, bound)
            assert process.returncode == 0, (entry, process.stdout, process.stderr)
            assert result["status"] == "SAFE", (entry, result)
            assert len(result["paths"]) == path_count, (entry, result)
            for path in result["paths"]:
                history = histories(path)
                assert sorted(item["traversed_backedges"] for item in history) == (
                    sorted(expected)), (entry, history)
                assert len({item["activation_id"] for item in history}) == (
                    len(history)), (entry, history)
                if entry in ("repeated_callee", "recursive_top"):
                    assert len({item["frame_id"] for item in history}) == (
                        len(history)), (entry, history)
                if entry == "nested_loops":
                    inner = [item for item in history
                             if item["header"] == "inner_header"]
                    outer = [item for item in history
                             if item["header"] == "outer_header"]
                    assert len(inner) == 2 and len(outer) == 1, history
            print(f"{entry}: SAFE, {path_count} completed path(s)")

        for entry, source, count in (("formal_override", "formal.loop.bound", 2),
                                     ("unroll_override", "llvm.loop.unroll.count", 1)):
            process, result = run(args.formal_check, args.ir, root, entry, 0)
            assert process.returncode == 0 and result["status"] == "SAFE", (
                entry, process.stdout, result)
            history = histories(result["paths"][0])
            assert len(history) == 1, history
            assert history[0]["bound_source"] == source, history
            assert history[0]["backedge_bound"] == count, history
            assert history[0]["traversed_backedges"] == count, history
            print(f"{entry}: {source} overrides global zero")

        process, result = run(args.formal_check, args.ir, root,
                              "forked_loop", 1)
        assert process.returncode == 2, (process.stdout, process.stderr)
        assert result["status"] == "UNKNOWN", result
        assert result["relation_completeness"] == "UNDER_APPROXIMATION", result
        assert result["incompleteness_reasons"] == {
            "UNKNOWN-LOOP-BOUND": 1}, result
        assert sorted(path["status"] for path in result["paths"]) == [
            "COMPLETED", "INCOMPLETE"], result
        assert all(histories(path)[0]["traversed_backedges"] == 1
                   for path in result["paths"]), result
        assert len({path["state_id"] for path in result["paths"]}) == 2, result
        print("forked_loop: independent completed and incomplete siblings")


if __name__ == "__main__":
    main()
