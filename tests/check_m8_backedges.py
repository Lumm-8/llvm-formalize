#!/usr/bin/env python3
"""Pin the exact CFG edge at which a loop bound stops execution."""

import argparse
import json
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


IR = '''target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@output_name = private constant [2 x i8] c"y\\00"
declare void @__formal_register_output(ptr, i64, ptr)

define void @formal_top() {
entry:
  %output = alloca i8, align 1
  call void @__formal_register_output(ptr %output, i64 1, ptr @output_name)
  br label %header
header:
  %i = phi i32 [ 0, %entry ], [ %next, %latch ]
  %forbidden = icmp eq i32 %i, 3
  br i1 %forbidden, label %error, label %test
error:
  store i8 99, ptr %output, align 1
  unreachable
test:
  %more = icmp ult i32 %i, TRIPS
  br i1 %more, label %latch, label %done
latch:
  %next = add i32 %i, 1
  br label %header
done:
  %value = trunc i32 %i to i8
  store i8 %value, ptr %output, align 1
  ret void
}
'''


def run(executor, root, bound, trips):
    ir = root / f"bound{bound}-trips{trips}.ll"
    ir.write_text(IR.replace("TRIPS", str(trips)))
    output = root / f"bound{bound}-trips{trips}"
    process = subprocess.run([
        executor, str(ir), "--entry", "formal_top", "--allow-unversioned-ir",
        "--loop-bound", str(bound), "--output-dir", str(output),
    ], capture_output=True, text=True)
    result = json.loads((output / "result.json").read_text())
    return process, result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m8-backedges-") as temporary:
        root = pathlib.Path(temporary)
        reference = None
        reference_relation = None
        for bound, trips, expected in ((2, 1, "SAFE"), (2, 2, "SAFE"),
                                       (2, 3, "UNKNOWN"), (0, 0, "SAFE"),
                                       (0, 1, "UNKNOWN")):
            process, result = run(args.formal_check, root, bound, trips)
            assert result["status"] == expected, (bound, trips, process.stdout)
            assert process.returncode == (0 if expected == "SAFE" else 2)
            assert result["relation_completeness"] == (
                "COMPLETE" if expected == "SAFE" else "UNDER_APPROXIMATION")
            assert not any(path["reason"] == "REACHED-UNREACHABLE"
                           for path in result["paths"]), result
            path = result["paths"][0]
            assert len(path["loop_bounds"]) == 1, path
            history = path["loop_bounds"][0]
            assert history["traversed_backedges"] == min(bound, trips), history
            assert history["backedge_bound"] == bound, history
            assert history["bound_source"] == "global-default", history
            assert history["activation_id"] > 0 and history["frame_id"] > 0
            if expected == "UNKNOWN":
                assert path["reason"] == "UNKNOWN-LOOP-BOUND", path
                assert path["source"]["block"] == "latch", path
                assert path["loop"]["attempted_backedge"] == bound + 1, path
                assert path["loop"]["suppressed_edge"] == {
                    "from": "latch", "to": "header"}, path
                assert path["limit_value"] == bound, path
                assert path["observed_value"] == bound + 1, path
                assert result["relation_metadata"]["paths"] == 0, result
            if (bound, trips) == (2, 3):
                reference = result
                reference_relation = (root / "bound2-trips3" / "relation.smt2").read_bytes()
            print(f"bound={bound} traversals={trips}: {expected}")

        # The same bounded run must produce the same path ledger and relation.
        _, repeated = run(args.formal_check, root, 2, 3)
        assert repeated["paths"] == reference["paths"]
        assert (root / "bound2-trips3" / "relation.smt2").read_bytes() == (
            reference_relation)

        for bound in (2**63 - 1, 2**63, 2**64 - 1):
            process, result = run(args.formal_check, root, bound, 1)
            encoded = bound if bound <= 2**63 - 1 else str(bound)
            assert process.returncode == 0 and result["status"] == "SAFE"
            assert result["limits"]["default_loop_backedge_bound"] == encoded
            assert result["paths"][0]["loop_bounds"][0]["backedge_bound"] == encoded


if __name__ == "__main__":
    main()
