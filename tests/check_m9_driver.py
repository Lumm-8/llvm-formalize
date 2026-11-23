#!/usr/bin/env python3
"""Exercise the M9 verdict boundary through the preflighted driver."""

import argparse
import json
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--ir", required=True)
    parser.add_argument("--stp")
    args = parser.parse_args()

    cases = (
        ("defined_freeze", "SAFE", 0),
        ("one_past", "SAFE", 0),
        ("one_past_load", "UNSAFE", 1),
        ("one_past_store", "UNSAFE", 1),
        ("past_end", "UNKNOWN", 2),
        ("wide_index", "UNKNOWN", 2),
        ("outside_reenter", "UNKNOWN", 2),
        ("zero_div", "UNSAFE-UB", 1),
        ("signed_div_overflow", "UNSAFE-UB", 1),
        ("symbolic_div", "UNSAFE-UB", 1),
        ("symbolic_nsw_branch", "UNSAFE-UB", 1),
        ("symbolic_shift_branch", "UNSAFE-UB", 1),
        ("symbolic_exact_branch", "UNSAFE-UB", 1),
        ("exact_div", "SAFE", 0),
        ("overshift", "SAFE", 0),
        ("unselected_poison_select", "SAFE", 0),
        ("unselected_poison_phi", "SAFE", 0),
        ("flagged", "SAFE", 0),
        ("undef_store", "UNKNOWN", 2),
        ("undef_independent", "UNKNOWN", 2),
        ("poison_branch", "UNSAFE-UB", 1),
        ("poison_arithmetic", "UNSAFE-UB", 1),
        ("poison_noundef_call", "UNSAFE-UB", 1),
        ("symbolic_noundef_call", "UNSAFE-UB", 1),
        ("callsite_noundef_call", "UNSAFE-UB", 1),
        ("poison_ignored_call", "SAFE", 0),
        ("poison_used_call", "UNSAFE-UB", 1),
        ("frozen", "SAFE", 0),
        ("frozen_reused", "SAFE", 0),
        ("frozen_undef", "SAFE", 0),
        ("frozen_independent", "SAFE", 0),
        ("frozen_loop", "SAFE", 0),
        ("frozen_recursive", "SAFE", 0),
        ("frozen_store", "SAFE", 0),
        ("frozen_wide", "UNKNOWN", 2),
        ("frozen_conditional", "SAFE", 0),
        ("width_add_over_limit", "UNKNOWN", 2),
        ("width_sub_over_limit", "UNKNOWN", 2),
        ("width_mul_over_limit", "UNKNOWN", 2),
        ("width_shl_over_limit", "UNKNOWN", 2),
        ("width_at_limit", "SAFE", 0),
    )
    preflight_unknown = {
        "undef_store": "UNSUPPORTED_VALUE_SEMANTICS",
        "undef_independent": "UNSUPPORTED_VALUE_SEMANTICS",
        "frozen_wide": "FREEZE_INSTRUCTION",
    }
    with RetainedTemporaryDirectory(prefix="formal-m9-driver-") as temporary:
        root = pathlib.Path(temporary)
        for entry, verdict, code in cases:
            directory = root / entry
            process = subprocess.run(
                [args.formal_check, args.ir, "--entry", entry,
                 "--allow-unversioned-ir", "--output-dir", str(directory)],
                capture_output=True, text=True,
            )
            assert process.returncode == code, (entry, process.stdout,
                                                process.stderr)
            if entry in preflight_unknown:
                assert ("preflight " + preflight_unknown[entry]
                        in process.stderr), (entry, process.stderr)
                report = json.loads((directory / "result.json").read_text())
                assert report["schema_version"] == 1, (entry, report)
                assert report["phase"] == "M0_PREFLIGHT", (entry, report)
                assert report["status"] == "UNKNOWN", (entry, report)
                assert report["reason"] == preflight_unknown[entry], (
                    entry, report)
                assert report["entry"] == entry, (entry, report)
                continue
            report = json.loads((directory / "result.json").read_text())
            assert report["status"] == verdict, (entry, report)
            if entry.endswith("_over_limit"):
                assert report["relation_completeness"] == "UNDER_APPROXIMATION", report
                assert report["incompleteness_reasons"] == {
                    "UNKNOWN-EXPRESSION-WIDTH-LIMIT": 1
                }, report
                assert report["path_counts"]["internal_error"] == 0, report
                assert report["paths"][0]["source"]["opcode"] == entry.split("_")[1], report
            if entry in ("one_past_load", "one_past_store"):
                assert report["reason"] == "OUT_OF_BOUNDS", report
                witness = json.loads(
                    (directory / "counterexample.json").read_text())
                assert witness["reason"] == "OUT_OF_BOUNDS", witness
                assert witness["semantic_kind"] == "PROPERTY_ERROR", witness
                opcode = entry.removeprefix("one_past_")
                assert witness["source"]["opcode"] == opcode, witness
                assert report["path_counts"]["error"] == 1, report
                continue
            if entry == "defined_freeze":
                assert report["relation_completeness"] == "COMPLETE", report
                relation = (directory / "relation.smt2").read_text()
                assert "input.x.0" in relation and "output.y.0" in relation
            if entry in ("frozen", "frozen_reused", "frozen_undef",
                         "frozen_independent", "frozen_loop",
                         "frozen_recursive", "frozen_store",
                         "frozen_conditional"):
                assert report["relation_completeness"] == "COMPLETE", report
                relation = (directory / "relation.smt2").read_text()
                assert "freeze.choice" not in relation, relation
            if entry in ("frozen_independent", "frozen_loop",
                         "frozen_recursive"):
                assert report["path_counts"]["normal"] == 4, report
            if entry == "frozen_store":
                assert report["path_counts"]["normal"] == 2, report
            if entry == "frozen_conditional":
                assert report["path_counts"]["normal"] == 3, report
                if args.stp:
                    suffix = "(assert |formal_relation|)\n(check-sat)\n"
                    assert relation.endswith(suffix), relation
                    expected = ("(or (= |output.y.0| (_ bv1 8)) "
                                "(and (= ((_ extract 0 0) |input.x.0|) "
                                "(_ bv1 1)) "
                                "(= |output.y.0| (_ bv0 8))))")
                    query = directory / "freeze-equivalence.smt2"
                    query.write_text(relation.removesuffix(suffix) +
                                     "(assert (xor |formal_relation| " +
                                     expected + "))\n(check-sat)\n")
                    checked = subprocess.run(
                        [args.stp, "--SMTLIB2", str(query)],
                        capture_output=True, text=True, timeout=30)
                    assert checked.returncode == 0 and (
                        checked.stdout.strip().lower() == "unsat"), (
                        checked.stdout, checked.stderr)
            if verdict == "UNSAFE-UB":
                witness = json.loads(
                    (directory / "counterexample.json").read_text())
                expected_reason = (
                    "LLVM-UB-SDIV-OVERFLOW" if entry == "signed_div_overflow"
                    else "LLVM-UB-POISON-CONSUMED" if entry in (
                        "symbolic_nsw_branch", "symbolic_shift_branch",
                        "symbolic_exact_branch", "poison_branch",
                        "poison_arithmetic", "poison_noundef_call",
                        "symbolic_noundef_call", "callsite_noundef_call",
                        "poison_used_call")
                    else "LLVM-UB-DIVIDE-BY-ZERO")
                assert witness["reason"] == expected_reason, witness
                assert witness["semantic_kind"] == "LLVM_UB", witness
                assert witness["replay_class"] == "IR_ONLY", witness
                assert witness["source"]["function"], witness
                assert witness["source"]["opcode"], witness
                assert witness["call_stack"], witness
                assert witness["path_constraints_smt2"].startswith(
                    "(set-logic "), witness
                assert witness["trigger_condition_smt2"], witness
                if entry == "symbolic_noundef_call" and args.stp:
                    query = directory / "ub-path.smt2"
                    query.write_text(witness["path_constraints_smt2"])
                    checked = subprocess.run(
                        [args.stp, "--SMTLIB2", str(query)],
                        capture_output=True, text=True, timeout=30)
                    assert checked.returncode == 0 and (
                        checked.stdout.strip().lower().startswith("sat")), (
                        checked.stdout, checked.stderr)
                if entry == "poison_used_call":
                    assert [frame["function"] for frame in witness["call_stack"]] == [
                        "poison_used_call", "branch_on_arg"], witness
                if entry == "symbolic_div":
                    assert witness["inputs"] == [
                        {"name": "x", "bytes": [0]}], witness
                if entry in ("symbolic_nsw_branch", "symbolic_noundef_call"):
                    assert witness["inputs"] == [
                        {"name": "x", "bytes": [127]}], witness
                    assert any(path["status"] == "COMPLETED"
                               for path in report["paths"]), report
            else:
                assert report.get("counterexample_file", "") == "", report


if __name__ == "__main__":
    main()
