#!/usr/bin/env python3
"""Exhaustive native byte outcomes, then both directions against SMT output."""

import argparse
import json
import pathlib
import shutil
import subprocess
import tempfile


def run(args, **kwargs):
    return subprocess.run(args, check=True, text=True, capture_output=True, **kwargs)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--ir")
    parser.add_argument("--clang")
    parser.add_argument("--source")
    parser.add_argument("--native", required=True)
    parser.add_argument("--include", required=True)
    parser.add_argument("--cc", required=True)
    parser.add_argument("--stp", required=True)
    parser.add_argument("--entry", default="scalar")
    parser.add_argument("--assumed", action="store_true")
    parser.add_argument("--assumed-plus-one", action="store_true")
    parser.add_argument("--two-outputs", action="store_true")
    parser.add_argument("--per-input", action="store_true")
    parser.add_argument("--expect-call-events", action="store_true")
    args = parser.parse_args()
    root = pathlib.Path(tempfile.mkdtemp(prefix="formal-m5-truth-"))
    try:
        ir = args.ir
        if args.clang and args.source:
            ir = str(root / "clang.ll")
            run([args.clang, "-O0", "-g", "-S", "-emit-llvm",
                 "-I", args.include, args.source, "-o", ir])
        assert ir
        executable = root / "native"
        run([args.cc, "-std=c11", "-O0", "-I", args.include,
             "-DASSUMED=1" if args.assumed else "-DNATIVE_SCALAR=1",
             "-DOUTPUT_PLUS_ONE=1" if args.assumed_plus_one
             else "-DNATIVE_OUTPUT_UNCHANGED=1",
             args.native, "-o", str(executable)])
        native_lines = run([str(executable)]).stdout.splitlines()
        observations = {}
        for line in native_lines:
            in_text, *out_text = line.split()
            input_value = int(in_text)
            output_value = tuple(int(value) for value in out_text)
            assert len(output_value) == (2 if args.two_outputs else 1)
            assert input_value not in observations
            observations[input_value] = output_value
        assert set(observations) == (set(range(4)) if args.assumed
                                     else set(range(256)))

        artifacts = root / "artifacts"
        result = run([args.executor, ir, "--m5", "--entry", args.entry,
                      "--output-dir", str(artifacts)])
        assert "M5 status SAFE" in result.stdout, result.stdout
        if args.expect_call_events:
            metadata = json.loads((artifacts / "result.json").read_text())
            for path in metadata["paths"]:
                if path["status"] != "COMPLETED":
                    continue
                entered = [event for event in path["events"]
                           if event["kind"] == "call_enter"]
                returned = [event for event in path["events"]
                            if event["kind"] == "call_return"]
                assert [event["model"] for event in entered] == [
                    "write_result", "add_one"], path
                assert [event["model"] for event in returned] == [
                    "add_one", "write_result"], path
                assert len({event["frame_id"] for event in entered}) == 2
            assert metadata["max_call_depth_observed"] >= 3
        relation = (artifacts / "relation.smt2").read_text()
        assert relation.endswith("(assert |formal_relation|)\n(check-sat)\n")
        prelude = relation.removesuffix("(assert |formal_relation|)\n(check-sat)\n")
        output_names = ("y", "z") if args.two_outputs else ("y",)
        terms = []
        for input_value in sorted(observations):
            equalities = [f"(= |input.x.0| (_ bv{input_value} 8))"]
            equalities += [f"(= |output.{name}.0| (_ bv{value} 8))"
                           for name, value in zip(output_names,
                                                  observations[input_value])]
            terms.append("(and " + " ".join(equalities) + ")")
        expected = "(or " + " ".join(terms) + ")"
        for direction, formula in (
                ("soundness", "(and " + expected + " (not |formal_relation|))"),
                ("precision", "(and |formal_relation| (not " + expected + "))"),
                ("xor", "(xor |formal_relation| " + expected + ")")):
            query = root / (direction + ".smt2")
            query.write_text(prelude + "(assert " + formula + ")\n(check-sat)\n")
            answer = run([args.stp, "--SMTLIB2", str(query)]).stdout.strip().lower()
            assert answer == "unsat", (direction, answer)
            print(direction + ": UNSAT for all native domain inputs")
        if args.per_input:
            for input_value, observed in sorted(observations.items()):
                fixed_input = f"(= |input.x.0| (_ bv{input_value} 8))"
                output_equalities = [
                    f"(= |output.{name}.0| (_ bv{value} 8))"
                    for name, value in zip(output_names, observed)]
                fixed_output = (output_equalities[0] if len(output_equalities) == 1
                                else "(and " + " ".join(output_equalities) + ")")
                for direction, formula, expected_answer in (
                        ("native-in-relation",
                         f"(and |formal_relation| {fixed_input} {fixed_output})",
                         "sat"),
                        ("no-extra-output",
                         f"(and |formal_relation| {fixed_input} (not {fixed_output}))",
                         "unsat")):
                    query = root / f"input-{input_value}-{direction}.smt2"
                    query.write_text(prelude + "(assert " + formula + ")\n(check-sat)\n")
                    answer = run([args.stp, "--SMTLIB2", str(query)]).stdout.strip().lower()
                    assert answer == expected_answer, (input_value, direction, answer)
            print(f"per-input SAT and no-extra-output: {len(observations)} inputs")
    except Exception:
        print(f"retained failing M5 queries and artifacts: {root}")
        raise
    else:
        shutil.rmtree(root)


if __name__ == "__main__":
    main()
