#!/usr/bin/env python3
"""Prove the exported byte relation preserves target endian scalar packing."""

import argparse
import pathlib
import subprocess
from m5_test_temp import RetainedTemporaryDirectory


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--executor", required=True)
    parser.add_argument("--little-ir", required=True)
    parser.add_argument("--big-ir", required=True)
    parser.add_argument("--stp", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m5-multibyte-") as root:
        root = pathlib.Path(root)
        for endian, ir in (("little", args.little_ir), ("big", args.big_ir)):
            artifacts = root / endian
            result = subprocess.run(
                [args.executor, ir, "--m5", "--entry", "multibyte",
                 "--output-dir", str(artifacts)], capture_output=True, text=True)
            assert result.returncode == 0, result.stdout + result.stderr
            relation = (artifacts / "relation.smt2").read_text()
            prelude = relation.removesuffix(
                "(assert |formal_relation|)\n(check-sat)\n")
            assert prelude != relation
            high, low = (1, 0) if endian == "little" else (0, 1)
            input_word = f"(concat |input.x.{high}| |input.x.{low}|)"
            output_word = f"(concat |output.y.{high}| |output.y.{low}|)"
            expected = f"(= {output_word} (bvadd {input_word} (_ bv1 16)))"
            query = root / (endian + ".smt2")
            query.write_text(prelude +
                             f"(assert (xor |formal_relation| {expected}))\n"
                             "(check-sat)\n")
            answer = subprocess.run(
                [args.stp, "--SMTLIB2", str(query)], check=True,
                capture_output=True, text=True).stdout.strip().lower()
            assert answer == "unsat", (endian, answer)
            print(endian + " endian byte packing: equivalent")


if __name__ == "__main__":
    main()
