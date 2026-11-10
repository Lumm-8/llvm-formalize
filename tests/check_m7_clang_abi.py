#!/usr/bin/env python3
"""Run the pinned Clang byval/sret program natively and through the executor."""

import re
import subprocess
import sys
import tempfile
from pathlib import Path


def checked(command):
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode:
        raise AssertionError(
            f"{command[0]} exited {result.returncode}:\n"
            f"{result.stderr}\n{result.stdout}"
        )
    return result.stdout


def main(clang, source, executor):
    source = Path(source).resolve()
    repository = source.parents[2]
    snapshot = repository / "tests/ir/m7/clang-byval-sret.ll"
    with tempfile.TemporaryDirectory(prefix="formal-m7-clang-abi-") as tmp:
        ir = Path(tmp) / "abi.ll"
        native = Path(tmp) / "native"
        checked([clang, "-O0", "-S", "-emit-llvm", "-Xclang",
                 "-disable-O0-optnone", str(source), "-o", str(ir)])
        text = ir.read_text(encoding="utf-8")
        retained = snapshot.read_text(encoding="utf-8")
        generated_body = text[text.index("; ModuleID ="):]
        snapshot_body = retained[retained.index("; ModuleID ="):]
        generated_body = generated_body.replace(str(source), "tests/c/m7_abi.c")
        if generated_body != snapshot_body:
            raise AssertionError(
                "pinned Clang ABI snapshot differs from tests/c/m7_abi.c")
        if not re.search(r"define .*@make_triple\(ptr .*sret\(%struct.Triple\)", text):
            raise AssertionError("pinned Clang did not emit the expected sret ABI")
        if not re.search(r"define .*@sum_triple\(ptr .*byval\(%struct.Triple\)", text):
            raise AssertionError("pinned Clang did not emit the expected byval ABI")
        if not re.search(r"define .*@mutate_pointer_triple\(ptr .*byval\(%struct.PointerTriple\)", text):
            raise AssertionError("pinned Clang did not emit pointer-bearing byval ABI")
        checked([clang, "-O0", "-DM7_ABI_NATIVE_MAIN", source, "-o", str(native)])
        checked([str(native)])
        output = checked([executor, str(ir), "--m4", "--entry", "abi_entry"])
        if "M4 status M4_EXECUTOR_COMPLETE" not in output or \
                "return=bv64:24" not in output:
            raise AssertionError(f"Clang ABI IR did not match native run:\n{output}")
    print("M7 pinned Clang byval/sret native and executor runs agree")


if __name__ == "__main__":
    main(*sys.argv[1:])
