#!/usr/bin/env python3
"""Compare Clang-generated M6 probe IR with bytes from the same native source."""

import hashlib
import re
import subprocess
import sys
import tempfile
from pathlib import Path


PROBES = {
    "m6_compiled_packed_copy_byte": ("PC", 1),
    "m6_compiled_unpacked_copy_byte": ("UC", 4),
    "m6_compiled_nested_copy_byte": ("NC", 14),
    "m6_compiled_tail_copy_byte": ("TC", 0),
}


def run(command):
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode:
        raise AssertionError(
            f"{command[0]} exited {result.returncode}:\n{result.stderr}\n{result.stdout}"
        )
    return result.stdout


def main(clang, source, native, executor):
    source = Path(source)
    native_bytes = {}
    for line in run([native]).splitlines():
        name, size, *values = line.split()
        if int(size) != len(values):
            raise AssertionError(f"native {name} size mismatch")
        native_bytes[name] = [int(value) for value in values]

    with tempfile.TemporaryDirectory(prefix="formal-m6-clang-ir-") as directory:
        ir = Path(directory) / "native.ll"
        run([clang, "-std=c++17", "-O0", "-S", "-emit-llvm",
             str(source), "-o", str(ir)])
        ir_text = ir.read_text(encoding="utf-8")
        for entry, (name, offset) in PROBES.items():
            body = re.search(rf"^define [^\n]*@{entry}\([^\n]*\n.*?^}}", ir_text,
                             re.MULTILINE | re.DOTALL)
            if not body or "@llvm.memcpy" not in body.group() or "load i8" not in body.group():
                raise AssertionError(f"Clang IR omitted {entry}'s byte-copy path")
            output = run([executor, str(ir), "--m4", "--entry", entry])
            value = re.search(r"return=bv8:(\d+)", output)
            if "M4 status M4_EXECUTOR_COMPLETE" not in output or not value:
                raise AssertionError(f"{entry} did not complete: {output}")
            expected = native_bytes[name][offset]
            if int(value.group(1)) != expected:
                raise AssertionError(
                    f"{entry}: native {name}[{offset}]={expected}, "
                    f"executor={value.group(1)}"
                )

    digest = hashlib.sha256(source.read_bytes()).hexdigest()
    print(f"M6 compiled C++ source sha256: {digest}")
    print(f"M6 Clang-generated IR matches {len(PROBES)} native defined bytes")


if __name__ == "__main__":
    main(*sys.argv[1:])
