#!/usr/bin/env python3
"""Compare native defined bytes and check typed-copy padding semantics."""

import hashlib
import re
import subprocess
import sys
import tempfile
from pathlib import Path


def run(command):
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode:
        raise AssertionError(
            f"{command[0]} exited {result.returncode}:\n{result.stderr}\n{result.stdout}"
        )
    return result.stdout


def body(kind, size):
    prefix = [f"  %slot = alloca %{kind}, align {4 if kind in 'UNTR' else 1}",
              f"  call void @llvm.memset.p0.i64(ptr %slot, i8 0, i64 {size}, i1 false)"]
    fields = {
        "P": [("i8", 7, "0, i32 0", 1), ("i32", 287454020, "0, i32 1", 1)],
        "U": [("i8", 7, "0, i32 0", 1), ("i32", 287454020, "0, i32 1", 4)],
        "N": [
            ("i8", 9, "0, i32 0, i32 0", 1),
            ("i32", 1432778632, "0, i32 0, i32 1", 4),
            ("i8", 1, "0, i32 1, i64 0, i32 0", 1),
            ("i32", 16909060, "0, i32 1, i64 0, i32 1", 1),
            ("i8", 2, "0, i32 1, i64 1, i32 0", 1),
            ("i32", 2695938256, "0, i32 1, i64 1, i32 1", 1),
        ],
        "T": [("i32", 305419896, "0, i32 0", 4),
              ("i8", 11, "0, i32 1", 1)],
        "R": [],
    }[kind]
    if kind == "R":
        prefix.append("  store i32 287454020, ptr %slot, align 4")
    for index, (ty, value, indices, align) in enumerate(fields):
        prefix.append(f"  %f{index} = getelementptr %{kind}, ptr %slot, i64 {indices}")
        prefix.append(f"  store {ty} {value}, ptr %f{index}, align {align}")
    return prefix


def copy_body(kind, size):
    lines = body(kind, size)
    align = 1 if kind == "P" else 4
    lines.extend([
        f"  %target = alloca %{kind}, align {align}",
        f"  call void @llvm.memset.p0.i64(ptr %target, i8 -91, i64 {size}, i1 false)",
        f"  %aggregate = load %{kind}, ptr %slot, align {align}",
        f"  store %{kind} %aggregate, ptr %target, align {align}",
    ])
    return lines


def main(native, executor):
    script_sha256 = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    native_lines = run([native]).splitlines()
    cases = {}
    for line in native_lines:
        name, size, *values = line.split()
        size = int(size)
        values = [int(value) for value in values]
        if size != len(values):
            raise AssertionError(f"native {name} size mismatch")
        cases[name] = values
    if set(cases) != {"P", "PC", "U", "UC", "N", "NC", "T", "TC", "R"}:
        raise AssertionError("native probe omitted an M6 aggregate")

    semantic_offsets = {
        "P": set(range(5)),
        "U": {0, 4, 5, 6, 7},
        "N": {0, *range(4, 18)},
        "T": set(range(5)),
        "R": set(range(4)),
    }

    ir = [
        'target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"',
        "%P = type <{ i8, i32 }>",
        "%U = type { i8, i32 }",
        "%N = type { %U, [2 x %P] }",
        "%T = type { i32, i8 }",
        "%R = type { i32 }",
        "declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)",
    ]
    for kind, values in cases.items():
        for index in range(len(values)):
            ir.append(f"define i8 @byte_{kind}_{index}() {{")
            is_copy = kind.endswith("C")
            base = kind[0] if is_copy else kind
            ir.extend(copy_body(base, len(values)) if is_copy
                      else body(base, len(values)))
            address = "%target" if is_copy else "%slot"
            ir.append(f"  %at = getelementptr i8, ptr {address}, i64 {index}")
            ir.append("  %value = load i8, ptr %at, align 1")
            ir.append("  ret i8 %value")
            ir.append("}")
    with tempfile.TemporaryDirectory(prefix="formal-m6-bytes-") as directory:
        path = Path(directory) / "bytes.ll"
        path.write_text("\n".join(ir) + "\n", encoding="utf-8")
        for kind, values in cases.items():
            for index, expected in enumerate(values):
                entry = f"byte_{kind}_{index}"
                output = run([executor, str(path), "--m4", "--entry", entry])
                if kind.endswith("C") and index not in semantic_offsets[kind[0]]:
                    if ("M4 status UNKNOWN" not in output or
                            "reason=UNINITIALIZED_READ" not in output):
                        raise AssertionError(
                            f"{entry} exposed aggregate padding: {output}"
                        )
                    continue
                match = re.search(r"return=bv8:(\d+)", output)
                if "M4 status M4_EXECUTOR_COMPLETE" not in output or not match:
                    raise AssertionError(f"{entry} did not complete: {output}")
                actual = int(match.group(1))
                if actual != expected:
                    raise AssertionError(
                        f"{kind}[{index}]: native={expected}, symbolic={actual}"
                    )
    compared = sum(len(values) for name, values in cases.items()
                   if not name.endswith("C"))
    compared += sum(len(semantic_offsets[name[0]]) for name in cases
                    if name.endswith("C"))
    print(f"M6 native comparison script sha256: {script_sha256}")
    print(f"M6 native bytes match symbolic memory for {compared} defined bytes; "
          "typed-copy padding is UNKNOWN")


if __name__ == "__main__":
    main(*sys.argv[1:])
