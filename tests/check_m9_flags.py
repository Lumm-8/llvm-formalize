#!/usr/bin/env python3
"""Exhaustively compare i2 LLVM flag poison with concrete arithmetic."""

import argparse
import pathlib
import subprocess
import tempfile


def signed(value):
    return value if value < 2 else value - 4


def poisoned(opcode, flag, left, right):
    if opcode == "shl" and right >= 2:
        return True
    a = signed(left) if flag == "nsw" else left
    b = signed(right) if flag == "nsw" and opcode != "shl" else right
    if opcode == "add":
        value = a + b
    elif opcode == "sub":
        value = a - b
    elif opcode == "mul":
        value = a * b
    else:
        value = a << right
    return not (-2 <= value <= 1) if flag == "nsw" else not (0 <= value <= 3)


def exact_poisoned(opcode, left, right):
    if opcode in ("udiv", "sdiv"):
        if right == 0:
            return True
        if opcode == "sdiv":
            a, b = signed(left), signed(right)
            return (a == -2 and b == -1) or abs(a) % abs(b) != 0
        return left % right != 0
    if right >= 2:
        return True
    return (left & ((1 << right) - 1)) != 0


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--scalar-executor", required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="formal-m9-flags-") as temporary:
        root = pathlib.Path(temporary)
        for opcode in ("add", "sub", "mul", "shl"):
            for flag in ("nsw", "nuw"):
                for left in range(4):
                    for right in range(4):
                        name = f"{opcode}_{flag}_{left}_{right}"
                        module = root / f"{name}.ll"
                        module.write_text(
                            'target triple = "x86_64-unknown-linux-gnu"\n'
                            f'define void @check() {{\n'
                            f'  %v = {opcode} {flag} i2 {left}, {right}\n'
                            '  %c = icmp eq i2 %v, %v\n'
                            '  br i1 %c, label %done, label %done\n'
                            'done:\n  ret void\n}\n')
                        process = subprocess.run(
                            [args.scalar_executor, str(module), "--m5",
                             "--entry", "check", "--output-dir", str(root / name)],
                            capture_output=True, text=True)
                        expected = "UNSAFE-UB" if poisoned(opcode, flag, left, right) else "SAFE"
                        actual = process.stdout.splitlines()[0] if process.stdout else ""
                        assert actual == f"M5 status {expected}", (
                            name, expected, process.returncode,
                            process.stdout, process.stderr)
        for opcode in ("udiv", "sdiv", "lshr", "ashr"):
            for left in range(4):
                for right in range(4):
                    name = f"{opcode}_exact_{left}_{right}"
                    module = root / f"{name}.ll"
                    module.write_text(
                        'target triple = "x86_64-unknown-linux-gnu"\n'
                        'define void @check() {\n'
                        f'  %v = {opcode} exact i2 {left}, {right}\n'
                        '  %c = icmp eq i2 %v, %v\n'
                        '  br i1 %c, label %done, label %done\n'
                        'done:\n  ret void\n}\n')
                    process = subprocess.run(
                        [args.scalar_executor, str(module), "--m5",
                         "--entry", "check", "--output-dir", str(root / name)],
                        capture_output=True, text=True)
                    expected = "UNSAFE-UB" if exact_poisoned(opcode, left, right) else "SAFE"
                    actual = process.stdout.splitlines()[0] if process.stdout else ""
                    assert actual == f"M5 status {expected}", (
                        name, expected, process.returncode,
                        process.stdout, process.stderr)


if __name__ == "__main__":
    main()
