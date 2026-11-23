#!/usr/bin/env python3
"""Check flags against Python integers and pinned LLVM APInt."""

import argparse
import json
import pathlib
import random
import re
import subprocess
import tempfile


def signed(value, width):
    boundary = 1 << (width - 1)
    return value if value < boundary else value - (1 << width)


def poisoned(opcode, flag, left, right, width):
    if opcode == "shl" and right >= width:
        return True
    a = signed(left, width) if flag == "nsw" else left
    b = signed(right, width) if flag == "nsw" and opcode != "shl" else right
    if opcode == "add":
        value = a + b
    elif opcode == "sub":
        value = a - b
    elif opcode == "mul":
        value = a * b
    else:
        value = a << right
    if flag == "nsw":
        minimum = -(1 << (width - 1))
        maximum = (1 << (width - 1)) - 1
        return not (minimum <= value <= maximum)
    return not (0 <= value < (1 << width))


def exact_poisoned(opcode, left, right, width):
    if opcode in ("udiv", "sdiv"):
        if right == 0:
            return True
        if opcode == "sdiv":
            a, b = signed(left, width), signed(right, width)
            return (a == -(1 << (width - 1)) and b == -1) or abs(a) % abs(b) != 0
        return left % right != 0
    if right >= width:
        return True
    return (left & ((1 << right) - 1)) != 0


OPERATIONS = tuple(
    (opcode, flag)
    for opcode in ("add", "sub", "mul", "shl")
    for flag in ("nsw", "nuw")
) + tuple((opcode, "exact") for opcode in ("udiv", "sdiv", "lshr", "ashr"))


def wide_pairs(width, opcode, rng):
    limit = 1 << width
    high = limit >> 1
    boundaries = (0, 1, 2, high - 1, high, high + 1, limit - 2, limit - 1)
    if opcode in ("shl", "lshr", "ashr"):
        rights = (0, 1, width - 1, width, width + 1, limit - 1)
        random_pairs = ((rng.getrandbits(width), rng.randrange(width + 2))
                        for _ in range(8))
    else:
        rights = boundaries
        random_pairs = ((rng.getrandbits(width), rng.getrandbits(width))
                        for _ in range(8))
    pairs = {(left, right) for left in boundaries for right in rights}
    pairs.update(random_pairs)
    if opcode in ("udiv", "sdiv"):
        # Sample exact quotients beyond the trivial zero and unit divisors,
        # with adjacent inexact dividends.
        for _ in range(8):
            divisor = rng.randrange(2, high - 1)
            quotient = rng.randrange(1, (high - 2) // divisor + 1)
            dividend = divisor * quotient
            pairs.update(((dividend, divisor), (dividend + 1, divisor)))
    return sorted(pairs)


def cases(widths):
    rng = random.Random(9)
    for width in widths:
        for opcode, flag in OPERATIONS:
            if width <= 3:
                pairs = ((left, right) for left in range(1 << width)
                         for right in range(1 << width))
            else:
                pairs = wide_pairs(width, opcode, rng)
            for left, right in pairs:
                yield width, opcode, flag, left, right


def expected_reason(width, opcode, flag, left, right):
    if opcode in ("udiv", "sdiv") and right == 0:
        return "LLVM-UB-DIVIDE-BY-ZERO"
    if opcode == "sdiv" and left == 1 << (width - 1) and right == (1 << width) - 1:
        return "LLVM-UB-SDIV-OVERFLOW"
    poison = (exact_poisoned(opcode, left, right, width) if flag == "exact"
              else poisoned(opcode, flag, left, right, width))
    return "LLVM-UB-POISON-CONSUMED" if poison else None


def defined_result(width, opcode, left, right):
    if opcode == "sdiv":
        # Called only when division is exact, so floor and truncation agree.
        value = signed(left, width) // signed(right, width)
    elif opcode == "ashr":
        value = signed(left, width) >> right
    else:
        value = {
            "add": lambda: left + right, "sub": lambda: left - right,
            "mul": lambda: left * right, "shl": lambda: left << right,
            "udiv": lambda: left // right, "lshr": lambda: left >> right,
        }[opcode]()
    return value % (1 << width)


def check_optimized_results(opt, defined_cases):
    source = []
    for index, (case, _) in enumerate(defined_cases):
        width, opcode, flag, left, right = case
        source.append(
            f"define i{width} @case_{index}() {{\n"
            f"  %v = {opcode} {flag} i{width} {left}, {right}\n"
            f"  ret i{width} %v\n}}\n")
    # Only compare defined executions. An optimizer can legally refine poison
    # and UB, so its concrete output cannot serve as their oracle.
    optimized = subprocess.run(
        [opt, "-passes=instcombine", "-verify-each", "-S", "-o", "-", "-"],
        input="".join(source), capture_output=True, text=True, check=True,
        timeout=60)
    returns = dict(re.findall(
        r"define i\d+ @case_(\d+)\(\) \{\s*ret i\d+ (-?\d+)\s*\}",
        optimized.stdout))
    assert len(returns) == len(defined_cases), (
        len(returns), len(defined_cases), optimized.stderr)
    for index, (case, expected) in enumerate(defined_cases):
        assert int(returns[str(index)]) % (1 << case[0]) == expected, (
            case, returns[str(index)], expected)
    print(f"Pinned opt InstCombine agrees on {len(defined_cases)} defined cases")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--scalar-executor", required=True)
    parser.add_argument("--apint-oracle", required=True)
    parser.add_argument("--wide", action="store_true")
    parser.add_argument("--opt")
    args = parser.parse_args()
    test_cases = list(cases((8, 24, 64, 128) if args.wide else (1, 2, 3)))
    oracle_input = "".join(" ".join(map(str, case)) + "\n" for case in test_cases)
    oracle = subprocess.run([args.apint_oracle], input=oracle_input,
                            capture_output=True, text=True, check=True, timeout=30)
    answers = oracle.stdout.splitlines()
    assert len(answers) == len(test_cases), oracle
    defined_cases = []
    with tempfile.TemporaryDirectory(prefix="formal-m9-flags-") as temporary:
        root = pathlib.Path(temporary)
        module = root / "flags.ll"
        output = root / "result"
        for case, answer in zip(test_cases, answers):
            width, opcode, flag, left, right = case
            reason = expected_reason(*case)
            assert answer == ("1" if reason else "0"), (case, reason, answer)
            expected = 0 if reason else defined_result(width, opcode, left, right)
            module.write_text(
                'target triple = "x86_64-unknown-linux-gnu"\n'
                'define void @check() {\n'
                f'  %v = {opcode} {flag} i{width} {left}, {right}\n'
                f'  %c = icmp eq i{width} %v, {expected}\n'
                '  br i1 %c, label %done, label %bad\n'
                'done:\n  ret void\nbad:\n  unreachable\n}\n')
            result = subprocess.run(
                [args.scalar_executor, str(module), "--m5", "--entry", "check",
                 "--output-dir", str(output)], capture_output=True, text=True,
                timeout=30)
            assert result.returncode == (1 if reason else 0), (case, result)
            report = json.loads((output / "result.json").read_text())
            assert report["status"] == ("UNSAFE-UB" if reason else "SAFE"), (
                case, report)
            if reason:
                assert report["reason"] == reason, (case, report)
            else:
                defined_cases.append((case, expected))
    if args.opt:
        check_optimized_results(args.opt, defined_cases)
    print(f"Checked {len(test_cases)} flag cases against Python and LLVM APInt")


if __name__ == "__main__":
    main()
