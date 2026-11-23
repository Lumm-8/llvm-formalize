#!/usr/bin/env python3
"""Compare exported M9 guards and relations with independent Z3 formulas."""

import argparse
import json
import pathlib
import subprocess


def word(prefix, width):
    """Pack the public little-endian byte symbols into one bit vector."""
    symbols = [f"|{prefix}.{index}|" for index in reversed(range(width // 8))]
    if len(symbols) == 1:
        return symbols[0]
    return f"(concat {' '.join(symbols)})"


def specification(entry):
    if entry == "signed_add_exact_shift":
        # Signed add overflows iff operands have equal signs and the result
        # has the opposite sign. This uses no widened add or round trip.
        return 128, """
(define-fun sum () (_ BitVec 128) (bvadd a b))
(define-fun sign ((x (_ BitVec 128))) (_ BitVec 1) ((_ extract 127 127) x))
(define-fun poison_expected () Bool
  (or (and (= (sign a) (sign b)) (distinct (sign a) (sign sum)))
      (distinct ((_ extract 6 0) sum) #b0000000)))
(define-fun value_expected () (_ BitVec 128) (concat #b0000000 ((_ extract 127 7) sum)))
"""
    return 24, """
(define-fun poison_expected () Bool (bvult a b))
(define-fun value_expected () (_ BitVec 24) (bvsub a b))
"""


def extra_cases():
    """Symbolic i8 coverage for each supported wrap flag and exact opcode."""
    wraps = {
        "add": ("bvadd", "(and (= (sign a) (sign b)) "
                         "(distinct (sign a) (sign value_expected)))",
                "(bvult value_expected a)"),
        "sub": ("bvsub", "(and (distinct (sign a) (sign b)) "
                         "(distinct (sign a) (sign value_expected)))",
                "(bvult a b)"),
        "mul": (
            "bvmul",
            "(let ((product (bvmul ((_ sign_extend 8) a) ((_ sign_extend 8) b)))) "
            "(or (bvslt product #xff80) (bvsgt product #x007f)))",
            "(distinct ((_ extract 15 8) "
            "(bvmul ((_ zero_extend 8) a) ((_ zero_extend 8) b))) #x00)"),
        "shl": ("bvshl", "(distinct (bvashr value_expected b) a)",
                "(distinct (bvlshr value_expected b) a)"),
    }
    for opcode, (smt_op, signed, unsigned) in wraps.items():
        for flags, checks in (("nsw", [signed]), ("nuw", [unsigned]),
                              ("nsw nuw", [signed, unsigned])):
            if opcode == "shl":
                checks.append("(bvuge b #x08)")
            poison = checks[0] if len(checks) == 1 else f"(or {' '.join(checks)})"
            yield opcode, flags, "", "b", f"({smt_op} a b)", poison
    for opcode, smt_op in (("lshr", "bvlshr"), ("ashr", "bvashr")):
        poison = ("(or (bvuge b #x08) "
                  "(distinct (bvand a (bvsub (bvshl #x01 b) #x01)) #x00))")
        yield opcode, "exact", "", "b", f"({smt_op} a b)", poison
    for opcode, smt_op in (("udiv", "bvudiv"), ("sdiv", "bvsdiv")):
        # Keep the divisor in [2, 15] so this check isolates exactness.
        setup = "  %masked = and i8 %b, 15\n  %divisor = or i8 %masked, 2\n"
        divisor = "(bvor (bvand b #x0f) #x02)"
        poison = f"(distinct (bvmul value_expected {divisor}) a)"
        yield opcode, "exact", setup, "divisor", f"({smt_op} a {divisor})", poison


def generate_extra_ir(base_ir, path):
    module = [base_ir, "\ndefine internal void @consume8(i8 noundef %value) {\n"
                      "  ret void\n}\n"]
    specs = {}
    for opcode, flags, setup, right, value, poison in extra_cases():
        entry = f"i8_{opcode}_{flags.replace(' ', '_')}"
        specs[entry] = (8, f"""
(define-fun sign ((x (_ BitVec 8))) (_ BitVec 1) ((_ extract 7 7) x))
(define-fun value_expected () (_ BitVec 8) {value})
(define-fun poison_expected () Bool {poison})
""")
        plain = f"  %plain = {opcode} i8 %a, %{right}\n"
        if opcode in ("shl", "lshr", "ashr"):
            # A variable shift itself carries an overshift predicate. Use
            # valid constant shifts for the value that crosses the store.
            plain = ""
            previous = "%a"
            for amount in range(1, 8):
                selected = "%plain" if amount == 7 else f"%plain{amount}"
                plain += (
                    f"  %shift{amount} = {opcode} i8 %a, {amount}\n"
                    f"  %match{amount} = icmp eq i8 %b, {amount}\n"
                    f"  {selected} = select i1 %match{amount}, "
                    f"i8 %shift{amount}, i8 {previous}\n")
                previous = selected
        module.append(f"""
define void @{entry}() {{
  %x = alloca i8
  %z = alloca i8
  %y = alloca i8
  call void @__formal_register_input(ptr %x, i64 1, ptr @a)
  call void @__formal_register_input(ptr %z, i64 1, ptr @b)
  call void @__formal_register_output(ptr %y, i64 1, ptr @y)
  %a = load i8, ptr %x
  %b = load i8, ptr %z
{setup}  %result = {opcode} {flags} i8 %a, %{right}
  call void @consume8(i8 %result)
{plain}  %same = icmp eq i8 %result, %plain
  br i1 %same, label %done, label %mismatch
done:
  store i8 %plain, ptr %y
  ret void
mismatch:
  unreachable
}}
""")
    path.write_text("".join(module))
    return specs


def check(z3, path, source, expected):
    path.write_text(source)
    result = subprocess.run([z3, "-smt2", str(path)], capture_output=True,
                            text=True, timeout=60)
    assert result.returncode == 0 and result.stdout.strip() == expected, (
        path, expected, result.stdout, result.stderr)
    path.with_suffix(".out").write_text(result.stdout)


def check_entry(args, entry, ir, spec):
    directory = args.output_dir / entry
    process = subprocess.run(
        [args.formal_check, str(ir), "--entry", entry, "--allow-unversioned-ir",
         "--output-dir", str(directory)], capture_output=True, text=True,
        timeout=60)
    assert process.returncode == 1, (entry, process.stdout, process.stderr)
    report = json.loads((directory / "result.json").read_text())
    witness = json.loads((directory / "counterexample.json").read_text())
    assert report["status"] == "UNSAFE-UB", report
    assert report["relation_completeness"] == "COMPLETE", report
    assert report["path_counts"] == {
        "normal": 1, "error": 1, "incomplete": 0, "internal_error": 0
    }, report
    assert witness["reason"] == "LLVM-UB-POISON-CONSUMED", witness
    assert witness["source"]["opcode"] == "call", witness

    width, formulas = spec
    operands = "".join(
        f"(define-fun {name} () (_ BitVec {width}) "
        f"{word(f'input.{name}', width)})\n" for name in ("a", "b"))
    trigger = witness["trigger_condition_smt2"]
    check(args.z3, directory / "poison-equivalence.smt2",
          trigger + operands + formulas
          + "(assert (xor |ub_trigger| poison_expected))\n(check-sat)\n",
          "unsat")
    check(args.z3, directory / "ub-path.smt2",
          witness["path_constraints_smt2"], "sat")

    relation = (directory / "relation.smt2").read_text()
    suffix = "(assert |formal_relation|)\n(check-sat)\n"
    assert relation.endswith(suffix), relation
    expected = (f"(and (not poison_expected) "
                f"(= {word('output.y', width)} value_expected))")
    check(args.z3, directory / "relation-equivalence.smt2",
          relation.removesuffix(suffix) + operands + formulas
          + f"(assert (xor |formal_relation| {expected}))\n(check-sat)\n",
          "unsat")
    print(f"{entry}: Z3 proved poison and normal relation equivalence")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--ir", required=True)
    parser.add_argument("--z3", required=True)
    parser.add_argument("--output-dir", type=pathlib.Path, required=True)
    args = parser.parse_args()
    for entry in ("signed_add_exact_shift", "unsigned_sub"):
        check_entry(args, entry, args.ir, specification(entry))
    generated = args.output_dir / "generated-i8.ll"
    specs = generate_extra_ir(pathlib.Path(args.ir).read_text(), generated)
    for entry, spec in specs.items():
        check_entry(args, entry, generated, spec)


if __name__ == "__main__":
    main()
