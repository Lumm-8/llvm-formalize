#!/usr/bin/env python3
"""
SMT2 fuzzing verifier: generates random concrete inputs, executes the IR
via JIT to get concrete outputs, then uses the STP solver to verify that
the SMT2 formula correctly predicts the output for each input vector.

For each input vector:
  - Constrain input variables to their concrete values
  - Negate the expected output: assert(not (= output expected))
  - Run STP:
      UNSAT → SMT2 formula forces the expected output (CORRECT)
      SAT   → SMT2 formula allows a different output than JIT (BUG!)

Usage:
  python3 fuzz_verify.py <test_base_name> [--rounds N] [--seed S]

The test must have been set up first:
  ./run_regression.sh --update        (generate SMT2)
  ./run_regression.sh --jit           (verify jit_runner compiled)
"""
import sys, os, re, subprocess, random, tempfile, argparse
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
TEST_DIR = SCRIPT_DIR / "simple"
BUILD_DIR = Path("/home/dengshy/github/llvm-formalize/build")
OPT_BIN = BUILD_DIR / "bin/opt"
LLC_BIN = BUILD_DIR / "bin/llc"
LLVM_LINK_BIN = BUILD_DIR / "bin/llvm-link"
CLANG_BIN = "clang-13"
STP_BIN = Path("/home/dengshy/github/stp/build/stp")
JIT_RUNNER = SCRIPT_DIR / "jit_runner.cpp"

STP_LIB_PATH = "/home/dengshy/github/stp/deps/install/lib:/home/dengshy/github/stp/build/lib:/home/dengshy/github/BuDDy/install/lib"
PIPELINE_UNROLL = "loop-simplify,lcssa,loop-unroll"

# Ensure all subprocess calls inherit the correct library path
BASE_ENV = {**os.environ, "LD_LIBRARY_PATH": STP_LIB_PATH}

# Range for random integer inputs
RAND_MIN = -100
RAND_MAX = 100

RED = '\033[0;31m'
GREEN = '\033[0;32m'
YELLOW = '\033[1;33m'
NC = '\033[0m'


def parse_variables(smt2_path: Path) -> dict:
    """Parse declare-fun from SMT2, return {name: bitwidth}."""
    vars = {}
    with open(smt2_path) as f:
        for line in f:
            m = re.match(r'\(declare-fun\s+(\w+)\s+\(\)\s+\(_\s*BitVec\s+(\d+)\)\)', line)
            if m:
                vars[m.group(1)] = int(m.group(2))
    return vars


def classify_vars(vars: dict, cpp_path: Path) -> tuple:
    """Separate variables into inputs and outputs by scanning the .cpp source."""
    with open(cpp_path) as f:
        source = f.read()
    inputs = set()
    outputs = set()
    for m in re.finditer(r'registerInput\("(\w+)"', source):
        inputs.add(m.group(1))
    for m in re.finditer(r'registerOutput\("(\w+)"', source):
        outputs.add(m.group(1))
    # Any declared variable that isn't an explicit output is an input
    declared = set(vars.keys())
    unknown = declared - inputs - outputs
    # Heuristic: remaining vars are usually inputs (e.g., 'b' in simple tests)
    inputs |= unknown
    return inputs, outputs


def build_jit(cpp_path: Path, base: str, workdir: Path) -> Path:
    """Compile test + force-unroll + link with jit_runner → native executable."""
    ll = workdir / f"{base}.ll"

    # 1. clang → .ll
    subprocess.run(
        [CLANG_BIN, "-S", "-emit-llvm", "-O0", "-I", str(SCRIPT_DIR),
         str(cpp_path), "-o", str(ll),
         "-Xclang", "-disable-O0-optnone"],
        capture_output=True, check=True)

    # 2. force-unroll
    unrolled = workdir / f"{base}_unrolled.ll"
    subprocess.run(
        [str(OPT_BIN), "-S", f"-passes={PIPELINE_UNROLL}",
         str(ll), "-o", str(unrolled)],
        capture_output=True, check=True, env=BASE_ENV)

    # 3. compile jit_runner
    jit_ll = workdir / "jit_runner.ll"
    subprocess.run(
        [CLANG_BIN, "-S", "-emit-llvm", "-O0", str(JIT_RUNNER), "-o", str(jit_ll),
         "-Xclang", "-disable-O0-optnone"],
        capture_output=True, check=True)

    # 4. link
    linked = workdir / "linked.ll"
    subprocess.run(
        [str(LLVM_LINK_BIN), "-S", str(unrolled), str(jit_ll), "-o", str(linked)],
        capture_output=True, check=True, env=BASE_ENV)

    # 5. llc
    obj = workdir / "linked.o"
    subprocess.run(
        [str(LLC_BIN), "-relocation-model=pic", "-filetype=obj",
         str(linked), "-o", str(obj)],
        capture_output=True, check=True, env=BASE_ENV)

    # 6. g++
    exe = workdir / "jit_test"
    subprocess.run(
        ["g++", "-no-pie", str(obj), "-o", str(exe)],
        capture_output=True, check=True)

    return exe


def run_jit(exe: Path, inputs: dict) -> dict:
    """Run the JIT executable with environment variables, return {output: value}."""
    env = dict(BASE_ENV)
    for k, v in inputs.items():
        env[k] = str(v)
    result = subprocess.run([str(exe)], env=env, capture_output=True, text=True)
    outputs = {}
    for line in result.stdout.strip().split('\n'):
        if '=' in line:
            name, val = line.split('=', 1)
            outputs[name.strip()] = int(val.strip())
    return outputs


def stp_verify(smt2_path: Path, inputs: dict, outputs: dict, workdir: Path, seq: int) -> bool:
    """Build an STP verification query and run the solver.

    Strategy: assert inputs, then assert NOT(expected output).
    - UNSAT → formula forces expected output (PASS)
    - SAT   → formula allows different output (FAIL)
    """
    query = workdir / f"verify_{seq}.smt2"

    with open(smt2_path) as f:
        original = f.read()

    # Build the query: original formula + input constraints + negated output
    lines = []
    for line in original.split('\n'):
        if '(check-sat)' in line:
            # Add input constraints
            for var, val in inputs.items():
                # Determine bitwidth from the original declares
                bw = 32  # default
                for ol in original.split('\n'):
                    m = re.match(rf'\(declare-fun\s+{var}\s+\(\)\s+\(_\s*BitVec\s+(\d+)\)\)', ol)
                    if m:
                        bw = int(m.group(1))
                        break
                # Format as hex for STP
                if val >= 0:
                    hex_val = f"#x{val:08X}"
                else:
                    # Two's complement
                    hex_val = f"#x{(val & ((1 << bw) - 1)):08X}"
                lines.append(f"(assert (= {var} {hex_val}))")

            # Add negated output constraints
            for var, expected in outputs.items():
                bw = 32
                for ol in original.split('\n'):
                    m = re.match(rf'\(declare-fun\s+{var}\s+\(\)\s+\(_\s*BitVec\s+(\d+)\)\)', ol)
                    if m:
                        bw = int(m.group(1))
                        break
                if expected >= 0:
                    hex_val = f"#x{expected:08X}"
                else:
                    hex_val = f"#x{(expected & ((1 << bw) - 1)):08X}"
                lines.append(f"(assert (not (= {var} {hex_val})))")

            lines.append("(check-sat)")
        else:
            lines.append(line)

    with open(query, 'w') as f:
        f.write('\n'.join(lines) + '\n')

    # Run STP
    result = subprocess.run(
        [str(STP_BIN), str(query)],
        env=BASE_ENV,
        capture_output=True, text=True)

    # STP prints "unsat" for UNSAT, "sat" for SAT.
    output = result.stdout + result.stderr
    is_unsat = 'unsat' in output.lower()
    return is_unsat, output.strip()


def fuzz_one(base: str, rounds: int, seed: int) -> tuple:
    """Fuzz a single test case. Returns (pass, fail, total)."""
    cpp_path = TEST_DIR / f"{base}.cpp"
    smt2_path = TEST_DIR / f"{base}_output.smt2"

    if not cpp_path.exists():
        print(f"{RED}  [SKIP] {base}: no .cpp found{NC}")
        return 0, 0, 0
    if not smt2_path.exists():
        print(f"{YELLOW}  [SKIP] {base}: no _output.smt2 (run --update first){NC}")
        return 0, 0, 0

    random.seed(seed + hash(base))

    vars = parse_variables(smt2_path)
    inputs_set, outputs_set = classify_vars(vars, cpp_path)

    if not outputs_set:
        print(f"{YELLOW}  [SKIP] {base}: no output variables found{NC}")
        return 0, 0, 0

    # Build JIT native binary once
    workdir = Path(tempfile.mkdtemp(prefix=f"fuzz_{base}_"))
    try:
        exe = build_jit(cpp_path, base, workdir)
    except subprocess.CalledProcessError as e:
        print(f"{RED}  [BUILD FAIL] {base}: {e}{NC}")
        return 0, 0, 0

    # Fuzz
    passed = 0
    failed = 0
    first_fail = None

    for i in range(rounds):
        # Generate random inputs
        inputs = {}
        for var in inputs_set:
            if var in vars:
                # Random value in [RAND_MIN, RAND_MAX]
                inputs[var] = random.randint(RAND_MIN, RAND_MAX)
            else:
                inputs[var] = 0

        # Execute via JIT
        try:
            outputs = run_jit(exe, inputs)
        except Exception as e:
            print(f"{RED}  [JIT CRASH] {base} round {i}: {e}{NC}")
            failed += 1
            continue

        # Verify with STP
        ok, msg = stp_verify(smt2_path, inputs, outputs, workdir, i)
        if ok:
            passed += 1
        else:
            failed += 1
            if first_fail is None:
                first_fail = (i, inputs, outputs, msg)

    # Cleanup
    import shutil
    shutil.rmtree(workdir, ignore_errors=True)

    return passed, failed, first_fail


def main():
    parser = argparse.ArgumentParser(description="SMT2 fuzz verification")
    parser.add_argument("tests", nargs="*", help="Test numbers (e.g., 1 2 8) or empty for all")
    parser.add_argument("--rounds", type=int, default=100, help="Number of random inputs per test")
    parser.add_argument("--seed", type=int, default=42, help="Random seed")
    args = parser.parse_args()

    # Collect tests
    if args.tests:
        bases = args.tests
    else:
        bases = sorted([
            p.stem for p in TEST_DIR.glob("*.cpp")
            if p.stem != "hello"
        ])

    total_pass = 0
    total_fail = 0

    print("=" * 60)
    print(f" SMT2 Fuzz Verification ({args.rounds} rounds, seed={args.seed})")
    print("=" * 60)
    print()

    for base in bases:
        print(f"--- {base}.cpp ---")
        sys.stdout.flush()

        p, f, first_fail = fuzz_one(base, args.rounds, args.seed)

        if p + f == 0:
            continue

        if f == 0:
            print(f"{GREEN}  [FUZZ PASS] {base}: {p}/{p+f} rounds verified{NC}")
        else:
            print(f"{RED}  [FUZZ FAIL] {base}: {f}/{p+f} rounds FAILED{NC}")
            if first_fail:
                i, inputs, outputs, msg = first_fail
                print(f"{RED}    First failure at round {i}:{NC}")
                print(f"{RED}      Inputs:  {inputs}{NC}")
                print(f"{RED}      Outputs: {outputs}{NC}")
                print(f"{RED}      STP output: {msg[:300]}{NC}")

        total_pass += p
        total_fail += f
        print()

    print("=" * 60)
    if total_fail == 0:
        print(f"{GREEN} ALL {total_pass} FUZZ ROUNDS PASSED{NC}")
    else:
        print(f"{RED} {total_fail}/{total_pass + total_fail} ROUNDS FAILED{NC}")
    print("=" * 60)

    return 1 if total_fail > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
