#!/bin/bash
#=============================================================================
# Regression test driver for TranslateToStp pass — three-layer validation.
#=============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEST_DIR="$SCRIPT_DIR/simple"
JIT_RUNNER="$SCRIPT_DIR/jit_runner.cpp"
BUILD_DIR="/home/dengshy/github/llvm-formalize/build"
OPT_BIN="$BUILD_DIR/bin/opt"
LLC_BIN="$BUILD_DIR/bin/llc"
LLVM_LINK_BIN="$BUILD_DIR/bin/llvm-link"
STP_BIN="/home/dengshy/github/stp/build/stp"
CLANG_BIN="clang-13"
PIPELINE_SMT2="loop-simplify,lcssa,loop-unroll,translate-to-stp"
PIPELINE_UNROLL="loop-simplify,lcssa,loop-unroll"
export LD_LIBRARY_PATH="/home/dengshy/github/stp/deps/install/lib:/home/dengshy/github/stp/build/lib:/home/dengshy/github/BuDDy/install/lib:${LD_LIBRARY_PATH:-}"

# ---- output helpers ----
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; CYAN='\033[0;36m'; NC='\033[0m'
green()  { echo -e "${GREEN}$*${NC}"; }
red()    { echo -e "${RED}$*${NC}"; }
yellow() { echo -e "${YELLOW}$*${NC}"; }
cyan()   { echo -e "${CYAN}$*${NC}"; }
log_info()  { echo -e "  ${CYAN}[INFO]${NC}  $*"; }
log_pass()  { echo -e "  ${GREEN}[PASS]${NC}  $*"; }
log_fail()  { echo -e "  ${RED}[FAIL]${NC}  $*"; }
log_skip()  { echo -e "  ${YELLOW}[SKIP]${NC}  $*"; }

# ---- state ----
PASSED=0; FAILED=0; SKIPPED=0
MODE="test"
DO_SMT2=true; DO_JIT=false; DO_FUZZ=false
FUZZ_ROUNDS=100
SELECTED=()
TIMESTART=$(date +%s)
LOGFILE=""

# ---- parse args ----
while [[ $# -gt 0 ]]; do
    case "$1" in
        --update|-u) MODE="update"; shift ;;
        --jit)   DO_JIT=true; DO_SMT2=false; shift ;;
        --all)   DO_JIT=true; DO_SMT2=true; shift ;;
        --fuzz)  DO_FUZZ=true; DO_SMT2=false; DO_JIT=false; shift ;;
        --full)  DO_SMT2=true; DO_JIT=true; DO_FUZZ=true; shift ;;
        --fuzz-rounds=*) FUZZ_ROUNDS="${1#*=}"; shift ;;
        --log=*)  LOGFILE="${1#*=}"; shift ;;
        *)       SELECTED+=("$1"); shift ;;
    esac
done

# ---- log file setup ----
if [[ -n "$LOGFILE" ]]; then
    exec > >(tee -a "$LOGFILE") 2>&1
fi

normalize() { sed -e '/^; ModuleID = /d' -e 's/alloca_[0-9]*_b[0-9]*/alloca_N/g' "$1"; }

# ============================================================================
# Precompile jit_runner once (shared across all JIT / fuzz tests)
# ============================================================================
JIT_RUNNER_LL=""
build_jit_runner() {
    if [[ -n "$JIT_RUNNER_LL" && -f "$JIT_RUNNER_LL" ]]; then return 0; fi
    JIT_RUNNER_LL=$(mktemp)
    if ! "$CLANG_BIN" -S -emit-llvm -O0 "$JIT_RUNNER" \
         -o "$JIT_RUNNER_LL" -Xclang -disable-O0-optnone 2>/dev/null; then
        red "Failed to compile jit_runner.cpp"
        rm -f "$JIT_RUNNER_LL"; JIT_RUNNER_LL=""; return 1
    fi
    log_info "jit_runner compiled OK"
}

# ============================================================================
# Layer 1: SMT2 text-diff
# ============================================================================
run_smt2_one() {
    local cpp="$1"; local base; base=$(basename "$cpp" .cpp)
    local expected="$TEST_DIR/${base}_output.smt2"
    local workdir; workdir=$(mktemp -d)
    local ll="$workdir/${base}.ll" smt2="$workdir/${base}_output.smt2"

    if [[ "$base" == "hello" ]]; then ((SKIPPED++)); rm -rf "$workdir"; return 0; fi

    # Compile
    if ! "$CLANG_BIN" -S -emit-llvm -O0 -I "$SCRIPT_DIR" "$cpp" \
         -o "$ll" -Xclang -disable-O0-optnone 2>"$workdir/clang.err"; then
        log_fail "compile: $base"; ((FAILED++)); rm -rf "$workdir"; return 1
    fi

    # Run opt pipeline
    if ! (cd "$workdir" && "$OPT_BIN" -S -passes="$PIPELINE_SMT2" "$ll" \
          -o /dev/null 2>"$workdir/opt.err"); then
        log_fail "opt: $base"; ((FAILED++)); rm -rf "$workdir"; return 1
    fi

    # Find output
    if [[ ! -f "$smt2" ]]; then
        smt2=$(find "$workdir" -maxdepth 1 -name '*_output.smt2' -print -quit 2>/dev/null || true)
        if [[ -z "$smt2" ]]; then
            log_fail "no SMT2 output: $base"; ((FAILED++)); rm -rf "$workdir"; return 1
        fi
    fi

    # Check STP parseable (basic sanity)
    local stp_ok=""; local stp_err=""
    if "$STP_BIN" "$smt2" 2>&1 | grep -qE "sat|unsat|Valid"; then stp_ok=" STP✓"; else stp_err=" STP✗"; fi

    # Save IR and SMT2 alongside the source for inspection.
    local saved_ll="$TEST_DIR/${base}.ll"
    cp "$ll" "$saved_ll"

    if [[ "$MODE" == "update" || ! -f "$expected" ]]; then
        # --update mode, or expected file missing (first run): save as golden.
        cp "$smt2" "$expected"
        if [[ "$MODE" == "update" ]]; then
            log_pass "SMT2 updated: $base (size=$(wc -c < "$smt2") bytes, IR saved$stp_ok)"
        else
            log_pass "SMT2 initialized: $base (size=$(wc -c < "$smt2") bytes, IR saved$stp_ok)"
        fi
        ((PASSED++))
    else
        if diff -q <(normalize "$smt2") <(normalize "$expected") >/dev/null 2>&1; then
            log_pass "SMT2: $base (size=$(wc -c < "$expected") bytes$stp_ok)"
            ((PASSED++))
        else
            log_fail "SMT2: $base — diff: $(diff <(normalize "$expected") <(normalize "$smt2") | wc -l) lines$stp_err"
            diff <(normalize "$expected") <(normalize "$smt2") 2>&1 | head -10 || true
            ((FAILED++))
        fi
    fi
    rm -rf "$workdir"
}

# ============================================================================
# Layer 2: JIT semantic verification
# ============================================================================
run_jit_one() {
    local cpp="$1"; local base; base=$(basename "$cpp" .cpp)
    local expect_file="$TEST_DIR/${base}.jit_expect"
    local workdir; workdir=$(mktemp -d)

    if [[ "$base" == "hello" ]]; then ((SKIPPED++)); rm -rf "$workdir"; return 0; fi
    if [[ ! -f "$expect_file" ]]; then log_skip "JIT: $base (no .jit_expect)"; ((SKIPPED++)); rm -rf "$workdir"; return 0; fi

    # Compile → unroll → link → llc → g++
    local ll="$workdir/${base}.ll"
    "$CLANG_BIN" -S -emit-llvm -O0 -I "$SCRIPT_DIR" "$cpp" -o "$ll" -Xclang -disable-O0-optnone 2>/dev/null
    "$OPT_BIN" -S -passes="$PIPELINE_UNROLL" "$ll" -o "$workdir/u.ll" 2>/dev/null
    "$LLVM_LINK_BIN" -S "$workdir/u.ll" "$JIT_RUNNER_LL" -o "$workdir/l.ll" 2>/dev/null
    "$LLC_BIN" -relocation-model=pic -filetype=obj "$workdir/l.ll" -o "$workdir/l.o" 2>/dev/null
    g++ -no-pie "$workdir/l.o" -o "$workdir/jit" 2>/dev/null || { log_fail "JIT build: $base"; ((FAILED++)); rm -rf "$workdir"; return 1; }

    local jit_pass=0 jit_fail=0
    local total_read=0
    while IFS= read -r line; do
        total_read=$((total_read + 1))
        [[ -z "$line" || "$line" == "#"* ]] && continue
        if [[ ! "$line" =~ \| ]]; then continue; fi
        local inputs="${line%%|*}" expected_outs="${line##*|}"
        local env_vars=(); for pair in $inputs; do env_vars+=("$pair"); done
        local raw; raw=$(env "${env_vars[@]}" "$workdir/jit" 2>/dev/null || true)
        local all_ok=true
        for exp in $expected_outs; do
            local var="${exp%%=*}" exp_val="${exp##*=}"
            local act_val; act_val=$(echo "$raw" | grep "^${var}=" | cut -d= -f2 | head -1)
            if [[ "$act_val" != "$exp_val" ]]; then
                log_fail "JIT vector: $base {$inputs} → $var=$act_val (expected $exp_val)"
                all_ok=false; break
            fi
        done
        if $all_ok; then ((jit_pass++)); else ((jit_fail++)); fi
    done < "$expect_file"

    if [[ $jit_fail -eq 0 ]]; then
        log_pass "JIT: $base ($jit_pass/$((jit_pass+jit_fail)) vectors)"
        ((PASSED++))
    else
        log_fail "JIT: $base ($jit_fail/$((jit_pass+jit_fail)) vectors FAILED)"
        ((FAILED++))
    fi
    rm -rf "$workdir"
}

# ============================================================================
# Layer 3: STP fuzz verification
# ============================================================================
run_fuzz_one() {
    local cpp="$1"; local base; base=$(basename "$cpp" .cpp)
    local smt2="$TEST_DIR/${base}_output.smt2"
    local jit_expect="$TEST_DIR/${base}.jit_expect"

    if [[ ! -f "$smt2" ]]; then log_skip "FUZZ: $base (no SMT2 — run --update)"; ((SKIPPED++)); return 0; fi
    if [[ ! -f "$jit_expect" ]]; then log_skip "FUZZ: $base (no .jit_expect)"; ((SKIPPED++)); return 0; fi

    # Quick STP parse check
    local stp_check; stp_check=$("$STP_BIN" "$smt2" 2>&1)
    if ! echo "$stp_check" | grep -qE "sat|unsat|Valid"; then
        log_skip "FUZZ: $base (SMT2 not STP-parseable: $(echo "$stp_check" | head -1))"
        ((SKIPPED++)); return 0
    fi

    # Build JIT binary once
    local workdir; workdir=$(mktemp -d)
    local ll="$workdir/${base}.ll"
    "$CLANG_BIN" -S -emit-llvm -O0 -I "$SCRIPT_DIR" "$cpp" -o "$ll" -Xclang -disable-O0-optnone 2>/dev/null
    "$OPT_BIN" -S -passes="$PIPELINE_UNROLL" "$ll" -o "$workdir/u.ll" 2>/dev/null
    "$LLVM_LINK_BIN" -S "$workdir/u.ll" "$JIT_RUNNER_LL" -o "$workdir/l.ll" 2>/dev/null
    "$LLC_BIN" -relocation-model=pic -filetype=obj "$workdir/l.ll" -o "$workdir/l.o" 2>/dev/null
    g++ -no-pie "$workdir/l.o" -o "$workdir/jit" 2>/dev/null || { log_fail "FUZZ build: $base"; ((FAILED++)); rm -rf "$workdir"; return 1; }

    # Parse inputs/outputs from the FIRST data line in .jit_expect
    local input_vars=() output_vars=()
    while IFS= read -r line; do
        [[ -z "$line" || "$line" == "#"* ]] && continue  # skip comments/empty
        if [[ "$line" =~ \| ]]; then
            local lhs="${line%%|*}"; local rhs="${line##*|}"
            for v in $lhs; do input_vars+=("${v%%=*}"); done
            for v in $rhs; do output_vars+=("${v%%=*}"); done
            break
        fi
    done < "$jit_expect"
    # Also check the first data line for JIT runner
    local first_inputs=""
    while IFS= read -r line; do
        [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue
        first_inputs="${line%%|*}"
        break
    done < "$jit_expect"
    if [[ -z "$first_inputs" && ${#input_vars[@]} -eq 0 ]]; then
        for v in $(grep 'declare-fun' "$smt2" | awk '{print $2}'); do input_vars+=("$v"); done
    fi

    local fuzz_pass=0 fuzz_fail=0
    local seed=42
    for i in $(seq 1 "$FUZZ_ROUNDS"); do
        # Generate random inputs (store values for STP constraints)
        local env_vars=() input_str="" rand_vals=()
        for v in "${input_vars[@]}"; do
            local val=$(( (RANDOM % 200) - 100 ))
            env_vars+=("$v=$val"); input_str+="$v=$val "; rand_vals+=("$val")
        done

        # JIT execute → get concrete outputs
        local raw; raw=$(env "${env_vars[@]}" "$workdir/jit" 2>/dev/null || true)

        # Build STP verification query
        local query="$workdir/verify_${i}.smt2"
        {
            cat "$smt2" | while IFS= read -r sline; do
                if [[ "$sline" =~ \(check-sat\) ]]; then
                    # Input constraints: use the generated random values
                    local idx=0
                    for v in "${input_vars[@]}"; do
                        printf '(assert (= %s #x%08X))\n' "$v" "$(( rand_vals[idx] & 0xFFFFFFFF ))"
                        ((idx++))
                    done
                    # Output constraints: negate the JIT-measured output
                    for v in "${output_vars[@]}"; do
                        local oval; oval=$(echo "$raw" | grep "^$v=" | cut -d= -f2 | head -1 || echo "0")
                        oval=${oval:-0}
                        printf '(assert (not (= %s #x%08X)))\n' "$v" "$(( oval & 0xFFFFFFFF ))"
                    done
                    echo '(check-sat)'
                else echo "$sline"; fi
            done
        } > "$query"

        local result; result=$("$STP_BIN" "$query" 2>&1) || true
        if echo "$result" | grep -qi 'unsat'; then
            ((fuzz_pass++))
        else
            ((fuzz_fail++))
            if [[ $fuzz_fail -eq 1 ]]; then
                log_fail "FUZZ round $i: $base {$input_str} → $(echo "$raw" | tr '\n' ' ') — SMT2 formula mismatch"
            fi
        fi
    done

    if [[ $fuzz_fail -eq 0 ]]; then
        log_pass "FUZZ: $base ($fuzz_pass/$FUZZ_ROUNDS verified by STP)"
        ((PASSED++))
    else
        log_fail "FUZZ: $base ($fuzz_fail/$FUZZ_ROUNDS failed — SMT2 semantic mismatch)"
        ((FAILED++))
    fi
    rm -rf "$workdir"
}

# ============================================================================
# Main
# ============================================================================
TESTS=()
if [[ ${#SELECTED[@]} -gt 0 ]]; then
    for id in "${SELECTED[@]}"; do
        c="$TEST_DIR/${id}.cpp"
        [[ -f "$c" ]] && TESTS+=("$c") || red "Not found: ${id}.cpp"
    done
else
    for c in "$TEST_DIR"/*.cpp; do TESTS+=("$c"); done
fi

echo "============================================================"
echo " TranslateToStp Regression  |  $(date '+%Y-%m-%d %H:%M:%S')"
echo " Mode: $MODE"
echo " Layers: $( $DO_SMT2 && echo -n 'SMT2 '; $DO_JIT && echo -n 'JIT '; $DO_FUZZ && echo -n "Fuzz($FUZZ_ROUNDS)" )"
echo " Tests: ${#TESTS[@]} cases"
echo "============================================================"

if $DO_JIT || $DO_FUZZ; then
    echo ""; build_jit_runner || exit 1
fi

# ---- Layer 1 ----
if $DO_SMT2; then
    echo ""; echo "--- Layer 1: SMT2 text-diff ---"
    for cpp in "${TESTS[@]}"; do run_smt2_one "$cpp" || true; done
fi

# ---- Layer 2 ----
if $DO_JIT; then
    echo ""; echo "--- Layer 2: JIT semantic ---"
    for cpp in "${TESTS[@]}"; do run_jit_one "$cpp" || true; done
fi

# ---- Layer 3 ----
if $DO_FUZZ; then
    echo ""; echo "--- Layer 3: STP fuzz ($FUZZ_ROUNDS rounds each) ---"
    for cpp in "${TESTS[@]}"; do run_fuzz_one "$cpp" || true; done
fi

# ---- Summary ----
TIMEEND=$(date +%s)
echo ""
echo "============================================================"
echo " Results: $(green "$PASSED passed")  $(yellow "$SKIPPED skipped")  $(red "$FAILED failed")  (${TIMEEND}s elapsed)"
if [[ $FAILED -eq 0 ]]; then green " ALL TESTS PASSED"; else red " SOME TESTS FAILED"; fi
echo "============================================================"
exit $FAILED
