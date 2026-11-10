#!/usr/bin/env python3
"""Check loop bounds, incomplete reasons, and pass/driver agreement."""

import argparse
import json
import pathlib
import subprocess

from m5_test_temp import RetainedTemporaryDirectory


def run_case(args, root, ir, name, expected, reason_counts,
             loop_bound=2, max_states=None, policy="unknown",
             require_local=False):
    driver_dir = root / f"{name}-driver"
    driver_command = [
        args.formal_check, str(ir), "--entry", "formal_top",
        "--allow-unversioned-ir", "--loop-bound", str(loop_bound),
        "--loop-limit-policy", policy,
        "--output-dir", str(driver_dir),
    ]
    if max_states is not None:
        driver_command.extend(["--max-states", str(max_states)])
    if require_local:
        driver_command.append("--require-local-loop-bound")
    driver = subprocess.run(driver_command, capture_output=True, text=True)
    exit_code = {"SAFE": 0, "UNSAFE": 1, "UNKNOWN": 2,
                 "CONDITIONAL": 2, "INVALID_TASK": 3}[expected]
    assert driver.returncode == exit_code, (
        driver.returncode, driver.stderr, driver.stdout)
    driver_json = json.loads((driver_dir / "result.json").read_text())

    plugin_dir = root / f"{name}-plugin"
    plugin_command = [
        args.opt, "-load-pass-plugin", args.plugin,
        "-passes=formal-exec", "-formal-run-m5",
        "-formal-allow-unversioned-ir", "-formal-entry=formal_top",
        f"-formal-loop-bound={loop_bound}",
        f"-formal-loop-limit-policy={policy}",
        f"-formal-output-dir={plugin_dir}",
        "-disable-output", str(ir),
    ]
    if max_states is not None:
        plugin_command.insert(-2, f"-formal-max-states={max_states}")
    if require_local:
        plugin_command.insert(-2, "-formal-require-local-loop-bound")
    plugin = subprocess.run(plugin_command, capture_output=True, text=True)
    assert plugin.returncode == exit_code, (
        plugin.returncode, plugin.stderr, plugin.stdout)
    runs = list((plugin_dir / "runs").iterdir())
    assert len(runs) == 1, runs
    plugin_json = json.loads((runs[0] / "result.json").read_text())

    for result in (driver_json, plugin_json):
        assert result["status"] == expected, result
        assert result["incompleteness_reasons"] == reason_counts, result
        encoded_bound = loop_bound if loop_bound <= 2**63 - 1 else str(loop_bound)
        assert result["limits"]["default_loop_backedge_bound"] == encoded_bound, result
        assert result["loop_limit_policy"] == policy, result
        assert result["limits"]["require_local_loop_bound"] is require_local
        if expected == "INVALID_TASK":
            assert result["counterexample_file"] == "", result
        if reason_counts:
            assert result["relation_completeness"] == "UNDER_APPROXIMATION", result
    for key in ("status", "relation_completeness", "paths", "path_counts",
                "incompleteness_reasons", "limits", "loop_limit_policy",
                "unwinding_assumption"):
        assert driver_json[key] == plugin_json[key], key


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--formal-check", required=True)
    parser.add_argument("--opt", required=True)
    parser.add_argument("--plugin", required=True)
    parser.add_argument("--ir", required=True)
    parser.add_argument("--multiple-reasons-ir", required=True)
    parser.add_argument("--unsafe-incomplete-ir", required=True)
    args = parser.parse_args()
    with RetainedTemporaryDirectory(prefix="formal-m8-loop-") as temporary:
        root = pathlib.Path(temporary)
        annotated = pathlib.Path(args.ir)
        run_case(args, root, annotated, "annotated", "SAFE", {})
        run_case(args, root, annotated, "large-default", "SAFE", {},
                 loop_bound=2**64 - 1)
        unannotated = root / "unannotated.ll"
        body = annotated.read_text()
        assert "br label %header, !llvm.loop !0" in body
        unannotated.write_text(body.replace(
            "br label %header, !llvm.loop !0", "br label %header"))
        run_case(args, root, unannotated, "unannotated", "UNKNOWN",
                 {"UNKNOWN-LOOP-BOUND": 1})
        run_case(args, root, unannotated, "required-local", "UNKNOWN",
                 {"UNKNOWN-LOOP-METADATA-LOST": 1}, require_local=True)
        run_case(args, root, unannotated, "error-policy", "INVALID_TASK",
                 {"UNKNOWN-LOOP-BOUND": 1}, policy="error")
        run_case(args, root, unannotated, "assume-exit", "CONDITIONAL",
                 {"UNKNOWN-LOOP-BOUND": 1}, policy="assume-exit")
        run_case(args, root, pathlib.Path(args.multiple_reasons_ir),
                 "multiple-reasons", "UNKNOWN",
                 {"UNKNOWN-LOOP-BOUND": 1, "UNKNOWN-STATE-LIMIT": 1},
                 loop_bound=1, max_states=2)
        run_case(args, root, pathlib.Path(args.unsafe_incomplete_ir),
                 "unsafe-and-incomplete", "UNSAFE",
                 {"UNKNOWN-LOOP-BOUND": 1})
        run_case(args, root, pathlib.Path(args.unsafe_incomplete_ir),
                 "error-and-unsafe", "INVALID_TASK",
                 {"UNKNOWN-LOOP-BOUND": 1}, policy="error")


if __name__ == "__main__":
    main()
