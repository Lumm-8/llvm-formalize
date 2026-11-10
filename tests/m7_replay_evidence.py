"""Retain verified M7 native replay inputs and diagnostics in a build tree."""

import json
import shutil
from pathlib import Path


def save_replay_evidence(root, destination, ir_name, entry, reason,
                         asan_phrase, replay):
    if destination is None:
        return
    root = Path(root)
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    manifest = destination / "verified.json"
    manifest.unlink(missing_ok=True)
    shutil.copy2(root / ir_name, destination / "program.ll")
    shutil.copy2(root / "artifacts" / "result.json",
                 destination / "result.json")
    shutil.copy2(root / "artifacts" / "counterexample.json",
                 destination / "counterexample.json")
    shutil.copy2(root / "safe-input.json", destination / "safe-input.json")
    shutil.copy2(root / "safe-output.json", destination / "safe-output.json")
    (destination / "asan.stderr.txt").write_text(replay.stderr,
                                                  encoding="utf-8")
    manifest.write_text(json.dumps({
        "entry": entry,
        "reason": reason,
        "asan_phrase": asan_phrase,
        "native_exit_code": replay.returncode,
        "witness": "counterexample.json",
        "result": "result.json",
        "ir": "program.ll",
        "asan_log": "asan.stderr.txt",
        "verified": True,
    }, indent=2) + "\n", encoding="utf-8")
