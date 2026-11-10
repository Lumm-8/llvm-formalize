#!/usr/bin/env python3
"""Generate the native replay wrapper for a configured void () entry."""

import argparse
import pathlib
import re


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--entry", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", args.entry):
        parser.error("entry must be a C identifier")
    pathlib.Path(args.output).write_text(
        '#include "formal_replay_runtime.h"\n'
        f'extern "C" void {args.entry}(void);\n'
        'int main(int argc, char **argv) {\n'
        '  if (argc != 3) return 3;\n'
        f'  return formal_replay_run({args.entry}, argv[1], argv[2]);\n'
        '}\n')


if __name__ == "__main__":
    main()
