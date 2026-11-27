"""Tests for the dual-mode comparison policy."""

import copy
import unittest

from check_m10_optimizations import compare


def report(status, reason, block, max_wall_time_ms):
    incomplete = status == "INCOMPLETE"
    return {
        "verdict": "UNKNOWN" if incomplete else "UNSAFE-UB",
        "reason": reason,
        "incompleteness_reasons": {reason: 1} if incomplete else {},
        "limits": {"max_wall_time_ms": max_wall_time_ms},
        "path_counts": {"error": int(not incomplete),
                        "incomplete": int(incomplete), "internal_error": 0},
        "paths": [{"status": status, "reason": reason,
                   "source": {"function": "infinite_loop", "block": block,
                              "instruction_ordinal": 0, "opcode": "br"}}],
    }


class ComparisonTest(unittest.TestCase):
    def check(self, left, right):
        compare(None, (left, None, None), (right, None, None))

    def test_wall_clock_cutoff_can_move_to_another_instruction(self):
        self.check(report("INCOMPLETE", "UNKNOWN-WALL-TIME-LIMIT", "entry", 1),
                   report("INCOMPLETE", "UNKNOWN-WALL-TIME-LIMIT", "header", 1))

    def test_a_changed_error_location_is_rejected(self):
        with self.assertRaises(AssertionError):
            self.check(report("ERROR", "LLVM-UB-POISON", "entry", 1),
                       report("ERROR", "LLVM-UB-POISON", "header", 1))

    def test_a_changed_incompleteness_reason_is_rejected(self):
        left = report("INCOMPLETE", "UNKNOWN-WALL-TIME-LIMIT", "entry", 1)
        right = copy.deepcopy(left)
        right["paths"][0]["reason"] = "UNKNOWN-SOLVER-QUERY-LIMIT"
        with self.assertRaises(AssertionError):
            self.check(left, right)


if __name__ == "__main__":
    unittest.main()
