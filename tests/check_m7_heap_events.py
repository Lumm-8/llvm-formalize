#!/usr/bin/env python3
"""Check the identity chain exported for a successful realloc."""

import json
import pathlib
import subprocess
import sys

from m5_test_temp import RetainedTemporaryDirectory


def main(executor, ir):
    with RetainedTemporaryDirectory(prefix="formal-m7-heap-events-") as root:
        output = pathlib.Path(root) / "artifacts"
        run = subprocess.run(
            [executor, ir, "--m5", "--entry", "realloc_event_trace",
             "--output-dir", str(output)], capture_output=True, text=True)
        assert run.returncode == 2, run.stdout + run.stderr
        report = json.loads((output / "result.json").read_text())
        assert report["status"] == "UNKNOWN", report
        assert report["relation_completeness"] == "UNDER_APPROXIMATION"
        assert report["path_counts"]["incomplete"] == 2, report
        path = next(path for path in report["paths"] if
                    path["status"] == "COMPLETED")
        assert all(path["reason"] ==
                   "UNKNOWN-ALLOCATION-FAILURE-ASSUMED" for path in
                   report["paths"] if path["status"] == "INCOMPLETE")
        malloc_failure = next(path for path in report["paths"] if
                              path["status"] == "INCOMPLETE" and
                              any(event["kind"] ==
                                  "allocation_failure_unmodeled" and
                                  event["model"] == "malloc"
                                  for event in path["events"]))
        assert not any(event["kind"] == "allocation_success_assumption"
                       for event in malloc_failure["events"]), malloc_failure
        assert path["status"] == "COMPLETED", path
        assert path["call_stack"][0]["frame_id"] > 0, path
        events = path["events"]
        assert [event["model"] for event in events if
                event["kind"] == "external_model"] == [
                    "malloc", "realloc", "free"], events
        allocations = [event for event in events if
                       event["kind"] == "heap_allocation"]
        assert len(allocations) == 2, events
        old, new = allocations
        assert old["object_id"] > 0 and new["object_id"] > 0
        assert old["object_id"] != new["object_id"]
        assert [old["size"], new["size"]] == [8, 4]
        realloc = next(event for event in events if
                       event["kind"] == "realloc_success")
        assert realloc["previous_object_id"] == old["object_id"]
        assert realloc["object_id"] == new["object_id"]
        freed = next(event for event in events if event["kind"] == "free")
        assert freed["object_id"] == new["object_id"]
        assert all(event["frame_id"] == path["call_stack"][0]["frame_id"]
                   for event in events)
        assert realloc["source"]["function"] == "realloc_event_trace"
        for entry in ("malloc_failure_handled", "calloc_failure_handled",
                      "realloc_failure_handled"):
            for enabled in (False, True):
                destination = pathlib.Path(root) / (
                    entry + ("-failures" if enabled else "-default"))
                command = [executor, ir, "--m5", "--entry", entry,
                           "--output-dir", str(destination)]
                if enabled:
                    command.append("--allocation-failures")
                execution = subprocess.run(command, capture_output=True,
                                           text=True)
                expected_exit = 0 if enabled else 2
                assert execution.returncode == expected_exit, (
                    command, execution.stdout, execution.stderr)
                result = json.loads((destination / "result.json").read_text())
                assert result["status"] == ("SAFE" if enabled else "UNKNOWN")
                if enabled:
                    assert result["path_counts"]["incomplete"] == 0, result
                    assert result["path_counts"]["normal"] == 2, result
                else:
                    assert any(path["reason"] ==
                               "UNKNOWN-ALLOCATION-FAILURE-ASSUMED"
                               for path in result["paths"]), result
    print("M7 heap identity and allocation-failure verdicts verified")


if __name__ == "__main__":
    main(*sys.argv[1:])
