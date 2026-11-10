#pragma once

#ifdef __cplusplus
extern "C" {
#endif

// Call this from each normal return path while registered output objects are
// still alive. This is required for stack outputs; a function-exit hook runs
// after local lifetimes end and cannot safely read their bytes.
void formal_replay_capture(void);

// Returns 0 for captured outputs, 2 for an excluded assume path, 3 for an
// invalid input file, and 4 for a runtime or capture failure.
int formal_replay_run(void (*entry)(void), const char *counterexample_json,
                      const char *output_json);

#ifdef __cplusplus
}
#endif
