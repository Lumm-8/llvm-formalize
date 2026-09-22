foreach(required_variable OPT_EXECUTABLE PLUGIN_FILE INPUT_FILE OUTPUT_DIR)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

include("${CMAKE_CURRENT_LIST_DIR}/ArtifactTestHelpers.cmake")
file(REMOVE_RECURSE "${OUTPUT_DIR}")

execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    "-formal-entry=formal_top"
    "-formal-output-dir=${OUTPUT_DIR}"
    "-passes=formal-exec"
    "${INPUT_FILE}"
    -disable-output
  RESULT_VARIABLE opt_result
  OUTPUT_VARIABLE opt_stdout
  ERROR_VARIABLE opt_stderr
)

if(NOT opt_result EQUAL 0)
  message(FATAL_ERROR
    "FormalPass smoke test failed with exit code ${opt_result}\n"
    "stdout:\n${opt_stdout}\n"
    "stderr:\n${opt_stderr}")
endif()

set(smoke_stderr "${opt_stderr}")

set(expected_datalayout "e-m:e-p:64:64-i64:64-n8:16:32:64-S128")
foreach(expected
    "FormalPass: entry function 'formal_top'"
    "FormalPass: output directory '${OUTPUT_DIR}'"
    "FormalPass: DataLayout '${expected_datalayout}'")
  string(FIND "${opt_stderr}" "${expected}" match_position)
  if(match_position EQUAL -1)
    message(FATAL_ERROR
      "FormalPass smoke test did not print: ${expected}\n"
      "stderr:\n${opt_stderr}")
  endif()
endforeach()

formal_find_single_result("${OUTPUT_DIR}" result_file)
file(READ "${result_file}" result_json)
foreach(expected_json
    "\"phase\": \"M0_PREFLIGHT\""
    "\"status\": \"VALIDATED\""
    "\"run_id\": \"run-"
    "\"host_opt_sha256\": \""
    "\"input_module_sha256\": \""
    "\"verdict\": null")
  string(FIND "${result_json}" "${expected_json}" json_position)
  if(json_position EQUAL -1)
    message(FATAL_ERROR "result.json is missing ${expected_json}: ${result_json}")
  endif()
endforeach()

foreach(invalid_entry
    missing_entry
    not_void
    has_argument
    variadic
    declaration_only)
  execute_process(
    COMMAND "${OPT_EXECUTABLE}"
      "-load-pass-plugin=${PLUGIN_FILE}"
      -formal-allow-unversioned-ir
      "-formal-entry=${invalid_entry}"
      "-formal-output-dir=${OUTPUT_DIR}-${invalid_entry}"
      "-passes=formal-exec"
      "${INPUT_FILE}"
      -disable-output
    RESULT_VARIABLE invalid_result
    OUTPUT_QUIET
    ERROR_VARIABLE invalid_stderr
  )

  if(invalid_result EQUAL 0)
    message(FATAL_ERROR
      "FormalPass accepted invalid entry function '${invalid_entry}'")
  endif()

  if(invalid_entry STREQUAL "missing_entry")
    set(expected_error "entry function 'missing_entry' was not found")
  elseif(invalid_entry STREQUAL "declaration_only")
    set(expected_error "entry function 'declaration_only' has no definition")
  else()
    set(expected_error "entry function '${invalid_entry}' must have type void ()")
  endif()

  string(FIND "${invalid_stderr}" "${expected_error}" error_position)
  if(error_position EQUAL -1)
    message(FATAL_ERROR
      "FormalPass did not diagnose invalid entry '${invalid_entry}' as expected\n"
      "stderr:\n${invalid_stderr}")
  endif()
endforeach()

string(FIND "${smoke_stderr}" "FormalPass: status 'SAFE'" false_safe_position)
if(NOT false_safe_position EQUAL -1)
  message(FATAL_ERROR "M0 must not emit a SAFE verdict")
endif()
