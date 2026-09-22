foreach(required_variable OPT_EXECUTABLE PLUGIN_FILE INPUT_FILE OUTPUT_DIR)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

file(REMOVE_RECURSE "${OUTPUT_DIR}")
execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=formal_top
    "-formal-output-dir=${OUTPUT_DIR}"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE opt_result
  OUTPUT_VARIABLE opt_stdout
  ERROR_VARIABLE opt_stderr)
if(opt_result EQUAL 0)
  message(FATAL_ERROR
    "incompatible host opt was accepted: ${opt_stdout}${opt_stderr}")
endif()

file(GLOB result_files LIST_DIRECTORIES FALSE
  "${OUTPUT_DIR}/runs/run-*/result.json")
if(EXPECT_HOST_MISMATCH)
  list(LENGTH result_files result_count)
  if(NOT result_count EQUAL 1)
    message(FATAL_ERROR
      "runtime host mismatch must produce one run-scoped result, got ${result_count}")
  endif()
endif()
foreach(result_file IN LISTS result_files)
  file(READ "${result_file}" result_json)
  string(FIND "${result_json}" "\"status\": \"VALIDATED\"" validated_position)
  if(NOT validated_position EQUAL -1)
    message(FATAL_ERROR
      "incompatible host opt produced VALIDATED: ${result_json}")
  endif()
  if(EXPECT_HOST_MISMATCH)
    foreach(expected
        "\"reason\": \"HOST_TOOL_MISMATCH\""
        "\"status\": \"INVALID_TASK\""
        "\"host_opt_sha256\": \"${EXPECT_ACTUAL_SHA256}\""
        "\"host_opt_identity\": \"mismatch\"")
      string(FIND "${result_json}" "${expected}" expected_position)
      if(expected_position EQUAL -1)
        message(FATAL_ERROR
          "runtime host mismatch result lacks ${expected}: ${result_json}")
      endif()
    endforeach()
  endif()
endforeach()
