foreach(required_variable
    CLANG_EXECUTABLE OPT_EXECUTABLE PLUGIN_FILE SOURCE_FILE BITCODE_FILE
    OUTPUT_DIR)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

include("${CMAKE_CURRENT_LIST_DIR}/ArtifactTestHelpers.cmake")
file(REMOVE_RECURSE "${OUTPUT_DIR}")

execute_process(
  COMMAND "${CLANG_EXECUTABLE}"
    -O0 -g -fno-discard-value-names -emit-llvm -c
    "${SOURCE_FILE}" -o "${BITCODE_FILE}"
  RESULT_VARIABLE clang_result
  OUTPUT_VARIABLE clang_stdout
  ERROR_VARIABLE clang_stderr
)
if(NOT clang_result EQUAL 0)
  message(FATAL_ERROR
    "incompatible-Clang fixture compilation failed: ${clang_stdout}${clang_stderr}")
endif()

execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    "-formal-entry=formal_top"
    "-formal-output-dir=${OUTPUT_DIR}"
    "-passes=formal-exec"
    "${BITCODE_FILE}"
    -disable-output
  RESULT_VARIABLE opt_result
  OUTPUT_VARIABLE opt_stdout
  ERROR_VARIABLE opt_stderr
)
if(opt_result EQUAL 0)
  message(FATAL_ERROR "FormalPass accepted bitcode from an incompatible Clang")
endif()
string(FIND "${opt_stderr}" "INVALID_TASK [INCOMPATIBLE_PRODUCER]"
  diagnostic_position)
if(diagnostic_position EQUAL -1)
  message(FATAL_ERROR "missing incompatible-producer diagnostic: ${opt_stderr}")
endif()

formal_find_single_result("${OUTPUT_DIR}" result_file)
file(READ "${result_file}" result_json)
string(FIND "${result_json}" "\"reason\": \"INCOMPATIBLE_PRODUCER\""
  reason_position)
if(reason_position EQUAL -1)
  message(FATAL_ERROR "incorrect incompatible-producer result: ${result_json}")
endif()
