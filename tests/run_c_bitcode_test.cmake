foreach(required_variable
    CLANG_EXECUTABLE
    OPT_EXECUTABLE
    PLUGIN_FILE
    SOURCE_FILE
    BITCODE_FILE
    OUTPUT_DIR)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

include("${CMAKE_CURRENT_LIST_DIR}/ArtifactTestHelpers.cmake")
file(REMOVE_RECURSE "${OUTPUT_DIR}")

execute_process(
  COMMAND "${CLANG_EXECUTABLE}"
    -O0
    -g
    -fno-discard-value-names
    -emit-llvm
    -c
    "${SOURCE_FILE}"
    -o "${BITCODE_FILE}"
  RESULT_VARIABLE clang_result
  OUTPUT_VARIABLE clang_stdout
  ERROR_VARIABLE clang_stderr
)

if(NOT clang_result EQUAL 0)
  message(FATAL_ERROR
    "C to bitcode compilation failed with exit code ${clang_result}\n"
    "stdout:\n${clang_stdout}\n"
    "stderr:\n${clang_stderr}")
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

if(NOT opt_result EQUAL 0)
  message(FATAL_ERROR
    "FormalPass rejected Clang-generated bitcode with exit code ${opt_result}\n"
    "stdout:\n${opt_stdout}\n"
    "stderr:\n${opt_stderr}")
endif()

foreach(expected
    "FormalPass: entry function 'formal_top'"
    "FormalPass: output directory '${OUTPUT_DIR}'"
    "FormalPass: DataLayout '")
  string(FIND "${opt_stderr}" "${expected}" match_position)
  if(match_position EQUAL -1)
    message(FATAL_ERROR
      "C bitcode test did not print: ${expected}\n"
      "stderr:\n${opt_stderr}")
  endif()
endforeach()

formal_find_single_result("${OUTPUT_DIR}" result_file)
file(READ "${result_file}" result_json)
string(FIND "${result_json}" "\"status\": \"VALIDATED\"" status_position)
if(status_position EQUAL -1)
  message(FATAL_ERROR "C bitcode result was not VALIDATED: ${result_json}")
endif()
foreach(expected "\"run_id\": \"run-" "\"host_opt_sha256\": \""
    "\"host_opt_sha256_basis\": \"runtime-sha256-of-running-executable-image\""
    "\"input_module_sha256\": \"")
  string(FIND "${result_json}" "${expected}" identity_position)
  if(identity_position EQUAL -1)
    message(FATAL_ERROR "C bitcode result lacks run identity ${expected}: ${result_json}")
  endif()
endforeach()

string(FIND "${opt_stderr}" "FormalPass: status 'SAFE'" false_safe_position)
if(NOT false_safe_position EQUAL -1)
  message(FATAL_ERROR "M0 must not emit a SAFE verdict")
endif()
