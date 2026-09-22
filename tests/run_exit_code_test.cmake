foreach(required_variable OPT_EXECUTABLE PLUGIN_FILE INPUT_FILE OUTPUT_DIR)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

include("${CMAKE_CURRENT_LIST_DIR}/ArtifactTestHelpers.cmake")
file(REMOVE_RECURSE "${OUTPUT_DIR}" "${OUTPUT_DIR}-unknown"
  "${OUTPUT_DIR}-not-a-directory")

execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=missing_entry
    "-formal-output-dir=${OUTPUT_DIR}"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE invalid_result
  ERROR_VARIABLE invalid_stderr)
if(NOT invalid_result EQUAL 3)
  message(FATAL_ERROR
    "INVALID_TASK must exit 3, got ${invalid_result}: ${invalid_stderr}")
endif()
formal_find_single_result("${OUTPUT_DIR}" invalid_result_file)

execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=unsupported_atomic
    "-formal-output-dir=${OUTPUT_DIR}-unknown"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE unknown_result
  ERROR_VARIABLE unknown_stderr)
if(NOT unknown_result EQUAL 2)
  message(FATAL_ERROR
    "UNKNOWN must exit 2, got ${unknown_result}: ${unknown_stderr}")
endif()
formal_find_single_result("${OUTPUT_DIR}-unknown" unknown_result_file)
file(READ "${unknown_result_file}" unknown_json)
foreach(expected "\"status\": \"UNKNOWN\"" "\"verdict\": \"UNKNOWN\"")
  string(FIND "${unknown_json}" "${expected}" position)
  if(position EQUAL -1)
    message(FATAL_ERROR "UNKNOWN result is missing ${expected}: ${unknown_json}")
  endif()
endforeach()

execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=formal_top
    "-formal-output-dir=${OUTPUT_DIR}/../forbidden"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE configuration_result
  ERROR_VARIABLE configuration_stderr)
if(NOT configuration_result EQUAL 3)
  message(FATAL_ERROR
    "invalid output configuration must exit 3, got ${configuration_result}: ${configuration_stderr}")
endif()

file(WRITE "${OUTPUT_DIR}-not-a-directory" "not a directory")
execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=formal_top
    "-formal-output-dir=${OUTPUT_DIR}-not-a-directory"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE non_directory_result
  ERROR_VARIABLE non_directory_stderr)
if(NOT non_directory_result EQUAL 3)
  message(FATAL_ERROR
    "non-directory output must exit 3, got ${non_directory_result}: ${non_directory_stderr}")
endif()
