foreach(required_variable OPT_EXECUTABLE PLUGIN_FILE INPUT_FILE OUTPUT_ROOT)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

file(REMOVE_RECURSE "${OUTPUT_ROOT}")
file(MAKE_DIRECTORY "${OUTPUT_ROOT}")
# The root is a valid configured directory, but the filesystem object at the
# required runs/ path makes run creation fail after configuration validation.
file(WRITE "${OUTPUT_ROOT}/runs" "not a directory\n")
execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=formal_top
    "-formal-output-dir=${OUTPUT_ROOT}"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE opt_result
  OUTPUT_VARIABLE opt_stdout
  ERROR_VARIABLE opt_stderr)
if(NOT opt_result EQUAL 4)
  message(FATAL_ERROR
    "artifact I/O failure must exit 4, got ${opt_result}: ${opt_stdout}${opt_stderr}")
endif()
string(FIND "${opt_stderr}" "INTERNAL_ERROR cannot create isolated artifact run"
  internal_error_position)
if(internal_error_position EQUAL -1)
  message(FATAL_ERROR "artifact I/O failure was misclassified: ${opt_stderr}")
endif()
