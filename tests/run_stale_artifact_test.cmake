foreach(required_variable OPT_EXECUTABLE PLUGIN_FILE INPUT_FILE OUTPUT_ROOT)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

file(REMOVE_RECURSE "${OUTPUT_ROOT}")
set(output_directory "${OUTPUT_ROOT}")
string(LENGTH "${output_directory}" output_length)
while(output_length LESS 4060)
  math(EXPR remaining "4060 - ${output_length} - 1")
  if(remaining GREATER 200)
    set(component_length 200)
  else()
    set(component_length ${remaining})
  endif()
  string(REPEAT x ${component_length} component)
  string(APPEND output_directory "/${component}")
  string(LENGTH "${output_directory}" output_length)
endwhile()

file(MAKE_DIRECTORY "${output_directory}")
set(legacy_result "${output_directory}/result.json")
file(WRITE "${legacy_result}"
  "{\"status\":\"VALIDATED\",\"verdict\":null}\n")

# The generated run directory and final result path fit below PATH_MAX, while
# the longer generated temporary name does not. The legacy fixed-path result is
# intentionally untouched because it is no longer authoritative for any run.
execute_process(
  COMMAND "${OPT_EXECUTABLE}"
    "-load-pass-plugin=${PLUGIN_FILE}"
    -formal-allow-unversioned-ir
    -passes=formal-exec
    -formal-entry=formal_top
    "-formal-output-dir=${output_directory}"
    "${INPUT_FILE}" -disable-output
  RESULT_VARIABLE writer_result
  ERROR_VARIABLE writer_stderr)
if(NOT writer_result EQUAL 4)
  message(FATAL_ERROR
    "forced artifact failure must exit 4, got ${writer_result}: ${writer_stderr}")
endif()
file(GLOB run_results LIST_DIRECTORIES FALSE
  "${output_directory}/runs/run-*/result.json")
if(run_results)
  file(READ "${run_results}" stale_result)
  message(FATAL_ERROR
    "failed run left a run-scoped result.json: ${stale_result}")
endif()
if(NOT EXISTS "${legacy_result}")
  message(FATAL_ERROR "run unexpectedly treated legacy root result as current")
endif()
