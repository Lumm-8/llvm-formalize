foreach(required_variable EXPR_TEST_EXECUTABLE OUTPUT_ONE OUTPUT_TWO)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

foreach(output_file "${OUTPUT_ONE}" "${OUTPUT_TWO}")
  execute_process(
    COMMAND "${EXPR_TEST_EXECUTABLE}" --emit-smt syntax "${output_file}"
    RESULT_VARIABLE emitter_result
    ERROR_VARIABLE emitter_stderr)
  if(NOT emitter_result EQUAL 0)
    message(FATAL_ERROR "SMT emitter failed: ${emitter_stderr}")
  endif()
endforeach()

execute_process(
  COMMAND "${CMAKE_COMMAND}" -E compare_files "${OUTPUT_ONE}" "${OUTPUT_TWO}"
  RESULT_VARIABLE compare_result)
if(NOT compare_result EQUAL 0)
  message(FATAL_ERROR "SMT output differs across independent process runs")
endif()
