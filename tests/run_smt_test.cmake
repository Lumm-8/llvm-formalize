foreach(required_variable EXPR_TEST_EXECUTABLE STP_EXECUTABLE MODE OUTPUT_FILE)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

execute_process(
  COMMAND "${EXPR_TEST_EXECUTABLE}" --emit-smt "${MODE}" "${OUTPUT_FILE}"
  RESULT_VARIABLE writer_result
  ERROR_VARIABLE writer_stderr
)
if(NOT writer_result EQUAL 0)
  message(FATAL_ERROR "SMT writer failed: ${writer_stderr}")
endif()

if(DEFINED STP_LIBRARY_PATH AND NOT STP_LIBRARY_PATH STREQUAL "")
  execute_process(
    COMMAND "${CMAKE_COMMAND}" -E env
      "LD_LIBRARY_PATH=${STP_LIBRARY_PATH}"
      "${STP_EXECUTABLE}" "${OUTPUT_FILE}"
    RESULT_VARIABLE stp_result
    OUTPUT_VARIABLE stp_stdout
    ERROR_VARIABLE stp_stderr
  )
else()
  execute_process(
    COMMAND "${STP_EXECUTABLE}" "${OUTPUT_FILE}"
    RESULT_VARIABLE stp_result
    OUTPUT_VARIABLE stp_stdout
    ERROR_VARIABLE stp_stderr
  )
endif()
if(NOT stp_result EQUAL 0)
  message(FATAL_ERROR "STP rejected generated SMT-LIB: ${stp_stderr}")
endif()

if(MODE STREQUAL "syntax")
  set(expected "sat")
else()
  set(expected "unsat")
endif()
string(TOLOWER "${stp_stdout}${stp_stderr}" solver_output)
string(REGEX MATCH "(^|[\r\n])[ \t]*(sat|unsat)[ \t]*([\r\n]|$)"
  status_match "${solver_output}")
set(actual "${CMAKE_MATCH_2}")
if(NOT actual STREQUAL expected)
  message(FATAL_ERROR
    "expected ${expected} from STP, got '${actual}':\n"
    "${stp_stdout}${stp_stderr}")
endif()
