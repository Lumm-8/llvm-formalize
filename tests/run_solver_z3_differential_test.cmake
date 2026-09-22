if(NOT DEFINED SOLVER_EMITTER OR NOT DEFINED Z3_EXECUTABLE OR
   NOT DEFINED OUTPUT_DIR)
  message(FATAL_ERROR "solver/Z3 differential test is missing an argument")
endif()

file(REMOVE_RECURSE "${OUTPUT_DIR}")
if(DEFINED STP_LIBRARY_PATH AND NOT STP_LIBRARY_PATH STREQUAL "")
  execute_process(
    COMMAND "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${STP_LIBRARY_PATH}"
      "${SOLVER_EMITTER}" "${OUTPUT_DIR}"
    RESULT_VARIABLE emitter_result
    OUTPUT_VARIABLE manifest
    ERROR_VARIABLE emitter_error)
else()
  execute_process(
    COMMAND "${SOLVER_EMITTER}" "${OUTPUT_DIR}"
    RESULT_VARIABLE emitter_result
    OUTPUT_VARIABLE manifest
    ERROR_VARIABLE emitter_error)
endif()
if(NOT emitter_result EQUAL 0)
  message(FATAL_ERROR "differential emitter failed: ${emitter_error}")
endif()

string(REPLACE "\r\n" "\n" manifest "${manifest}")
string(REPLACE "\n" ";" manifest_lines "${manifest}")
set(query_count 0)
foreach(line IN LISTS manifest_lines)
  if(line STREQUAL "")
    continue()
  endif()
  if(NOT line MATCHES "^(SAT|UNSAT)\\|(.+)$")
    message(FATAL_ERROR "malformed differential manifest line: ${line}")
  endif()
  set(stp_status "${CMAKE_MATCH_1}")
  set(query_file "${CMAKE_MATCH_2}")
  if(NOT EXISTS "${query_file}")
    message(FATAL_ERROR "missing differential query: ${query_file}")
  endif()
  execute_process(
    COMMAND "${Z3_EXECUTABLE}" -smt2 "${query_file}"
    RESULT_VARIABLE z3_result
    OUTPUT_VARIABLE z3_output
    ERROR_VARIABLE z3_error
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT z3_result EQUAL 0)
    message(FATAL_ERROR "Z3 failed for ${query_file}: ${z3_error}")
  endif()
  string(TOUPPER "${z3_output}" z3_status)
  string(REGEX MATCH "^(SAT|UNSAT|UNKNOWN)" z3_status "${z3_status}")
  if(NOT z3_status STREQUAL stp_status)
    message(FATAL_ERROR
      "STP/Z3 disagreement for ${query_file}: STP=${stp_status}, Z3=${z3_status}")
  endif()
  math(EXPR query_count "${query_count} + 1")
endforeach()
if(query_count LESS 44)
  message(FATAL_ERROR "differential suite emitted only ${query_count} queries")
endif()
