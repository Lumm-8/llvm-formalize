if(NOT DEFINED SOLVER_EMITTER OR NOT DEFINED STP_EXECUTABLE OR
   NOT DEFINED OUTPUT_DIR)
  message(FATAL_ERROR "solver export test is missing an argument")
endif()

function(run_stp_query query_file expected_status)
  if(DEFINED STP_LIBRARY_PATH AND NOT STP_LIBRARY_PATH STREQUAL "")
    execute_process(
      COMMAND "${CMAKE_COMMAND}" -E env
        "LD_LIBRARY_PATH=${STP_LIBRARY_PATH}" "${STP_EXECUTABLE}" "${query_file}"
      RESULT_VARIABLE stp_result
      OUTPUT_VARIABLE stp_output
      ERROR_VARIABLE stp_error
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  else()
    execute_process(
      COMMAND "${STP_EXECUTABLE}" "${query_file}"
      RESULT_VARIABLE stp_result
      OUTPUT_VARIABLE stp_output
      ERROR_VARIABLE stp_error
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
  if(NOT stp_result EQUAL 0)
    message(FATAL_ERROR
      "STP could not re-read ${query_file}: ${stp_error}")
  endif()
  string(TOLOWER "${stp_output}" stp_output_lower)
  string(REGEX MATCH "^(sat|unsat|unknown)" actual_status
    "${stp_output_lower}")
  if(NOT actual_status STREQUAL expected_status)
    message(FATAL_ERROR
      "unexpected STP result for ${query_file}: expected ${expected_status}, got ${stp_output}")
  endif()
endfunction()

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
  message(FATAL_ERROR "query emitter failed: ${emitter_error}")
endif()

string(REPLACE "\r\n" "\n" manifest "${manifest}")
string(REPLACE "\n" ";" manifest_lines "${manifest}")
set(query_count 0)
set(value_count 0)
foreach(line IN LISTS manifest_lines)
  if(line STREQUAL "")
    continue()
  endif()
  if(line MATCHES "^SAT\\|(.+)$")
    set(query_file "${CMAKE_MATCH_1}")
    if(NOT EXISTS "${query_file}")
      message(FATAL_ERROR "query emitter did not create ${query_file}")
    endif()
    run_stp_query("${query_file}" sat)
  elseif(line MATCHES "^ARRAY\\|(.+)$")
    set(query_file "${CMAKE_MATCH_1}")
    if(NOT EXISTS "${query_file}")
      message(FATAL_ERROR "query emitter did not create ${query_file}")
    endif()
    file(READ "${query_file}" query_text)
    foreach(required_text IN ITEMS
        "; array-request-0: name=export_requested_second index-width=16 byte-count=2"
        "; array-request-1: name=export_requested_first index-width=8 byte-count=1"
        "(declare-fun |export_requested_second| () (Array (_ BitVec 16) (_ BitVec 8)))"
        "(declare-fun |export_requested_first| () (Array (_ BitVec 8) (_ BitVec 8)))")
      string(FIND "${query_text}" "${required_text}" found_at)
      if(found_at EQUAL -1)
        message(FATAL_ERROR
          "array request artifact omitted ${required_text}: ${query_file}")
      endif()
    endforeach()
    run_stp_query("${query_file}" sat)
  elseif(line MATCHES "^VALUE\\|([0-9]+)\\|([0-9]+)\\|(.+)$")
    set(value_width "${CMAKE_MATCH_1}")
    set(value_decimal "${CMAKE_MATCH_2}")
    set(query_file "${CMAKE_MATCH_3}")
    if(NOT EXISTS "${query_file}")
      message(FATAL_ERROR "query emitter did not create ${query_file}")
    endif()
    file(READ "${query_file}" query_text)
    if(NOT query_text MATCHES "; query-target: value-only" OR
       NOT query_text MATCHES
         "\\(define-fun \\|__formal_query_target\\| \\(\\) \\(_ BitVec ${value_width}\\)")
      message(FATAL_ERROR "getValue artifact omitted its typed target term")
    endif()
    string(REGEX MATCHALL "\\(check-sat\\)" check_commands "${query_text}")
    list(LENGTH check_commands check_count)
    if(NOT check_count EQUAL 1)
      message(FATAL_ERROR "getValue artifact has ${check_count} check-sat commands")
    endif()
    set(expected_term "(_ bv${value_decimal} ${value_width})")
    string(REPLACE "(check-sat)"
      "(assert (= |__formal_query_target| ${expected_term}))\n(check-sat)"
      equal_query "${query_text}")
    string(REPLACE "(check-sat)"
      "(assert (not (= |__formal_query_target| ${expected_term})))\n(check-sat)"
      unequal_query "${query_text}")
    set(equal_file "${OUTPUT_DIR}/get-value-equal.smt2")
    set(unequal_file "${OUTPUT_DIR}/get-value-unequal.smt2")
    file(WRITE "${equal_file}" "${equal_query}")
    file(WRITE "${unequal_file}" "${unequal_query}")
    run_stp_query("${query_file}" sat)
    run_stp_query("${equal_file}" sat)
    run_stp_query("${unequal_file}" unsat)
    math(EXPR value_count "${value_count} + 1")
  else()
    message(FATAL_ERROR "malformed solver artifact manifest line: ${line}")
  endif()
  math(EXPR query_count "${query_count} + 1")
endforeach()

if(query_count LESS 3 OR value_count LESS 1)
  message(FATAL_ERROR
    "artifact suite emitted ${query_count} queries and ${value_count} value targets")
endif()
