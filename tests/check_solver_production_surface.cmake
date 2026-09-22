if(NOT DEFINED NM_EXECUTABLE OR NOT DEFINED LIBRARY OR NOT DEFINED EMITTER OR
   NOT DEFINED WORKER)
  message(FATAL_ERROR "production solver surface test is missing an argument")
endif()

foreach(binary IN ITEMS "${LIBRARY}" "${EMITTER}" "${WORKER}")
  execute_process(
    COMMAND "${NM_EXECUTABLE}" -C "${binary}"
    RESULT_VARIABLE nm_result
    OUTPUT_VARIABLE symbols
    ERROR_VARIABLE nm_error)
  if(NOT nm_result EQUAL 0)
    message(FATAL_ERROR "nm failed for ${binary}: ${nm_error}")
  endif()
  foreach(forbidden IN ITEMS
      "formal::setSTPSolverWorkerTestMode(int)"
      "formal::isPinnedSTPCompilationProfileForTest"
      "formal::(anonymous namespace)::WorkerTestMode")
    string(FIND "${symbols}" "${forbidden}" found_at)
    if(NOT found_at EQUAL -1)
      message(FATAL_ERROR
        "production binary ${binary} contains test symbol ${forbidden}")
    endif()
  endforeach()
  file(STRINGS "${binary}" injected_strings
    REGEX "injected|not-a-number|worker inherited an unrelated host descriptor")
  if(injected_strings)
    message(FATAL_ERROR
      "production binary ${binary} contains fault-injection code: ${injected_strings}")
  endif()
endforeach()
