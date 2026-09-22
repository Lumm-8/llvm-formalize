function(formal_find_single_result output_root result_variable)
  file(GLOB result_files LIST_DIRECTORIES FALSE
    "${output_root}/runs/run-*/result.json")
  list(LENGTH result_files result_count)
  if(NOT result_count EQUAL 1)
    message(FATAL_ERROR
      "expected exactly one run-scoped result under ${output_root}, found ${result_count}: ${result_files}")
  endif()
  list(GET result_files 0 result_file)
  set(${result_variable} "${result_file}" PARENT_SCOPE)
endfunction()
