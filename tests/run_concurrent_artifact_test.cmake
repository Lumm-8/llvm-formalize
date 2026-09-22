foreach(required_variable OPT_EXECUTABLE PLUGIN_FILE INPUT_FILE OUTPUT_ROOT)
  if(NOT DEFINED ${required_variable})
    message(FATAL_ERROR "${required_variable} is required")
  endif()
endforeach()

file(REMOVE_RECURSE "${OUTPUT_ROOT}")
set(command_file "${OUTPUT_ROOT}-concurrent.sh")
file(WRITE "${command_file}"
  "#!/bin/sh\n"
  "set -eu\n"
  "\"${OPT_EXECUTABLE}\" -load-pass-plugin=\"${PLUGIN_FILE}\" -formal-allow-unversioned-ir -passes=formal-exec -formal-entry=formal_top -formal-output-dir=\"${OUTPUT_ROOT}\" \"${INPUT_FILE}\" -disable-output >/dev/null 2>&1 &\n"
  "first_pid=$!\n"
  "\"${OPT_EXECUTABLE}\" -load-pass-plugin=\"${PLUGIN_FILE}\" -formal-allow-unversioned-ir -passes=formal-exec -formal-entry=formal_top -formal-output-dir=\"${OUTPUT_ROOT}\" \"${INPUT_FILE}\" -disable-output >/dev/null 2>&1 &\n"
  "second_pid=$!\n"
  "wait $first_pid\n"
  "wait $second_pid\n")
execute_process(COMMAND /bin/sh "${command_file}"
  RESULT_VARIABLE concurrent_result)
if(NOT concurrent_result EQUAL 0)
  message(FATAL_ERROR "concurrent FormalPass runs failed: ${concurrent_result}")
endif()

file(GLOB result_files LIST_DIRECTORIES FALSE
  "${OUTPUT_ROOT}/runs/run-*/result.json")
list(LENGTH result_files result_count)
if(NOT result_count EQUAL 2)
  message(FATAL_ERROR
    "concurrent runs must produce two isolated results, found ${result_count}: ${result_files}")
endif()

set(run_ids)
set(module_hashes)
foreach(result_file IN LISTS result_files)
  file(READ "${result_file}" result_json)
  string(JSON run_id GET "${result_json}" run_id)
  string(JSON module_hash GET "${result_json}" input_module_sha256)
  list(APPEND run_ids "${run_id}")
  list(APPEND module_hashes "${module_hash}")
endforeach()
list(REMOVE_DUPLICATES run_ids)
list(REMOVE_DUPLICATES module_hashes)
list(LENGTH run_ids run_id_count)
list(LENGTH module_hashes module_hash_count)
if(NOT run_id_count EQUAL 2 OR NOT module_hash_count EQUAL 1)
  message(FATAL_ERROR
    "run IDs must differ and module hashes must agree: IDs=${run_ids}; hashes=${module_hashes}")
endif()
