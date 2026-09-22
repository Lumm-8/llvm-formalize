function(formal_enable_sanitizers target)
  if(NOT FORMAL_ENABLE_SANITIZERS)
    return()
  endif()
  if(NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    message(FATAL_ERROR "FORMAL_ENABLE_SANITIZERS requires Clang or GCC")
  endif()
  target_compile_options(${target} PRIVATE
    -fsanitize=address,undefined
    # The pinned LLVM build has RTTI disabled; vptr instrumentation would
    # require typeinfo symbols that LLVMSupport intentionally does not export.
    -fno-sanitize=vptr
    -fno-omit-frame-pointer
  )
  target_link_options(${target} PRIVATE
    -fsanitize=address,undefined
    -fno-sanitize=vptr
  )
endfunction()

function(formal_set_sanitizer_test_environment test_name)
  if(NOT FORMAL_ENABLE_SANITIZERS)
    return()
  endif()
  set_tests_properties(${test_name} PROPERTIES ENVIRONMENT
    "ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:verify_asan_link_order=0;UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1")
endfunction()
