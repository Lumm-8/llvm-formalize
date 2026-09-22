cmake_minimum_required(VERSION 3.20)

project(FormalPass LANGUAGES CXX)

find_package(LLVM REQUIRED CONFIG)

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

option(FORMAL_ENABLE_STP "Build the required M2 STP backend" ON)
option(FORMAL_ENABLE_Z3 "Build the optional Z3 backend" OFF)
option(FORMAL_REQUIRE_Z3_TEST_ORACLE
  "Fail configuration unless the independent Z3 differential oracle is available"
  OFF)
option(FORMAL_ENABLE_CUDD "Build the optional CUDD optimization" OFF)
option(FORMAL_ENABLE_KLEE_DIFF "Enable KLEE differential tests" OFF)
option(FORMAL_ENABLE_ALIVE2_TESTS "Enable Alive2 tests" OFF)
option(FORMAL_ENABLE_SANITIZERS "Build project tests with ASan and UBSan" OFF)
option(FORMAL_ENABLE_FUZZING "Build fuzz targets" OFF)
option(FORMAL_BUILD_TESTS "Build M0/M1/M2 tests" ON)
option(FORMAL_WARNINGS_AS_ERRORS
  "Treat warnings in Formal project sources as errors"
  OFF)
option(FORMAL_REQUIRE_PINNED_CLANG
  "Fail configuration unless the pinned Clang is available for C-to-bitcode tests"
  OFF)
option(FORMAL_REQUIRE_STP_TEST_ORACLE
  "Fail configuration unless the external STP SMT-LIB test oracle is usable"
  OFF)
set(FORMAL_STP_TEST_LIBRARY_PATH "" CACHE PATH
  "Optional runtime library directory for the external STP test oracle")

if(FORMAL_ENABLE_Z3 OR FORMAL_ENABLE_CUDD)
  message(FATAL_ERROR "Z3 and CUDD remain outside the M2 build")
endif()
if(NOT FORMAL_ENABLE_STP)
  message(FATAL_ERROR
    "FORMAL_ENABLE_STP=OFF would omit the required M2 solver and its tests")
endif()

include(cmake/Dependencies.cmake)
include(cmake/FormalSanitizers.cmake)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

function(formal_enable_project_warnings target)
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(${target} PRIVATE -Wall -Wextra -Wpedantic)
    if(FORMAL_WARNINGS_AS_ERRORS)
      target_compile_options(${target} PRIVATE -Werror)
    endif()
  endif()
endfunction()

function(formal_set_stp_test_environment test_name)
  if(FORMAL_STP_TEST_LIBRARY_PATH)
    set_property(TEST ${test_name} APPEND PROPERTY ENVIRONMENT_MODIFICATION
      "LD_LIBRARY_PATH=path_list_prepend:${FORMAL_STP_TEST_LIBRARY_PATH}")
  endif()
endfunction()

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_llvm_pass_plugin(FormalPass
  lib/FileDigest.cpp
  lib/FormalEntry.cpp
  lib/FormalPass.cpp
  lib/PreflightResult.cpp
)
target_include_directories(FormalPass PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_compile_definitions(FormalPass PRIVATE
  FORMAL_PINNED_LLVM_VERSION="${FORMAL_LLVM_VERSION}"
  FORMAL_PINNED_LLVM_REVISION="${FORMAL_LLVM_REVISION}"
  FORMAL_ACTUAL_LLVM_REVISION="${FORMAL_ACTUAL_LLVM_REVISION}"
  FORMAL_PINNED_OPT_EXECUTABLE="${FORMAL_PINNED_OPT_EXECUTABLE}"
  FORMAL_PINNED_OPT_SHA256="${FORMAL_PINNED_OPT_SHA256}"
  FORMAL_PINNED_OPT_SIZE=${FORMAL_PINNED_OPT_SIZE}
)
target_link_libraries(FormalPass PRIVATE FormalLLVMSupport)
formal_enable_sanitizers(FormalPass)
formal_enable_project_warnings(FormalPass)

add_library(FormalExpr STATIC
  lib/Expr.cpp
  lib/SMTLibWriter.cpp
)
target_include_directories(FormalExpr PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_compile_features(FormalExpr PUBLIC cxx_std_20)
target_link_libraries(FormalExpr PUBLIC FormalLLVMSupport)
formal_enable_sanitizers(FormalExpr)
formal_enable_project_warnings(FormalExpr)

if(FORMAL_ENABLE_STP)
  function(formal_add_solver_library target_name worker_name)
    add_library(${target_name} STATIC
      lib/Solver.cpp
      lib/STPSolver.cpp
    )
    target_include_directories(${target_name} PUBLIC
      ${CMAKE_CURRENT_SOURCE_DIR}/include
    )
    target_compile_features(${target_name} PUBLIC cxx_std_20)
    target_compile_definitions(${target_name} PRIVATE
      FORMAL_STP_VERSION="${FORMAL_STP_VERSION}"
      FORMAL_STP_REVISION="${FORMAL_STP_REVISION}"
      FORMAL_STP_COMPILER="${FORMAL_STP_TEST_COMPILER}"
      FORMAL_STP_WORKER_EXECUTABLE="${CMAKE_CURRENT_BINARY_DIR}/${worker_name}"
    )
    target_link_libraries(${target_name} PUBLIC FormalExpr PRIVATE stp)
    formal_enable_sanitizers(${target_name})
    formal_enable_project_warnings(${target_name})
  endfunction()

  formal_add_solver_library(FormalSolver FormalSTPWorker)

  add_executable(FormalSTPWorker tools/STPWorkerMain.cpp)
  target_link_libraries(FormalSTPWorker PRIVATE FormalSolver)
  formal_enable_sanitizers(FormalSTPWorker)
  formal_enable_project_warnings(FormalSTPWorker)
endif()

if(FORMAL_BUILD_TESTS)
  include(CTest)
  enable_testing()

  # Test-only plugin variant that proves the in-process host-image digest gate
  # is reached after successful plugin loading. It is never installed.
  add_library(FormalPassWrongHostDigest MODULE
    lib/FileDigest.cpp
    lib/FormalEntry.cpp
    lib/FormalPass.cpp
    lib/PreflightResult.cpp
  )
  set_target_properties(FormalPassWrongHostDigest PROPERTIES PREFIX "")
  target_include_directories(FormalPassWrongHostDigest PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include)
  target_compile_definitions(FormalPassWrongHostDigest PRIVATE
    FORMAL_PINNED_LLVM_VERSION="${FORMAL_LLVM_VERSION}"
    FORMAL_PINNED_LLVM_REVISION="${FORMAL_LLVM_REVISION}"
    FORMAL_ACTUAL_LLVM_REVISION="${FORMAL_ACTUAL_LLVM_REVISION}"
    FORMAL_PINNED_OPT_EXECUTABLE="${FORMAL_PINNED_OPT_EXECUTABLE}"
    FORMAL_PINNED_OPT_SHA256="0000000000000000000000000000000000000000000000000000000000000000"
    FORMAL_PINNED_OPT_SIZE=${FORMAL_PINNED_OPT_SIZE})
  target_link_libraries(FormalPassWrongHostDigest PRIVATE FormalLLVMSupport)
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(FormalPassWrongHostDigest PRIVATE -fno-rtti)
  endif()
  formal_enable_sanitizers(FormalPassWrongHostDigest)
  formal_enable_project_warnings(FormalPassWrongHostDigest)
  if(DEFINED FORMAL_CLANG_EXECUTABLE AND
     FORMAL_CLANG_EXECUTABLE STREQUAL "")
    unset(FORMAL_CLANG_EXECUTABLE CACHE)
  endif()
  find_program(FORMAL_CLANG_EXECUTABLE
    NAMES clang-${LLVM_VERSION_MAJOR} clang
    HINTS ${LLVM_TOOLS_BINARY_DIR}
  )
  if(FORMAL_CLANG_EXECUTABLE)
    execute_process(COMMAND ${FORMAL_CLANG_EXECUTABLE} --version
      RESULT_VARIABLE FORMAL_CLANG_VERSION_RESULT
      OUTPUT_VARIABLE FORMAL_CLANG_VERSION_TEXT
      ERROR_VARIABLE FORMAL_CLANG_VERSION_TEXT)
    string(REGEX MATCH "clang version ([^ \r\n]+)" FORMAL_CLANG_VERSION_MATCH
      "${FORMAL_CLANG_VERSION_TEXT}")
    if(NOT FORMAL_CLANG_VERSION_RESULT EQUAL 0 OR
       NOT CMAKE_MATCH_1 STREQUAL "${FORMAL_LLVM_VERSION}" OR
       NOT FORMAL_CLANG_VERSION_TEXT MATCHES "${FORMAL_LLVM_REVISION}")
      message(WARNING
        "Ignoring incompatible cached Clang executable: ${FORMAL_CLANG_EXECUTABLE}")
      set(FORMAL_CLANG_EXECUTABLE "" CACHE FILEPATH
        "Pinned Clang executable for end-to-end tests" FORCE)
    endif()
  endif()
  if(FORMAL_REQUIRE_PINNED_CLANG AND NOT FORMAL_CLANG_EXECUTABLE)
    message(FATAL_ERROR
      "FORMAL_REQUIRE_PINNED_CLANG requires Clang ${FORMAL_LLVM_VERSION}")
  endif()
  if(DEFINED FORMAL_LLVM_LIT_EXECUTABLE AND NOT
     FORMAL_LLVM_LIT_EXECUTABLE MATCHES "^${LLVM_TOOLS_BINARY_DIR}/")
    unset(FORMAL_LLVM_LIT_EXECUTABLE CACHE)
  endif()
  find_program(FORMAL_LLVM_LIT_EXECUTABLE NAMES llvm-lit
    HINTS ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH
    REQUIRED)
  if(DEFINED FORMAL_FILECHECK_EXECUTABLE AND NOT
     FORMAL_FILECHECK_EXECUTABLE MATCHES "^${LLVM_TOOLS_BINARY_DIR}/")
    unset(FORMAL_FILECHECK_EXECUTABLE CACHE)
  endif()
  find_program(FORMAL_FILECHECK_EXECUTABLE NAMES FileCheck
    HINTS ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH
    REQUIRED)
  if(DEFINED FORMAL_NOT_EXECUTABLE AND NOT
     FORMAL_NOT_EXECUTABLE MATCHES "^${LLVM_TOOLS_BINARY_DIR}/")
    unset(FORMAL_NOT_EXECUTABLE CACHE)
  endif()
  find_program(FORMAL_NOT_EXECUTABLE NAMES not
    HINTS ${LLVM_TOOLS_BINARY_DIR}
    NO_DEFAULT_PATH
    REQUIRED)

  function(add_capability_gate test_name milestone reason labels)
    add_test(
      NAME ${test_name}
      COMMAND ${CMAKE_COMMAND} -E echo
        "SKIP: ${milestone} not implemented; ${reason}"
    )
    set_tests_properties(${test_name} PROPERTIES
      LABELS "${labels};future"
      SKIP_REGULAR_EXPRESSION "^SKIP:"
    )
  endfunction()

  add_library(FormalGTest STATIC
    third-party/unittest/googletest/src/gtest-all.cc
  )
  target_include_directories(FormalGTest PUBLIC
    third-party/unittest/googletest/include
    PRIVATE third-party/unittest/googletest
  )
  target_compile_definitions(FormalGTest PUBLIC GTEST_HAS_RTTI=0)
  target_link_libraries(FormalGTest PUBLIC FormalLLVMSupport)
  formal_enable_sanitizers(FormalGTest)

  add_executable(FormalPassUnitTests
    lib/FileDigest.cpp
    lib/FormalEntry.cpp
    tests/FormalPassUnitTest.cpp
    tests/TestMain.cpp
  )
  target_include_directories(FormalPassUnitTests PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
  )
  target_link_libraries(FormalPassUnitTests PRIVATE
    FormalGTest
    LLVMCore
    LLVMAnalysis
    LLVMTargetParser
  )
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(FormalPassUnitTests PRIVATE -fno-rtti)
  endif()
  formal_enable_sanitizers(FormalPassUnitTests)
  formal_enable_project_warnings(FormalPassUnitTests)

  add_test(
    NAME FormalPass.Unit.EntryValidation
    COMMAND FormalPassUnitTests
  )
  set_tests_properties(FormalPass.Unit.EntryValidation PROPERTIES
    LABELS "unit;m0"
  )
  formal_set_sanitizer_test_environment(FormalPass.Unit.EntryValidation)

  add_executable(FormalExprUnitTests
    tests/ExprUnitTest.cpp
    tests/TestMain.cpp
  )
  target_link_libraries(FormalExprUnitTests PRIVATE FormalExpr FormalGTest)
  formal_enable_sanitizers(FormalExprUnitTests)
  formal_enable_project_warnings(FormalExprUnitTests)

  add_test(NAME FormalExpr.Unit COMMAND FormalExprUnitTests)
  set_tests_properties(FormalExpr.Unit PROPERTIES LABELS "unit;m1;expr")
  formal_set_sanitizer_test_environment(FormalExpr.Unit)

  if(FORMAL_ENABLE_STP)
    formal_add_solver_library(FormalSolverTest FormalSTPTestWorker)
    target_compile_definitions(FormalSolverTest PRIVATE
      FORMAL_SOLVER_TEST_ACCESS=1
      FORMAL_SOLVER_ENABLE_TEST_FAULTS=1)
    add_executable(FormalSTPTestWorker tools/STPWorkerMain.cpp)
    target_link_libraries(FormalSTPTestWorker PRIVATE FormalSolverTest)
    formal_enable_sanitizers(FormalSTPTestWorker)
    formal_enable_project_warnings(FormalSTPTestWorker)

    add_executable(FormalSolverUnitTests
      tests/SolverUnitTest.cpp
      tests/TestMain.cpp
    )
    target_link_libraries(FormalSolverUnitTests PRIVATE
      FormalSolverTest FormalGTest)
    target_compile_definitions(FormalSolverUnitTests PRIVATE
      FORMAL_SOLVER_TEST_ACCESS=1)
    add_dependencies(FormalSolverUnitTests FormalSTPTestWorker)
    formal_enable_sanitizers(FormalSolverUnitTests)
    formal_enable_project_warnings(FormalSolverUnitTests)

    add_test(NAME FormalSolver.Unit COMMAND FormalSolverUnitTests)
    set_tests_properties(FormalSolver.Unit PROPERTIES
      LABELS "unit;m2;solver;stp")
    formal_set_sanitizer_test_environment(FormalSolver.Unit)
    formal_set_stp_test_environment(FormalSolver.Unit)

    add_executable(FormalSolverQueryEmitter tests/SolverQueryEmitter.cpp)
    target_link_libraries(FormalSolverQueryEmitter PRIVATE FormalSolver)
    add_dependencies(FormalSolverQueryEmitter FormalSTPWorker)
    formal_enable_sanitizers(FormalSolverQueryEmitter)
    formal_enable_project_warnings(FormalSolverQueryEmitter)

    add_executable(FormalSolverDifferentialEmitter
      tests/SolverDifferentialEmitter.cpp)
    target_link_libraries(FormalSolverDifferentialEmitter PRIVATE FormalSolver)
    add_dependencies(FormalSolverDifferentialEmitter FormalSTPWorker)
    formal_enable_sanitizers(FormalSolverDifferentialEmitter)
    formal_enable_project_warnings(FormalSolverDifferentialEmitter)

    add_test(NAME FormalSolver.ProductionSurface
      COMMAND ${CMAKE_COMMAND}
        -DNM_EXECUTABLE=${CMAKE_NM}
        -DLIBRARY=$<TARGET_FILE:FormalSolver>
        -DEMITTER=$<TARGET_FILE:FormalSolverQueryEmitter>
        -DWORKER=$<TARGET_FILE:FormalSTPWorker>
        -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_solver_production_surface.cmake)
    set_tests_properties(FormalSolver.ProductionSurface PROPERTIES
      LABELS "m2;solver;stp;negative")
  endif()

  add_executable(FormalExprSMTEmitter tests/ExprSMTEmitter.cpp)
  target_link_libraries(FormalExprSMTEmitter PRIVATE FormalExpr)
  formal_enable_sanitizers(FormalExprSMTEmitter)
  formal_enable_project_warnings(FormalExprSMTEmitter)

  find_program(FORMAL_STP_TEST_EXECUTABLE NAMES stp)
  if(FORMAL_STP_TEST_EXECUTABLE)
    if(FORMAL_STP_TEST_LIBRARY_PATH)
      execute_process(COMMAND ${CMAKE_COMMAND} -E env
          "LD_LIBRARY_PATH=${FORMAL_STP_TEST_LIBRARY_PATH}"
          ${FORMAL_STP_TEST_EXECUTABLE} --version
        RESULT_VARIABLE FORMAL_STP_TEST_RESULT
        OUTPUT_VARIABLE FORMAL_STP_TEST_VERSION_TEXT
        ERROR_VARIABLE FORMAL_STP_TEST_VERSION_TEXT)
    else()
      execute_process(COMMAND ${FORMAL_STP_TEST_EXECUTABLE} --version
        RESULT_VARIABLE FORMAL_STP_TEST_RESULT
        OUTPUT_VARIABLE FORMAL_STP_TEST_VERSION_TEXT
        ERROR_VARIABLE FORMAL_STP_TEST_VERSION_TEXT)
    endif()
    if(FORMAL_STP_TEST_RESULT EQUAL 0)
      set(FORMAL_STP_PROFILE_MISMATCH "")
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES
         "STP version ${FORMAL_STP_TEST_VERSION}([\r\n]|$)")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " version")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES "${FORMAL_STP_TEST_REVISION}")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " revision")
      endif()
      string(FIND "${FORMAL_STP_TEST_VERSION_TEXT}"
        "CMAKE_CXX_COMPILER = ${FORMAL_STP_TEST_COMPILER}"
        FORMAL_STP_COMPILER_POSITION)
      if(FORMAL_STP_COMPILER_POSITION EQUAL -1)
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " compiler")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES "ONLY_SIMPLE = OFF")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " ONLY_SIMPLE")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES "STATICCOMPILE = OFF")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " shared-libraries")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES "FORCE_CMS = OFF")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " FORCE_CMS")
      endif()
      if(FORMAL_STP_TEST_VERSION_TEXT MATCHES "-DNDEBUG")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " assertions")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES "ENABLE_TESTING = ON")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " ENABLE_TESTING")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES
         "ENABLE_PYTHON_INTERFACE = OFF")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " Python")
      endif()
      if(FORMAL_STP_TEST_VERSION_TEXT MATCHES "USE_CRYPTOMINISAT")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " CryptoMiniSat")
      endif()
      if(NOT FORMAL_STP_TEST_VERSION_TEXT MATCHES "libminisat")
        string(APPEND FORMAL_STP_PROFILE_MISMATCH " MiniSat")
      endif()
      if(FORMAL_STP_PROFILE_MISMATCH)
        set(FORMAL_STP_TEST_RESULT 1)
        message(WARNING
          "Ignoring external STP oracle with lock-profile mismatch:${FORMAL_STP_PROFILE_MISMATCH}")
      endif()
    endif()
  endif()
  if(FORMAL_STP_TEST_EXECUTABLE AND FORMAL_STP_TEST_RESULT EQUAL 0)
    foreach(smt_mode syntax semantics arrays property simplification)
      add_test(NAME FormalExpr.SMT.${smt_mode}
        COMMAND ${CMAKE_COMMAND}
          -DEXPR_TEST_EXECUTABLE=$<TARGET_FILE:FormalExprSMTEmitter>
          -DSTP_EXECUTABLE=${FORMAL_STP_TEST_EXECUTABLE}
          -DSTP_LIBRARY_PATH=${FORMAL_STP_TEST_LIBRARY_PATH}
          -DMODE=${smt_mode}
          -DOUTPUT_FILE=${CMAKE_CURRENT_BINARY_DIR}/formal-${smt_mode}.smt2
          -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_smt_test.cmake)
      set_tests_properties(FormalExpr.SMT.${smt_mode} PROPERTIES
        LABELS "m1;smt;test-oracle")
      formal_set_sanitizer_test_environment(FormalExpr.SMT.${smt_mode})
    endforeach()
    if(FORMAL_ENABLE_STP)
      add_test(NAME FormalSolver.SMTLib.ReparseFailureQuery
        COMMAND ${CMAKE_COMMAND}
          -DSOLVER_EMITTER=$<TARGET_FILE:FormalSolverQueryEmitter>
          -DSTP_EXECUTABLE=${FORMAL_STP_TEST_EXECUTABLE}
          -DSTP_LIBRARY_PATH=${FORMAL_STP_TEST_LIBRARY_PATH}
          -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/m2-export-reparse
          -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_solver_export_test.cmake)
      set_tests_properties(FormalSolver.SMTLib.ReparseFailureQuery PROPERTIES
        LABELS "m2;stp;smt;diagnostic")
      formal_set_sanitizer_test_environment(
        FormalSolver.SMTLib.ReparseFailureQuery)
      formal_set_stp_test_environment(
        FormalSolver.SMTLib.ReparseFailureQuery)
    endif()
  else()
    if(FORMAL_REQUIRE_STP_TEST_ORACLE)
      message(FATAL_ERROR
        "FORMAL_REQUIRE_STP_TEST_ORACLE requires a runnable external stp executable")
    endif()
    add_capability_gate(
      FormalExpr.SMT.ExternalOracle
      "M1 environment"
      "the optional STP executable is unavailable or cannot load its runtime libraries"
      "m1;smt;test-oracle"
    )
  endif()

  if(FORMAL_ENABLE_STP)
    find_program(FORMAL_Z3_EXECUTABLE NAMES z3)
    if(FORMAL_Z3_EXECUTABLE)
      execute_process(
        COMMAND ${FORMAL_Z3_EXECUTABLE} -version
        RESULT_VARIABLE FORMAL_Z3_TEST_RESULT
        OUTPUT_QUIET
        ERROR_QUIET)
      if(NOT FORMAL_Z3_TEST_RESULT EQUAL 0)
        set(FORMAL_Z3_EXECUTABLE "")
      endif()
    endif()
    if(FORMAL_Z3_EXECUTABLE)
      add_test(NAME FormalSolver.Cross.Z3
        COMMAND ${CMAKE_COMMAND}
          -DSOLVER_EMITTER=$<TARGET_FILE:FormalSolverDifferentialEmitter>
          -DZ3_EXECUTABLE=${FORMAL_Z3_EXECUTABLE}
          -DSTP_LIBRARY_PATH=${FORMAL_STP_TEST_LIBRARY_PATH}
          -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/m2-z3-differential
          -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_solver_z3_differential_test.cmake)
      set_tests_properties(FormalSolver.Cross.Z3 PROPERTIES
        LABELS "m2;differential;z3;stp")
      formal_set_sanitizer_test_environment(FormalSolver.Cross.Z3)
      formal_set_stp_test_environment(FormalSolver.Cross.Z3)
    elseif(FORMAL_REQUIRE_Z3_TEST_ORACLE)
      message(FATAL_ERROR
        "FORMAL_REQUIRE_Z3_TEST_ORACLE requires a runnable z3 executable")
    else()
      add_capability_gate(
        FormalSolver.Cross.Z3
        "M2 optional"
        "Z3 is unavailable, so representative STP/Z3 cross-checks did not run"
        "m2;differential;z3"
      )
    endif()
  endif()

  add_test(NAME FormalExpr.SMT.StableOutput
    COMMAND ${CMAKE_COMMAND}
      -DEXPR_TEST_EXECUTABLE=$<TARGET_FILE:FormalExprSMTEmitter>
      -DOUTPUT_ONE=${CMAKE_CURRENT_BINARY_DIR}/formal-stable-one.smt2
      -DOUTPUT_TWO=${CMAKE_CURRENT_BINARY_DIR}/formal-stable-two.smt2
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_stable_smt_test.cmake)
  set_tests_properties(FormalExpr.SMT.StableOutput PROPERTIES
    LABELS "m1;smt;determinism")
  formal_set_sanitizer_test_environment(FormalExpr.SMT.StableOutput)

  find_program(FORMAL_INCOMPATIBLE_CLANG_EXECUTABLE NAMES clang-13)
  if(FORMAL_INCOMPATIBLE_CLANG_EXECUTABLE)
    add_test(NAME FormalPass.EndToEnd.IncompatibleClang
      COMMAND ${CMAKE_COMMAND}
        -DCLANG_EXECUTABLE=${FORMAL_INCOMPATIBLE_CLANG_EXECUTABLE}
        -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
        -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
        -DSOURCE_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/c/formal_top.c
        -DBITCODE_FILE=${CMAKE_CURRENT_BINARY_DIR}/formal-incompatible.bc
        -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/formal-incompatible-results
        -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_incompatible_c_bitcode_test.cmake)
    set_tests_properties(FormalPass.EndToEnd.IncompatibleClang PROPERTIES
      LABELS "end-to-end;m0;negative")
    formal_set_sanitizer_test_environment(
      FormalPass.EndToEnd.IncompatibleClang)
  endif()

  find_program(FORMAL_INCOMPATIBLE_OPT_EXECUTABLE NAMES opt-13)
  if(FORMAL_INCOMPATIBLE_OPT_EXECUTABLE)
    add_test(NAME FormalPass.Plugin.IncompatibleHostOpt
      COMMAND ${CMAKE_COMMAND}
        -DOPT_EXECUTABLE=${FORMAL_INCOMPATIBLE_OPT_EXECUTABLE}
        -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
        -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
        -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/formal-incompatible-opt
        -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_incompatible_opt_test.cmake)
    set_tests_properties(FormalPass.Plugin.IncompatibleHostOpt PROPERTIES
      LABELS "integration;m0;negative;toolchain")
  endif()

  add_test(NAME FormalPass.Plugin.RuntimeHostDigestMismatch
    COMMAND ${CMAKE_COMMAND}
      -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
      -DPLUGIN_FILE=$<TARGET_FILE:FormalPassWrongHostDigest>
      -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
      -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/formal-host-digest-mismatch
      -DEXPECT_HOST_MISMATCH=ON
      -DEXPECT_ACTUAL_SHA256=${FORMAL_PINNED_OPT_SHA256}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_incompatible_opt_test.cmake)
  set_tests_properties(FormalPass.Plugin.RuntimeHostDigestMismatch PROPERTIES
    LABELS "integration;m0;negative;toolchain")
  formal_set_sanitizer_test_environment(
    FormalPass.Plugin.RuntimeHostDigestMismatch)

  add_test(
    NAME FormalPass.Plugin.EntryDiagnostics
    COMMAND ${CMAKE_COMMAND}
      -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
      -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
      -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
      -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/formal-smoke-results
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_smoke_test.cmake
  )
  set_tests_properties(FormalPass.Plugin.EntryDiagnostics PROPERTIES
    LABELS "integration;m0"
    # This test starts the pinned opt once for the positive case and once for
    # each negative entry diagnostic.  Debug LLVM builds may be several
    # gigabytes and validate their executable digest at every startup, so the
    # generic 1500-second CTest limit can expire despite steady progress.
    TIMEOUT 3600
  )
  formal_set_sanitizer_test_environment(FormalPass.Plugin.EntryDiagnostics)

  add_test(NAME FormalPass.Plugin.ExitCodes
    COMMAND ${CMAKE_COMMAND}
      -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
      -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
      -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
      -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/formal-exit-results
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_exit_code_test.cmake)
  set_tests_properties(FormalPass.Plugin.ExitCodes PROPERTIES
    LABELS "integration;m0;negative")
  formal_set_sanitizer_test_environment(FormalPass.Plugin.ExitCodes)

  add_test(NAME FormalPass.Plugin.StaleArtifact
    COMMAND ${CMAKE_COMMAND}
      -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
      -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
      -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
      -DOUTPUT_ROOT=${CMAKE_CURRENT_BINARY_DIR}/formal-stale-root
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_stale_artifact_test.cmake)
  set_tests_properties(FormalPass.Plugin.StaleArtifact PROPERTIES
    LABELS "integration;m0;negative;artifact")
  formal_set_sanitizer_test_environment(FormalPass.Plugin.StaleArtifact)

  add_test(NAME FormalPass.Plugin.ArtifactIOFailure
    COMMAND ${CMAKE_COMMAND}
      -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
      -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
      -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
      -DOUTPUT_ROOT=${CMAKE_CURRENT_BINARY_DIR}/formal-artifact-io-root
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_artifact_io_failure_test.cmake)
  set_tests_properties(FormalPass.Plugin.ArtifactIOFailure PROPERTIES
    LABELS "integration;m0;negative;artifact")
  formal_set_sanitizer_test_environment(FormalPass.Plugin.ArtifactIOFailure)

  add_test(NAME FormalPass.Plugin.ConcurrentArtifacts
    COMMAND ${CMAKE_COMMAND}
      -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
      -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
      -DINPUT_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/formal_top.ll
      -DOUTPUT_ROOT=${CMAKE_CURRENT_BINARY_DIR}/formal-concurrent-root
      -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_concurrent_artifact_test.cmake)
  set_tests_properties(FormalPass.Plugin.ConcurrentArtifacts PROPERTIES
    LABELS "integration;m0;negative;artifact;concurrency")
  formal_set_sanitizer_test_environment(
    FormalPass.Plugin.ConcurrentArtifacts)

  if(FORMAL_CLANG_EXECUTABLE)
    add_test(
      NAME FormalPass.EndToEnd.CBitcode
      COMMAND ${CMAKE_COMMAND}
        -DCLANG_EXECUTABLE=${FORMAL_CLANG_EXECUTABLE}
        -DOPT_EXECUTABLE=${FORMAL_PINNED_OPT_EXECUTABLE}
        -DPLUGIN_FILE=$<TARGET_FILE:FormalPass>
        -DSOURCE_FILE=${CMAKE_CURRENT_SOURCE_DIR}/tests/c/formal_top.c
        -DBITCODE_FILE=${CMAKE_CURRENT_BINARY_DIR}/formal-top.bc
        -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/formal-e2e-results
        -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_c_bitcode_test.cmake
    )
    set_tests_properties(FormalPass.EndToEnd.CBitcode PROPERTIES
      LABELS "end-to-end;m0"
    )
    formal_set_sanitizer_test_environment(FormalPass.EndToEnd.CBitcode)
  else()
    add_capability_gate(
      FormalPass.EndToEnd.CBitcode
      "M0 environment"
      "clang-${LLVM_VERSION_MAJOR} is unavailable, so compatible C-to-bitcode generation cannot run"
      "end-to-end;m0"
    )
  endif()

  configure_file(tests/lit.site.cfg.py.in
    ${CMAKE_CURRENT_BINARY_DIR}/tests/lit.site.cfg.py @ONLY)
  add_test(NAME FormalPass.Lit.M0M1
    COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv
      ${CMAKE_CURRENT_BINARY_DIR}/tests)
  set_tests_properties(FormalPass.Lit.M0M1 PROPERTIES
    LABELS "lit;ir;m0;m1"
    # The aggregate runs every IR fixture through the pinned host opt.  A
    # multi-gigabyte Debug opt validates its digest in each process, so the
    # suite needs a larger bound than a single integration test.
    TIMEOUT 14400)
  formal_set_sanitizer_test_environment(FormalPass.Lit.M0M1)
  add_capability_gate(
    FormalPass.UNSAFE.NativeReplay
    "M5"
    "there is no executor, concrete model, or replay runtime"
    "unsafe;replay"
  )
  add_capability_gate(
    FormalPass.UNKNOWN.PathTruncation
    "M3/M8"
    "path scheduling and truncation are not implemented"
    "unknown;path"
  )
  add_capability_gate(
    FormalPass.UNKNOWN.LoopBound
    "M8"
    "loop execution and per-state backedge bounds are not implemented"
    "unknown;loop"
  )
  add_capability_gate(
    FormalPass.UNKNOWN.UnsupportedInstruction
    "M3/M9"
    "instruction execution and UNKNOWN verdicts are not implemented"
    "unknown;unsupported"
  )
endif()
