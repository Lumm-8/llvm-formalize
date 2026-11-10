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
option(FORMAL_BUILD_TESTS "Build FormalPass tests" ON)
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
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(FormalPass PRIVATE -fexceptions)
endif()
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
set_target_properties(FormalExpr PROPERTIES POSITION_INDEPENDENT_CODE ON)
formal_enable_sanitizers(FormalExpr)
formal_enable_project_warnings(FormalExpr)

add_library(formal_marker_runtime STATIC runtime/formal_marker_runtime.cpp)
target_include_directories(formal_marker_runtime PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
target_compile_features(formal_marker_runtime PUBLIC cxx_std_20)
formal_enable_sanitizers(formal_marker_runtime)
formal_enable_project_warnings(formal_marker_runtime)

add_library(formal_replay_runtime STATIC runtime/formal_replay_runtime.cpp)
target_include_directories(formal_replay_runtime PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
target_compile_features(formal_replay_runtime PUBLIC cxx_std_20)
target_link_libraries(formal_replay_runtime PUBLIC FormalLLVMSupport)
formal_enable_sanitizers(formal_replay_runtime)
formal_enable_project_warnings(formal_replay_runtime)

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
    set_target_properties(${target_name} PROPERTIES
      POSITION_INDEPENDENT_CODE ON)
    formal_enable_sanitizers(${target_name})
    formal_enable_project_warnings(${target_name})
  endfunction()

  formal_add_solver_library(FormalSolver FormalSTPWorker)

  add_executable(FormalSTPWorker tools/STPWorkerMain.cpp)
  target_link_libraries(FormalSTPWorker PRIVATE FormalSolver)
  formal_enable_sanitizers(FormalSTPWorker)
  formal_enable_project_warnings(FormalSTPWorker)

  add_library(FormalExecutor STATIC
    lib/Constraints.cpp
    lib/Executor.cpp
    lib/Memory.cpp
    lib/M5Artifacts.cpp
    lib/M5Run.cpp
    lib/Relation.cpp
    lib/Result.cpp
    lib/State.cpp
  )
  target_include_directories(FormalExecutor PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
  )
  target_compile_features(FormalExecutor PUBLIC cxx_std_20)
  target_link_libraries(FormalExecutor PUBLIC FormalSolver LLVMCore LLVMAnalysis)
  set_target_properties(FormalExecutor PROPERTIES POSITION_INDEPENDENT_CODE ON)
  formal_enable_sanitizers(FormalExecutor)
  formal_enable_project_warnings(FormalExecutor)
  # The opt host already owns LLVM's static registries. Linking the
  # transitive LLVMAnalysis archive into a pass plugin would register its
  # command-line options twice at dlopen time.
  target_link_libraries(FormalPass PRIVATE
    $<TARGET_FILE:FormalExecutor>
    $<TARGET_FILE:FormalSolver>
    $<TARGET_FILE:FormalExpr>
    stp)
  add_dependencies(FormalPass FormalExecutor FormalSolver FormalExpr)

  add_executable(FormalScalarExecutor tools/ScalarExecutorMain.cpp)
  target_link_libraries(FormalScalarExecutor PRIVATE
    FormalExecutor LLVMIRReader LLVMCore LLVMSupport)
  formal_enable_sanitizers(FormalScalarExecutor)
  formal_enable_project_warnings(FormalScalarExecutor)

  add_executable(formal-check
    tools/ScalarExecutorMain.cpp lib/FormalEntry.cpp)
  target_compile_definitions(formal-check PRIVATE
    FORMAL_CHECK_DEFAULT_M5=1
    FORMAL_PINNED_LLVM_VERSION="${FORMAL_LLVM_VERSION}"
    FORMAL_PINNED_LLVM_REVISION="${FORMAL_LLVM_REVISION}")
  target_link_libraries(formal-check PRIVATE
    FormalExecutor LLVMIRReader LLVMCore LLVMAnalysis
    LLVMTargetParser LLVMSupport)
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(formal-check PRIVATE -fno-rtti)
  endif()
  formal_enable_sanitizers(formal-check)
  formal_enable_project_warnings(formal-check)
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
  target_link_libraries(FormalPassWrongHostDigest PRIVATE
    FormalLLVMSupport
    $<TARGET_FILE:FormalExecutor>
    $<TARGET_FILE:FormalSolver>
    $<TARGET_FILE:FormalExpr>
    stp)
  add_dependencies(FormalPassWrongHostDigest
    FormalExecutor FormalSolver FormalExpr)
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(FormalPassWrongHostDigest PRIVATE
      -fno-rtti -fexceptions)
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

    add_executable(FormalExecutorUnitTests
      tests/ExecutorUnitTest.cpp
      tests/TestMain.cpp
    )
    target_link_libraries(FormalExecutorUnitTests PRIVATE
      FormalExecutor FormalGTest LLVMAsmParser LLVMCore)
    add_dependencies(FormalExecutorUnitTests FormalSTPWorker)
    formal_enable_sanitizers(FormalExecutorUnitTests)
    formal_enable_project_warnings(FormalExecutorUnitTests)

    add_test(NAME FormalExecutor.Unit COMMAND FormalExecutorUnitTests)
    set_tests_properties(FormalExecutor.Unit PROPERTIES
      LABELS "unit;m3;executor")
    formal_set_sanitizer_test_environment(FormalExecutor.Unit)
    formal_set_stp_test_environment(FormalExecutor.Unit)

    add_executable(FormalM6LayoutProbe tests/M6LayoutProbe.cpp)
    target_link_libraries(FormalM6LayoutProbe PRIVATE LLVMCore)
    formal_enable_sanitizers(FormalM6LayoutProbe)
    formal_enable_project_warnings(FormalM6LayoutProbe)
    add_test(NAME FormalExecutor.M6.Layout COMMAND FormalM6LayoutProbe)
    set_tests_properties(FormalExecutor.M6.Layout PROPERTIES
      LABELS "unit;m6;layout")
    formal_set_sanitizer_test_environment(FormalExecutor.M6.Layout)
    add_test(NAME FormalExecutor.Lit.M6
      COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv --filter=m6/
        ${CMAKE_CURRENT_BINARY_DIR}/tests)
    set_tests_properties(FormalExecutor.Lit.M6 PROPERTIES
      LABELS "lit;ir;m6;aggregate;memory")
    formal_set_sanitizer_test_environment(FormalExecutor.Lit.M6)
    add_test(NAME FormalExecutor.Lit.M7
      COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv --filter=m7/
        ${CMAKE_CURRENT_BINARY_DIR}/tests)
    set_tests_properties(FormalExecutor.Lit.M7 PROPERTIES
      LABELS "lit;ir;m7;calls;heap")
    formal_set_sanitizer_test_environment(FormalExecutor.Lit.M7)
    add_test(NAME FormalExecutor.Lit.M8
      COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv --filter=m8/
        ${CMAKE_CURRENT_BINARY_DIR}/tests)
    set_tests_properties(FormalExecutor.Lit.M8 PROPERTIES
      LABELS "lit;ir;m8;loop")
    formal_set_sanitizer_test_environment(FormalExecutor.Lit.M8)
    find_program(FORMAL_PYTHON3_EXECUTABLE NAMES python3 REQUIRED)
    add_test(NAME FormalExecutor.M7.HeapEvents
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_events.py
        $<TARGET_FILE:FormalScalarExecutor>
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m7/heap-options.ll)
    set_tests_properties(FormalExecutor.M7.HeapEvents PROPERTIES
      LABELS "m7;heap;artifacts")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.HeapEvents)
    add_test(NAME FormalExecutor.M7.NoundefAggregate
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_noundef_aggregate.py
        $<TARGET_FILE:formal-check>
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m7/call-boundaries.ll)
    set_tests_properties(FormalExecutor.M7.NoundefAggregate PROPERTIES
      LABELS "m7;abi;artifacts")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.NoundefAggregate)
    add_executable(FormalM6NativeBytes tests/M6NativeBytes.cpp)
    formal_enable_sanitizers(FormalM6NativeBytes)
    add_test(NAME FormalExecutor.M6.NativeBytes
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m6_native_bytes.py
        $<TARGET_FILE:FormalM6NativeBytes>
        $<TARGET_FILE:FormalScalarExecutor>)
    set_tests_properties(FormalExecutor.M6.NativeBytes PROPERTIES
      LABELS "m6;native;bytes")
    formal_set_sanitizer_test_environment(FormalExecutor.M6.NativeBytes)
    if(FORMAL_CLANG_EXECUTABLE)
      add_test(NAME FormalExecutor.M7.ClangABI
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_clang_abi.py
          ${FORMAL_CLANG_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m7_abi.c
          $<TARGET_FILE:FormalScalarExecutor>)
      set_tests_properties(FormalExecutor.M7.ClangABI PROPERTIES
        LABELS "m7;native;clang;abi")
      formal_set_sanitizer_test_environment(FormalExecutor.M7.ClangABI)
      add_test(NAME FormalExecutor.M6.CompiledIR
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m6_compiled_ir.py
          ${FORMAL_CLANG_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/M6NativeBytes.cpp
          $<TARGET_FILE:FormalM6NativeBytes>
          $<TARGET_FILE:FormalScalarExecutor>)
      set_tests_properties(FormalExecutor.M6.CompiledIR PROPERTIES
        LABELS "m6;native;clang;ir")
      formal_set_sanitizer_test_environment(FormalExecutor.M6.CompiledIR)
    endif()
    add_test(NAME FormalExecutor.M5.AssumeSolverUnknown
      COMMAND FormalExecutorUnitTests
        --gtest_filter=FormalExecutor.AssumeBackendUnknownKeepsThePathIncomplete)
    set_tests_properties(FormalExecutor.M5.AssumeSolverUnknown PROPERTIES
      LABELS "m5;assume;unit")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.AssumeSolverUnknown)

    add_executable(FormalMemoryUnitTests
      tests/MemoryUnitTest.cpp
      tests/TestMain.cpp
    )
    target_link_libraries(FormalMemoryUnitTests PRIVATE
      FormalExecutor FormalGTest LLVMAsmParser LLVMCore)
    add_dependencies(FormalMemoryUnitTests FormalSTPWorker)
    find_program(FORMAL_M4_NATIVE_SANITIZER_CC NAMES gcc)
    if(NOT FORMAL_M4_NATIVE_SANITIZER_CC)
      message(FATAL_ERROR "M4 model replay tests require GCC with ASan/UBSan")
    endif()
    set(FORMAL_M4_REPLAY_BINARY "${CMAKE_CURRENT_BINARY_DIR}/FormalM4Replay")
    add_custom_command(
      OUTPUT "${FORMAL_M4_REPLAY_BINARY}"
      COMMAND "${FORMAL_M4_NATIVE_SANITIZER_CC}" -O0 -g
        -fsanitize=address,undefined -fno-sanitize-recover=all
        "${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m4/replay.c"
        -o "${FORMAL_M4_REPLAY_BINARY}"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m4/replay.c"
      VERBATIM)
    add_custom_target(FormalM4ReplayNative DEPENDS "${FORMAL_M4_REPLAY_BINARY}")
    add_dependencies(FormalMemoryUnitTests FormalM4ReplayNative)
    target_compile_definitions(FormalMemoryUnitTests PRIVATE
      FORMAL_M4_REPLAY_BINARY="${FORMAL_M4_REPLAY_BINARY}"
      FORMAL_M4_IR_DIRECTORY="${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m4")
    formal_enable_sanitizers(FormalMemoryUnitTests)
    formal_enable_project_warnings(FormalMemoryUnitTests)
    add_test(NAME FormalMemory.Unit COMMAND FormalMemoryUnitTests)
    set_tests_properties(FormalMemory.Unit PROPERTIES
      LABELS "unit;m4;memory;property")
    formal_set_sanitizer_test_environment(FormalMemory.Unit)
    formal_set_stp_test_environment(FormalMemory.Unit)

    add_executable(FormalMemoryBenchmark tests/MemoryBenchmark.cpp)
    target_link_libraries(FormalMemoryBenchmark PRIVATE FormalExecutor LLVMCore)
    formal_enable_sanitizers(FormalMemoryBenchmark)
    formal_enable_project_warnings(FormalMemoryBenchmark)
    add_test(NAME FormalMemory.Benchmark COMMAND FormalMemoryBenchmark)
    set_tests_properties(FormalMemory.Benchmark PROPERTIES
      LABELS "m4;memory;benchmark")
    formal_set_sanitizer_test_environment(FormalMemory.Benchmark)

    if(FORMAL_CLANG_EXECUTABLE)
      set(FORMAL_M3_NATIVE_LIBRARY
        "${CMAKE_CURRENT_BINARY_DIR}/libFormalM3DifferentialNative${CMAKE_SHARED_LIBRARY_SUFFIX}")
      add_custom_command(
        OUTPUT ${FORMAL_M3_NATIVE_LIBRARY}
        COMMAND ${FORMAL_CLANG_EXECUTABLE} -O0 -fPIC -shared
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m3/differential.ll
          -o ${FORMAL_M3_NATIVE_LIBRARY}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m3/differential.ll
        COMMENT "Compiling the independent M3 native differential oracle"
        VERBATIM)
      add_custom_target(FormalM3DifferentialNative
        DEPENDS ${FORMAL_M3_NATIVE_LIBRARY})

      add_executable(FormalExecutorDifferentialTests
        tests/ExecutorDifferentialTest.cpp
        tests/TestMain.cpp
      )
      target_compile_definitions(FormalExecutorDifferentialTests PRIVATE
        FORMAL_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}"
        FORMAL_M3_NATIVE_LIBRARY="${FORMAL_M3_NATIVE_LIBRARY}")
      target_link_libraries(FormalExecutorDifferentialTests PRIVATE
        FormalExecutor FormalGTest LLVMIRReader LLVMCore LLVMSupport
        ${CMAKE_DL_LIBS})
      add_dependencies(FormalExecutorDifferentialTests
        FormalSTPWorker FormalM3DifferentialNative)
      formal_enable_sanitizers(FormalExecutorDifferentialTests)
      formal_enable_project_warnings(FormalExecutorDifferentialTests)

      add_test(NAME FormalExecutor.Differential
        COMMAND FormalExecutorDifferentialTests)
      set_tests_properties(FormalExecutor.Differential PROPERTIES
        LABELS "m3;executor;differential;exhaustive;native")
      formal_set_sanitizer_test_environment(FormalExecutor.Differential)
      formal_set_stp_test_environment(FormalExecutor.Differential)
    else()
      add_capability_gate(
        FormalExecutor.Differential
        "M3 environment"
        "the pinned Clang is unavailable, so the native differential oracle cannot be built"
        "m3;executor;differential;exhaustive;native")
    endif()

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
    # The aggregate discovers all IR fixtures.  Pre-M3 fixtures start the
    # multi-gigabyte Debug opt and validate its digest in each process, so the
    # suite needs a larger bound than a single integration test.
    TIMEOUT 14400)
  formal_set_sanitizer_test_environment(FormalPass.Lit.M0M1)
  add_test(NAME FormalExecutor.Lit.M3
    COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv --filter=m3/
      ${CMAKE_CURRENT_BINARY_DIR}/tests)
  set_tests_properties(FormalExecutor.Lit.M3 PROPERTIES
    LABELS "lit;ir;m3;executor")
  formal_set_sanitizer_test_environment(FormalExecutor.Lit.M3)
  add_test(NAME FormalExecutor.Lit.M4
    COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv --filter=m4/
      ${CMAKE_CURRENT_BINARY_DIR}/tests)
  set_tests_properties(FormalExecutor.Lit.M4 PROPERTIES
    LABELS "lit;ir;m4;memory")
  formal_set_sanitizer_test_environment(FormalExecutor.Lit.M4)
  add_test(NAME FormalExecutor.Lit.M5
    COMMAND ${FORMAL_LLVM_LIT_EXECUTABLE} -sv --filter=m5/
      ${CMAKE_CURRENT_BINARY_DIR}/tests)
  set_tests_properties(FormalExecutor.Lit.M5 PROPERTIES
    LABELS "lit;ir;m5;registration")
  formal_set_sanitizer_test_environment(FormalExecutor.Lit.M5)
  add_executable(FormalRelationUnitTests
    tests/RelationUnitTest.cpp tests/TestMain.cpp)
  target_link_libraries(FormalRelationUnitTests PRIVATE
    FormalExecutor FormalGTest)
  formal_enable_sanitizers(FormalRelationUnitTests)
  formal_enable_project_warnings(FormalRelationUnitTests)
  add_test(NAME FormalExecutor.M5.RelationUnit
    COMMAND FormalRelationUnitTests)
  set_tests_properties(FormalExecutor.M5.RelationUnit PROPERTIES
    LABELS "unit;m5;relation;closure")
  formal_set_sanitizer_test_environment(FormalExecutor.M5.RelationUnit)
  find_program(FORMAL_PYTHON3_EXECUTABLE NAMES python3 REQUIRED)
  set(FORMAL_M5_REPLAY_MAIN ${CMAKE_CURRENT_BINARY_DIR}/m5-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M5_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replay_top --output ${FORMAL_M5_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM5ReplayFixture
    tests/M5ReplayFixture.cpp ${FORMAL_M5_REPLAY_MAIN})
  target_link_libraries(FormalM5ReplayFixture PRIVATE formal_replay_runtime)
  formal_enable_sanitizers(FormalM5ReplayFixture)
  formal_enable_project_warnings(FormalM5ReplayFixture)
  set(FORMAL_M5_ERROR_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m5-error-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M5_ERROR_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayErrorTop --output ${FORMAL_M5_ERROR_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM5ErrorReplayFixture
    tests/M5ErrorReplayFixture.cpp ${FORMAL_M5_ERROR_REPLAY_MAIN})
  target_link_libraries(FormalM5ErrorReplayFixture PRIVATE formal_replay_runtime)
  target_include_directories(FormalM5ErrorReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM5ErrorReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM5ErrorReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow
    -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM5ErrorReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM5ErrorReplayFixture)
  set(FORMAL_M7_HEAP_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m7-heap-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M7_HEAP_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayHeapErrorTop --output ${FORMAL_M7_HEAP_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM7HeapReplayFixture
    tests/M7HeapReplayFixture.cpp ${FORMAL_M7_HEAP_REPLAY_MAIN})
  target_link_libraries(FormalM7HeapReplayFixture PRIVATE formal_replay_runtime)
  target_include_directories(FormalM7HeapReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM7HeapReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM7HeapReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow
    -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM7HeapReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM7HeapReplayFixture)
  set(FORMAL_M7_UAF_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m7-uaf-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M7_UAF_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayUseAfterFreeTop --output ${FORMAL_M7_UAF_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM7UAFReplayFixture
    tests/M7HeapReplayFixture.cpp ${FORMAL_M7_UAF_REPLAY_MAIN})
  target_link_libraries(FormalM7UAFReplayFixture PRIVATE formal_replay_runtime)
  target_include_directories(FormalM7UAFReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM7UAFReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM7UAFReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM7UAFReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM7UAFReplayFixture)
  set(FORMAL_M7_INVALID_FREE_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m7-invalid-free-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M7_INVALID_FREE_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayInvalidFreeTop
      --output ${FORMAL_M7_INVALID_FREE_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM7InvalidFreeReplayFixture
    tests/M7HeapReplayFixture.cpp ${FORMAL_M7_INVALID_FREE_REPLAY_MAIN})
  target_link_libraries(FormalM7InvalidFreeReplayFixture PRIVATE
    formal_replay_runtime)
  target_include_directories(FormalM7InvalidFreeReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM7InvalidFreeReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM7InvalidFreeReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM7InvalidFreeReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM7InvalidFreeReplayFixture)
  set(FORMAL_M7_INTERIOR_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m7-interior-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M7_INTERIOR_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayInteriorFreeTop
      --output ${FORMAL_M7_INTERIOR_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM7InteriorReplayFixture
    tests/M7HeapReplayFixture.cpp ${FORMAL_M7_INTERIOR_REPLAY_MAIN})
  target_link_libraries(FormalM7InteriorReplayFixture PRIVATE
    formal_replay_runtime)
  target_include_directories(FormalM7InteriorReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM7InteriorReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM7InteriorReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM7InteriorReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM7InteriorReplayFixture)
  set(FORMAL_M7_REALLOC_UAF_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m7-realloc-uaf-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M7_REALLOC_UAF_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayUseAfterReallocTop
      --output ${FORMAL_M7_REALLOC_UAF_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM7ReallocUAFReplayFixture
    tests/M7HeapReplayFixture.cpp ${FORMAL_M7_REALLOC_UAF_REPLAY_MAIN})
  target_link_libraries(FormalM7ReallocUAFReplayFixture PRIVATE
    formal_replay_runtime)
  target_include_directories(FormalM7ReallocUAFReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM7ReallocUAFReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM7ReallocUAFReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM7ReallocUAFReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM7ReallocUAFReplayFixture)
  set(FORMAL_M7_STACK_REPLAY_MAIN
    ${CMAKE_CURRENT_BINARY_DIR}/m7-stack-replay-main.cpp)
  add_custom_command(OUTPUT ${FORMAL_M7_STACK_REPLAY_MAIN}
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
      --entry replayUseAfterReturnTop
      --output ${FORMAL_M7_STACK_REPLAY_MAIN}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
  add_executable(FormalM7StackReplayFixture
    tests/M7StackReplayFixture.cpp ${FORMAL_M7_STACK_REPLAY_MAIN})
  target_link_libraries(FormalM7StackReplayFixture PRIVATE
    formal_replay_runtime)
  target_include_directories(FormalM7StackReplayFixture PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
  target_compile_definitions(FormalM7StackReplayFixture PRIVATE
    FORMAL_NATIVE_REPLAY=1)
  target_compile_options(FormalM7StackReplayFixture PRIVATE
    -O0 -g -fno-strict-overflow -fsanitize=address,undefined
    -fno-omit-frame-pointer)
  target_link_options(FormalM7StackReplayFixture PRIVATE
    -fsanitize=address,undefined)
  formal_enable_project_warnings(FormalM7StackReplayFixture)
  function(formal_add_m7_runtime_error_fixture Target Entry)
    set(Main ${CMAKE_CURRENT_BINARY_DIR}/${Target}-main.cpp)
    add_custom_command(OUTPUT ${Main}
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py
        --entry ${Entry} --output ${Main}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/tools/generate_replay_main.py)
    add_executable(${Target}
      tests/M7RuntimeErrorReplayFixture.cpp ${Main})
    target_link_libraries(${Target} PRIVATE formal_replay_runtime)
    target_include_directories(${Target} PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
    target_compile_definitions(${Target} PRIVATE FORMAL_NATIVE_REPLAY=1)
    target_compile_options(${Target} PRIVATE
      -O0 -g -fno-strict-overflow -fsanitize=address,undefined
      -fno-omit-frame-pointer)
    target_link_options(${Target} PRIVATE -fsanitize=address,undefined)
    formal_enable_project_warnings(${Target})
  endfunction()
  formal_add_m7_runtime_error_fixture(
    FormalM7OverlapMemcpyReplayFixture replayOverlapMemcpyTop)
  formal_add_m7_runtime_error_fixture(
    FormalM7DoubleDeleteReplayFixture replayDoubleDeleteTop)
  formal_add_m7_runtime_error_fixture(
    FormalM7FreeNewReplayFixture replayFreeNewTop)
  formal_add_m7_runtime_error_fixture(
    FormalM7AbortReplayFixture replayAbortTop)
  formal_add_m7_runtime_error_fixture(
    FormalM7AssertReplayFixture replayAssertTop)
  find_program(FORMAL_NATIVE_CC_EXECUTABLE NAMES cc gcc REQUIRED)
  add_test(NAME FormalExecutor.M5.ReplayRuntime
    COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_replay.py
      --fixture $<TARGET_FILE:FormalM5ReplayFixture>)
  set_tests_properties(FormalExecutor.M5.ReplayRuntime PROPERTIES
    LABELS "m5;replay;native")
  formal_set_sanitizer_test_environment(FormalExecutor.M5.ReplayRuntime)
  if(FORMAL_CLANG_EXECUTABLE)
    add_test(NAME FormalExecutor.M7.HeapErrorReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7HeapReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7HeapReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayHeapErrorTop --reason DOUBLE_FREE
        --asan-phrase "attempting double-free"
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/double-free)
    set_tests_properties(FormalExecutor.M7.HeapErrorReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.HeapErrorReplay)
    add_test(NAME FormalExecutor.M7.UseAfterFreeReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7UAFReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7HeapReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayUseAfterFreeTop --reason USE_AFTER_FREE
        --asan-phrase heap-use-after-free
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/use-after-free)
    set_tests_properties(FormalExecutor.M7.UseAfterFreeReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.UseAfterFreeReplay)
    add_test(NAME FormalExecutor.M7.InvalidFreeReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7InvalidFreeReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7HeapReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayInvalidFreeTop --reason INVALID_FREE
        --asan-phrase "attempting free on address which was not malloc"
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/invalid-free)
    set_tests_properties(FormalExecutor.M7.InvalidFreeReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.InvalidFreeReplay)
    add_test(NAME FormalExecutor.M7.InteriorFreeReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7InteriorReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7HeapReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayInteriorFreeTop --reason INVALID_FREE
        --asan-phrase "attempting free on address which was not malloc"
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/interior-free)
    set_tests_properties(FormalExecutor.M7.InteriorFreeReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.InteriorFreeReplay)
    add_test(NAME FormalExecutor.M7.ReallocUseAfterFreeReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7ReallocUAFReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7HeapReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayUseAfterReallocTop --reason USE_AFTER_FREE
        --model realloc --asan-phrase heap-use-after-free
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/realloc-old-pointer)
    set_tests_properties(FormalExecutor.M7.ReallocUseAfterFreeReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(
      FormalExecutor.M7.ReallocUseAfterFreeReplay)
    add_test(NAME FormalExecutor.M7.UseAfterReturnReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_stack_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7StackReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7StackReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/use-after-return)
    set_tests_properties(FormalExecutor.M7.UseAfterReturnReplay PROPERTIES
      LABELS "m7;stack;replay;asan")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.UseAfterReturnReplay)
    add_test(NAME FormalExecutor.M7.OverlapMemcpyReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7OverlapMemcpyReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7RuntimeErrorReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayOverlapMemcpyTop --reason OVERLAPPING_MEMCPY
        --model memcpy --allow-nonheap-error
        --asan-phrase memcpy-param-overlap
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/overlap-memcpy)
    set_tests_properties(FormalExecutor.M7.OverlapMemcpyReplay PROPERTIES
      LABELS "m7;memory;replay;asan")
    formal_set_sanitizer_test_environment(
      FormalExecutor.M7.OverlapMemcpyReplay)
    add_test(NAME FormalExecutor.M7.DoubleDeleteReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7DoubleDeleteReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7RuntimeErrorReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayDoubleDeleteTop --reason DOUBLE_FREE
        --model _ZdlPv --asan-phrase "attempting double-free"
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/double-delete)
    set_tests_properties(FormalExecutor.M7.DoubleDeleteReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(
      FormalExecutor.M7.DoubleDeleteReplay)
    add_test(NAME FormalExecutor.M7.FreeNewReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_heap_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7FreeNewReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7RuntimeErrorReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayFreeNewTop --reason INVALID_FREE
        --model free --asan-phrase alloc-dealloc-mismatch
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/free-new)
    set_tests_properties(FormalExecutor.M7.FreeNewReplay PROPERTIES
      LABELS "m7;heap;replay;asan")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.FreeNewReplay)
    add_test(NAME FormalExecutor.M7.AbortReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_termination_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7AbortReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7RuntimeErrorReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayAbortTop --reason ABORT_CALLED --model abort
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/abort)
    set_tests_properties(FormalExecutor.M7.AbortReplay PROPERTIES
      LABELS "m7;termination;replay;native")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.AbortReplay)
    add_test(NAME FormalExecutor.M7.AssertReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m7_termination_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM7AssertReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M7RuntimeErrorReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --entry replayAssertTop --reason ASSERTION_FAILED
        --model __assert_fail --stderr-phrase "M7 assertion"
        --evidence-dir ${CMAKE_CURRENT_BINARY_DIR}/m7-evidence/assert)
    set_tests_properties(FormalExecutor.M7.AssertReplay PROPERTIES
      LABELS "m7;termination;replay;native")
    formal_set_sanitizer_test_environment(FormalExecutor.M7.AssertReplay)
    add_test(NAME FormalExecutor.M5.ErrorNativeReplay
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_error_replay.py
        --executor $<TARGET_FILE:formal-check>
        --fixture $<TARGET_FILE:FormalM5ErrorReplayFixture>
        --clang ${FORMAL_CLANG_EXECUTABLE}
        --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/M5ErrorReplayFixture.cpp
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime)
    set_tests_properties(FormalExecutor.M5.ErrorNativeReplay PROPERTIES
      LABELS "m5;replay;native;asan;ubsan")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.ErrorNativeReplay)
    formal_set_stp_test_environment(FormalExecutor.M5.ErrorNativeReplay)
  endif()
  if(FORMAL_STP_TEST_EXECUTABLE AND FORMAL_STP_TEST_RESULT EQUAL 0)
    add_test(NAME FormalExecutor.M5.ArtifactClosure
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_artifacts.py
        --executor $<TARGET_FILE:FormalScalarExecutor>
        --formal-check $<TARGET_FILE:formal-check>
        --edge-ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/edge-cases.ll
        --scalar-ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/scalar.ll
        --assume-ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/assume-and-descriptors.ll
        --stp ${FORMAL_STP_TEST_EXECUTABLE})
    set_tests_properties(FormalExecutor.M5.ArtifactClosure PROPERTIES
      LABELS "m5;relation;closure;stp")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.ArtifactClosure)
    formal_set_stp_test_environment(FormalExecutor.M5.ArtifactClosure)
    add_test(NAME FormalExecutor.M5.TruthTable
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
        --executor $<TARGET_FILE:FormalScalarExecutor>
        --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/scalar.ll
        --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/native_truth_table.c
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
        --stp ${FORMAL_STP_TEST_EXECUTABLE}
        --per-input)
    set_tests_properties(FormalExecutor.M5.TruthTable PROPERTIES
      LABELS "m5;relation;native;stp")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.TruthTable)
    formal_set_stp_test_environment(FormalExecutor.M5.TruthTable)
    add_test(NAME FormalExecutor.M5.AssumeTruthTable
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
        --executor $<TARGET_FILE:FormalScalarExecutor>
        --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/assume-and-descriptors.ll
        --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/native_truth_table.c
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
        --stp ${FORMAL_STP_TEST_EXECUTABLE}
        --entry assumed --assumed --per-input)
    set_tests_properties(FormalExecutor.M5.AssumeTruthTable PROPERTIES
      LABELS "m5;relation;assume;native;stp")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.AssumeTruthTable)
    formal_set_stp_test_environment(FormalExecutor.M5.AssumeTruthTable)
    add_test(NAME FormalExecutor.M5.TupleTruthTable
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
        --executor $<TARGET_FILE:FormalScalarExecutor>
        --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/two_outputs.ll
        --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/native_two_outputs.c
        --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
        --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
        --stp ${FORMAL_STP_TEST_EXECUTABLE}
        --entry two_outputs --assumed --two-outputs --per-input)
    set_tests_properties(FormalExecutor.M5.TupleTruthTable PROPERTIES
      LABELS "m5;relation;tuple;native;stp")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.TupleTruthTable)
    formal_set_stp_test_environment(FormalExecutor.M5.TupleTruthTable)
    add_test(NAME FormalExecutor.M5.MultibyteRelation
      COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_multibyte.py
        --executor $<TARGET_FILE:FormalScalarExecutor>
        --little-ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/multibyte.ll
        --big-ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/multibyte_be.ll
        --stp ${FORMAL_STP_TEST_EXECUTABLE})
    set_tests_properties(FormalExecutor.M5.MultibyteRelation PROPERTIES
      LABELS "m5;relation;endian;stp")
    formal_set_sanitizer_test_environment(FormalExecutor.M5.MultibyteRelation)
    formal_set_stp_test_environment(FormalExecutor.M5.MultibyteRelation)
    if(FORMAL_CLANG_EXECUTABLE)
      add_test(NAME FormalExecutor.M5.ClangO0TruthTable
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
          --executor $<TARGET_FILE:FormalScalarExecutor>
          --clang ${FORMAL_CLANG_EXECUTABLE}
          --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m5_scalar.c
          --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/native_truth_table.c
          --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
          --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
          --stp ${FORMAL_STP_TEST_EXECUTABLE}
          --entry formal_top --assumed --assumed-plus-one --per-input)
      set_tests_properties(FormalExecutor.M5.ClangO0TruthTable PROPERTIES
        LABELS "m5;relation;clang;native;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M5.ClangO0TruthTable)
      formal_set_stp_test_environment(FormalExecutor.M5.ClangO0TruthTable)
      add_test(NAME FormalExecutor.M7.CrossCallTruthTable
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
          --executor $<TARGET_FILE:formal-check>
          --clang ${FORMAL_CLANG_EXECUTABLE}
          --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m7_relation.c
          --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/native_truth_table.c
          --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
          --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
          --stp ${FORMAL_STP_TEST_EXECUTABLE}
          --entry m7_relation_top --assumed --assumed-plus-one
          --per-input --expect-call-events)
      set_tests_properties(FormalExecutor.M7.CrossCallTruthTable PROPERTIES
        LABELS "m7;calls;relation;clang;native;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M7.CrossCallTruthTable)
      formal_set_stp_test_environment(FormalExecutor.M7.CrossCallTruthTable)
      add_test(NAME FormalExecutor.M5.FormalCheck
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
          --executor $<TARGET_FILE:formal-check>
          --clang ${FORMAL_CLANG_EXECUTABLE}
          --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m5_scalar.c
          --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m5/native_truth_table.c
          --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
          --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
          --stp ${FORMAL_STP_TEST_EXECUTABLE}
          --entry formal_top --assumed --assumed-plus-one)
      set_tests_properties(FormalExecutor.M5.FormalCheck PROPERTIES
        LABELS "m5;driver;preflight;relation;native;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M5.FormalCheck)
      formal_set_stp_test_environment(FormalExecutor.M5.FormalCheck)
      add_test(NAME FormalExecutor.M5.OptPlugin
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_plugin.py
          --clang ${FORMAL_CLANG_EXECUTABLE}
          --opt ${FORMAL_PINNED_OPT_EXECUTABLE}
          --plugin $<TARGET_FILE:FormalPass>
          --formal-check $<TARGET_FILE:formal-check>
          --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m5_scalar.c
          --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
          --stp ${FORMAL_STP_TEST_EXECUTABLE})
      set_tests_properties(FormalExecutor.M5.OptPlugin PROPERTIES
        LABELS "m5;plugin;preflight;relation;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M5.OptPlugin)
      formal_set_stp_test_environment(FormalExecutor.M5.OptPlugin)
      add_test(NAME FormalExecutor.M7.PluginConsistency
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_plugin.py
          --clang ${FORMAL_CLANG_EXECUTABLE}
          --opt ${FORMAL_PINNED_OPT_EXECUTABLE}
          --plugin $<TARGET_FILE:FormalPass>
          --formal-check $<TARGET_FILE:formal-check>
          --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m7_relation.c
          --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
          --stp ${FORMAL_STP_TEST_EXECUTABLE}
          --entry m7_relation_top --expect-calls)
      set_tests_properties(FormalExecutor.M7.PluginConsistency PROPERTIES
        LABELS "m7;calls;plugin;clang;relation;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M7.PluginConsistency)
      formal_set_stp_test_environment(FormalExecutor.M7.PluginConsistency)
      add_test(NAME FormalExecutor.M8.PluginDriverLoopConsistency
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_plugin_driver.py
          --formal-check $<TARGET_FILE:formal-check>
          --opt ${FORMAL_PINNED_OPT_EXECUTABLE}
          --plugin $<TARGET_FILE:FormalPass>
          --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/registered-loop.ll
          --multiple-reasons-ir
            ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/multiple-reasons.ll
          --unsafe-incomplete-ir
            ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/unsafe-and-incomplete.ll)
      set_tests_properties(FormalExecutor.M8.PluginDriverLoopConsistency
        PROPERTIES LABELS "m8;loop;plugin;driver;stp")
      formal_set_sanitizer_test_environment(
        FormalExecutor.M8.PluginDriverLoopConsistency)
      formal_set_stp_test_environment(
        FormalExecutor.M8.PluginDriverLoopConsistency)
      add_test(NAME FormalExecutor.M8.BackedgeContract
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_backedges.py
          --formal-check $<TARGET_FILE:formal-check>)
      set_tests_properties(FormalExecutor.M8.BackedgeContract PROPERTIES
        LABELS "m8;loop;backedge;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.BackedgeContract)
      formal_set_stp_test_environment(FormalExecutor.M8.BackedgeContract)
      add_test(NAME FormalExecutor.M8.DynamicActivations
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_structure.py
          --formal-check $<TARGET_FILE:formal-check>
          --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/structure.ll)
      set_tests_properties(FormalExecutor.M8.DynamicActivations PROPERTIES
        LABELS "m8;loop;activation;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.DynamicActivations)
      formal_set_stp_test_environment(FormalExecutor.M8.DynamicActivations)
      add_test(NAME FormalExecutor.M8.ResourceLimits
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_limits.py
          --formal-check $<TARGET_FILE:formal-check>
          --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/structure.ll)
      set_tests_properties(FormalExecutor.M8.ResourceLimits PROPERTIES
        LABELS "m8;loop;limits;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.ResourceLimits)
      formal_set_stp_test_environment(FormalExecutor.M8.ResourceLimits)
      add_test(NAME FormalExecutor.M8.LimitPolicies
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_policies.py
          --formal-check $<TARGET_FILE:formal-check>
          --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/registered-loop.ll
          --scalar-executor $<TARGET_FILE:FormalScalarExecutor>
          --structure-ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/structure.ll)
      set_tests_properties(FormalExecutor.M8.LimitPolicies PROPERTIES
        LABELS "m8;loop;policy;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.LimitPolicies)
      formal_set_stp_test_environment(FormalExecutor.M8.LimitPolicies)
      add_test(NAME FormalExecutor.M8.InvalidMetadata
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_metadata_invalid.py
          --formal-check $<TARGET_FILE:formal-check>
          --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/metadata-invalid.ll)
      set_tests_properties(FormalExecutor.M8.InvalidMetadata PROPERTIES
        LABELS "m8;loop;metadata;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.InvalidMetadata)
      formal_set_stp_test_environment(FormalExecutor.M8.InvalidMetadata)
      add_test(NAME FormalExecutor.M8.NativeDifferential
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m5_truth_table.py
          --executor $<TARGET_FILE:FormalScalarExecutor>
          --ir ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/loop-relation.ll
          --native ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/native_loop.c
          --include ${CMAKE_CURRENT_SOURCE_DIR}/runtime
          --cc ${FORMAL_NATIVE_CC_EXECUTABLE}
          --stp ${FORMAL_STP_TEST_EXECUTABLE}
          --entry loop_top --assumed --per-input)
      set_tests_properties(FormalExecutor.M8.NativeDifferential PROPERTIES
        LABELS "m8;loop;native;relation;stp")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.NativeDifferential)
      formal_set_stp_test_environment(FormalExecutor.M8.NativeDifferential)
      add_test(NAME FormalExecutor.M8.ClangPragmaMetadata
        COMMAND ${FORMAL_PYTHON3_EXECUTABLE}
          ${CMAKE_CURRENT_SOURCE_DIR}/tests/check_m8_pragma.py
          --clang ${FORMAL_CLANG_EXECUTABLE}
          --opt ${FORMAL_PINNED_OPT_EXECUTABLE}
          --filecheck ${FORMAL_FILECHECK_EXECUTABLE}
          --formal-check $<TARGET_FILE:formal-check>
          --formal-source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m8_pragma_e2e.c
          --source ${CMAKE_CURRENT_SOURCE_DIR}/tests/c/m8_pragma.c
          --check ${CMAKE_CURRENT_SOURCE_DIR}/tests/ir/m8/pragma.check)
      set_tests_properties(FormalExecutor.M8.ClangPragmaMetadata
        PROPERTIES LABELS "m8;loop;clang;metadata")
      formal_set_sanitizer_test_environment(FormalExecutor.M8.ClangPragmaMetadata)
      formal_set_stp_test_environment(FormalExecutor.M8.ClangPragmaMetadata)
    endif()
  endif()
endif()
