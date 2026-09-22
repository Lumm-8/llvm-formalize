set(FORMAL_LLVM_MAJOR "20")
set(FORMAL_LLVM_VERSION "20.0.0git")
set(FORMAL_LLVM_REVISION "eccf4d44d346eee498b0ff709e625e3104448751")
set(FORMAL_STP_TEST_REVISION "1bdfe5076c3f78e2bb7fccc5efb1a2a36c4068e6")
set(FORMAL_STP_TEST_VERSION "2.3.3")
set(FORMAL_STP_TEST_COMPILER "/usr/bin/c++")

if(FORMAL_ENABLE_STP)
  find_package(STP 2.3.3 EXACT CONFIG REQUIRED)
  if(NOT TARGET stp)
    message(FATAL_ERROR "STP package did not provide its public 'stp' target")
  endif()
  if(NOT STP_VERSION STREQUAL FORMAL_STP_TEST_VERSION)
    message(FATAL_ERROR
      "FormalPass requires STP ${FORMAL_STP_TEST_VERSION}, but ${STP_VERSION} was found")
  endif()

  include(CheckCXXSourceRuns)
  set(FORMAL_STP_PROBE_RUNTIME_DIRS "")
  set(FORMAL_STP_IMPORTED_LOCATION "")
  foreach(FORMAL_STP_IMPORTED_CONFIGURATION
      DEBUG RELEASE RELWITHDEBINFO MINSIZEREL NOCONFIG)
    get_target_property(FORMAL_STP_CONFIGURATION_LOCATION stp
      "IMPORTED_LOCATION_${FORMAL_STP_IMPORTED_CONFIGURATION}")
    if(FORMAL_STP_CONFIGURATION_LOCATION AND
       NOT FORMAL_STP_CONFIGURATION_LOCATION MATCHES "-NOTFOUND$")
      set(FORMAL_STP_IMPORTED_LOCATION
        "${FORMAL_STP_CONFIGURATION_LOCATION}")
      break()
    endif()
  endforeach()
  if(NOT FORMAL_STP_IMPORTED_LOCATION)
    get_target_property(FORMAL_STP_IMPORTED_LOCATION stp IMPORTED_LOCATION)
  endif()
  if(FORMAL_STP_IMPORTED_LOCATION)
    get_filename_component(FORMAL_STP_IMPORTED_LIBRARY_DIR
      "${FORMAL_STP_IMPORTED_LOCATION}" DIRECTORY)
    list(APPEND FORMAL_STP_PROBE_RUNTIME_DIRS
      "${FORMAL_STP_IMPORTED_LIBRARY_DIR}")
  endif()
  get_target_property(FORMAL_STP_TRANSITIVE_LIBRARIES stp
    INTERFACE_LINK_LIBRARIES)
  foreach(FORMAL_STP_TRANSITIVE_LIBRARY IN LISTS
      FORMAL_STP_TRANSITIVE_LIBRARIES)
    if(IS_ABSOLUTE "${FORMAL_STP_TRANSITIVE_LIBRARY}" AND
       EXISTS "${FORMAL_STP_TRANSITIVE_LIBRARY}")
      get_filename_component(FORMAL_STP_TRANSITIVE_LIBRARY_DIR
        "${FORMAL_STP_TRANSITIVE_LIBRARY}" DIRECTORY)
      list(APPEND FORMAL_STP_PROBE_RUNTIME_DIRS
        "${FORMAL_STP_TRANSITIVE_LIBRARY_DIR}")
    endif()
  endforeach()
  list(REMOVE_DUPLICATES FORMAL_STP_PROBE_RUNTIME_DIRS)
  list(JOIN FORMAL_STP_PROBE_RUNTIME_DIRS ":" FORMAL_STP_PROBE_RUNTIME_PATH)
  set(FORMAL_SAVED_LD_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
  if(FORMAL_STP_PROBE_RUNTIME_PATH)
    set(ENV{LD_LIBRARY_PATH}
      "${FORMAL_STP_PROBE_RUNTIME_PATH}:$ENV{LD_LIBRARY_PATH}")
  endif()
  set(CMAKE_REQUIRED_INCLUDES ${STP_INCLUDE_DIRS})
  set(CMAKE_REQUIRED_LIBRARIES stp)
  unset(FORMAL_STP_API_MATCHES_LOCK CACHE)
  check_cxx_source_runs([[
    #include <stp/c_interface.h>
    #include <cstring>
    int main() {
      if (std::strcmp(get_git_version_tag(), "2.3.3") != 0)
        return 1;
      if (std::strcmp(get_git_version_sha(),
          "1bdfe5076c3f78e2bb7fccc5efb1a2a36c4068e6") != 0)
        return 2;
      const char *profile = get_compilation_env();
      if (!profile || std::strstr(profile, "-DNDEBUG") ||
          !std::strstr(profile, "ONLY_SIMPLE = OFF") ||
          !std::strstr(profile, "STATICCOMPILE = OFF") ||
          !std::strstr(profile, "FORCE_CMS = OFF") ||
          !std::strstr(profile, "ENABLE_TESTING = ON") ||
          !std::strstr(profile, "ENABLE_PYTHON_INTERFACE = OFF") ||
          !std::strstr(profile, "libminisat") ||
          std::strstr(profile, "USE_CRYPTOMINISAT"))
        return 3;
      VC vc = vc_createValidityChecker();
      if (!vc || !vc_supportsMinisat(vc) || !vc_useMinisat(vc) ||
          !vc_isUsingMinisat(vc))
        return 4;
      Expr wide = vc_bvConstExprFromDecStr(vc, 65, "18446744073709551616");
      int result = wide && vc_query(vc, vc_falseExpr(vc));
      vc_Destroy(vc);
      return result == 0 ? 0 : 5;
    }
  ]] FORMAL_STP_API_MATCHES_LOCK)
  unset(CMAKE_REQUIRED_INCLUDES)
  unset(CMAKE_REQUIRED_LIBRARIES)
  set(ENV{LD_LIBRARY_PATH} "${FORMAL_SAVED_LD_LIBRARY_PATH}")
  if(NOT FORMAL_STP_API_MATCHES_LOCK)
    message(FATAL_ERROR
      "STP headers/library/runtime do not match the locked 2.3.3 MiniSat API profile")
  endif()
  set(FORMAL_STP_VERSION "${FORMAL_STP_TEST_VERSION}")
  set(FORMAL_STP_REVISION "${FORMAL_STP_TEST_REVISION}")
endif()

set(FORMAL_PINNED_OPT_EXECUTABLE "${LLVM_TOOLS_BINARY_DIR}/opt" CACHE FILEPATH
  "Exact opt executable whose path, size, and SHA-256 are runtime-verified")
if(NOT EXISTS "${FORMAL_PINNED_OPT_EXECUTABLE}")
  message(FATAL_ERROR
    "FormalPass requires the pinned opt executable at ${FORMAL_PINNED_OPT_EXECUTABLE}")
endif()
file(SHA256 "${FORMAL_PINNED_OPT_EXECUTABLE}" FORMAL_PINNED_OPT_SHA256)
file(SIZE "${FORMAL_PINNED_OPT_EXECUTABLE}" FORMAL_PINNED_OPT_SIZE)

if(NOT LLVM_VERSION_MAJOR EQUAL FORMAL_LLVM_MAJOR)
  message(FATAL_ERROR
    "FormalPass requires LLVM ${FORMAL_LLVM_MAJOR}, but LLVM ${LLVM_VERSION_MAJOR} was found")
endif()

if(NOT LLVM_PACKAGE_VERSION STREQUAL FORMAL_LLVM_VERSION)
  message(FATAL_ERROR
    "FormalPass requires LLVM package ${FORMAL_LLVM_VERSION}, but ${LLVM_PACKAGE_VERSION} was found")
endif()

find_file(FORMAL_LLVM_VCS_REVISION_HEADER
  NAMES llvm/Support/VCSRevision.h
  HINTS ${LLVM_INCLUDE_DIRS}
  NO_DEFAULT_PATH
  REQUIRED)
file(STRINGS "${FORMAL_LLVM_VCS_REVISION_HEADER}"
  FORMAL_LLVM_VCS_REVISION_LINE
  REGEX "^#define LLVM_REVISION \"[0-9a-f]+\"$")
string(REGEX REPLACE
  "^#define LLVM_REVISION \"([0-9a-f]+)\"$" "\\1"
  FORMAL_ACTUAL_LLVM_REVISION "${FORMAL_LLVM_VCS_REVISION_LINE}")
if(NOT FORMAL_ACTUAL_LLVM_REVISION STREQUAL FORMAL_LLVM_REVISION)
  message(FATAL_ERROR
    "FormalPass requires LLVM revision ${FORMAL_LLVM_REVISION}, but ${FORMAL_ACTUAL_LLVM_REVISION} was found")
endif()

# This repository's historical LLVM build exports globally injected KLEE/STP
# link dependencies.  M0/M1 deliberately use clean archive targets so the
# solver-independent expression library does not inherit those dependencies.
find_library(FORMAL_LLVM_SUPPORT_ARCHIVE
  NAMES LLVMSupport
  HINTS "${LLVM_LIBRARY_DIR}"
  NO_DEFAULT_PATH
  REQUIRED)
find_library(FORMAL_LLVM_DEMANGLE_ARCHIVE
  NAMES LLVMDemangle
  HINTS "${LLVM_LIBRARY_DIR}"
  NO_DEFAULT_PATH
  REQUIRED)

find_package(ZLIB REQUIRED)
find_package(LibXml2 REQUIRED)

add_library(FormalLLVMSupport INTERFACE)
target_link_libraries(FormalLLVMSupport INTERFACE
  "${FORMAL_LLVM_SUPPORT_ARCHIVE}"
  "${FORMAL_LLVM_DEMANGLE_ARCHIVE}"
  ZLIB::ZLIB
  LibXml2::LibXml2
  ${CMAKE_DL_LIBS}
)
if(UNIX AND NOT APPLE)
  target_link_libraries(FormalLLVMSupport INTERFACE rt m)
endif()
