cmake_minimum_required(VERSION 3.23)
project(psyc)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
option(PSYC_PROFILE_BUILD OFF)

set(PSYC_FIND_EXTERNAL_LLVM OFF CACHE BOOL "Enable this to locate an existing LLVM on your machine via `find_package`. If disabled, llvm will be built from scratch as a submodule.")

set(LLVM_TARGETS_TO_BUILD "X86")

if(PSYC_FIND_EXTERNAL_LLVM)
	message(STATUS "Looking for external LLVM...")
	find_package(LLVM REQUIRED CONFIG)
else()
	message(STATUS "Using LLVM from within submodule.")
	if(WIN32)
		set(LLVM_HOST_TRIPLE "x86_64-pc-windows-msvc")
		set(LLVM_DEFAULT_TARGET_TRIPLE ${LLVM_HOST_TRIPLE})
	endif()
	add_subdirectory(llvm/llvm)
	set(LLVM_INCLUDE_DIRS "llvm/llvm/include" "${CMAKE_BINARY_DIR}/llvm/llvm/include/")
endif()

llvm_map_components_to_libnames(llvm_dependent_libs support core irreader executionengine
	# need this for jit compilation (which is used to parse & jit-compile build meta-regions)
	interpreter jitlink mcjit
	#codegen bits:
	#AMDGPUCodeGen AArch64CodeGen ARMCodeGen AVRCodeGen BPFCodeGen HexagonCodeGen LanaiCodeGen LoongArchCodeGen MipsCodeGen MSP430CodeGen NVPTXCodeGen PowerPCCodeGen RISCVCodeGen SparcCodeGen SystemZCodeGen VECodeGen WebAssemblyCodeGen XCoreCodeGen
	X86CodeGen X86AsmParser
	)
separate_arguments(llvm_definitions_list NATIVE_COMMAND ${LLVM_DEFINITIONS})

add_library(libpsyc
	src2/ast.cpp
	src2/ast.hpp
	src2/config.hpp
	src2/diag.hpp
	src2/error.hpp
	src2/lex.cpp
	src2/lex.hpp

	src2/parse_macros.hpp

	src2/parse.cpp
	src2/parse.hpp
	src2/parser.cpp
	src2/parser.hpp
	src2/profile.cpp
	src2/profile.hpp
	src2/srcloc.hpp
	src2/static.cpp
	src2/static.hpp
	src2/timer.cpp
	src2/timer.hpp
	src2/type.cpp
	src2/type.hpp
	src2/util.cpp
	src2/util.hpp
)

cmake_path(GET PROJECT_SOURCE_DIR PARENT_PATH psyc_root_dir)

file(GLOB stdlib_psy_sources
	CONFIGURE_DEPENDS
	"${psyc_root_dir}/stdlib/*.psy"
)

if(${stdlib_psy_sources})
	add_custom_command(TARGET psyc POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy ${stdlib_psy_sources} $<TARGET_FILE_DIR:psyc>
		COMMENT "Copying psyc stdlib sources: ${stdlib_psy_sources}"
	)
endif()

add_custom_target(psyc_install
	COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:psyc>" "${CMAKE_INSTALL_PREFIX}/bin/$<TARGET_FILE_NAME:psyc>"
	DEPENDS psyc
	COMMENT "Installing psyc to ${CMAKE_INSTALL_PREFIX}"
	)
if(${stdlib_psy_sources})
	add_custom_command(TARGET psyc_install POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy ${stdlib_psy_sources} "${CMAKE_INSTALL_PREFIX}/bin/"
		COMMENT "Copying psyc stdlib sources: ${stdlib_psy_sources}"
	)
endif()

set(MI_OVERRIDE ON)
add_subdirectory(mimalloc)

target_include_directories(libpsyc PUBLIC ${LLVM_INCLUDE_DIRS})
target_compile_definitions(libpsyc PUBLIC ${llvm_definitions_list})
target_link_libraries(libpsyc PUBLIC ${llvm_dependent_libs} mimalloc-static)

if(PSYC_PROFILE_BUILD)
	set(TRACY_ENABLE ON)
	set(TRACY_NO_EXIT ON)
	add_subdirectory(tracy)
	target_link_libraries(libpsyc PUBLIC TracyClient)
	target_compile_definitions(libpsyc PUBLIC "-DPSYC_PROFILE")
endif()

add_executable(psyc
	src2/psyc_main.cpp
)
target_link_libraries(psyc PRIVATE libpsyc)

###################
## PSYC - TESTS ##
###################

include(CTest)
add_subdirectory(test)