cmake_minimum_required(VERSION 3.23)
project(psyc)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

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

add_executable(psyc
	src/psyc_main.cpp
	src/ast.cpp
	src/ast.hpp
	src/config.hpp
	src/diag.hpp
	src/error.hpp
	src/lex.cpp
	src/lex.hpp
	src/parse.cpp
	src/parse.hpp
	src/semal.cpp
	src/semal.hpp
	src/srcloc.hpp
	src/timer.cpp
	src/timer.hpp
	src/type.cpp
	src/type.hpp
	src/util.cpp
	src/util.hpp
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

target_include_directories(psyc PRIVATE ${LLVM_INCLUDE_DIRS})
target_compile_definitions(psyc PRIVATE ${llvm_definitions_list})
target_link_libraries(psyc PRIVATE ${llvm_dependent_libs})