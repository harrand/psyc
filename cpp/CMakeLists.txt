cmake_minimum_required(VERSION 3.23)
project(psyc)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

set(PSYC_FIND_EXTERNAL_LLVM OFF CACHE BOOL "Enable this to locate an existing LLVM on your machine via `find_package`. If disabled, llvm will be built from scratch as a submodule.")

if(PSYC_FIND_EXTERNAL_LLVM)
	message(STATUS "Looking for external LLVM...")
	find_package(LLVM REQUIRED CONFIG)
else()
	message(STATUS "Using LLVM from within submodule.")
	if(WIN32)
		set(LLVM_HOST_TRIPLE x86_64)
	endif()
	add_subdirectory(llvm/llvm)
endif()

add_executable(psyc
	src/psyc_main.cpp
	src/ast.cpp
	src/ast.hpp
	src/codegen.cpp
	src/codegen.hpp
	src/diag.hpp
	src/lex.cpp
	src/lex.hpp
	src/parse.cpp
	src/parse.hpp
	src/semantic.cpp
	src/semantic.hpp
)
