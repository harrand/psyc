cmake_minimum_required(VERSION 3.23)
project(psyc LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
include(llvm.cmake)

add_executable(psyc
	psyc.cpp
)

target_compile_definitions(psyc PRIVATE "-D_CRT_SECURE_NO_WARNINGS")
target_include_directories(psyc PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_libraries(psyc PUBLIC ${llvm_dependent_libs})
target_link_options(psyc PUBLIC -Xlinker /STACK:8000000,8000000)
