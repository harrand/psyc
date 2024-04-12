cmake_minimum_required(VERSION 3.23)
project(psyc)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(psyc
	src/psyc_main.cpp
	src/ast.cpp
	src/ast.hpp
	src/diag.hpp
	src/lex.cpp
	src/lex.hpp
	src/parse.cpp
	src/parse.hpp
	src/semantic.cpp
	src/semantic.hpp
)
