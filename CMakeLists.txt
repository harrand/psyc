cmake_minimum_required(VERSION 3.23)
project(psyc LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)

add_executable(psyc
	psyc.cpp
)

target_compile_definitions(psyc PRIVATE "-D_CRT_SECURE_NO_WARNINGS")
