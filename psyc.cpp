#include <print>
#include <cstdlib>
#include <source_location>
#include <stacktrace>

struct compile_args
{

};

void panic(const char* msg, std::source_location loc = std::source_location::current(), std::stacktrace trace = std::stacktrace::current())
{
	std::print("\033[1;31minternal compiler error: {}\n\tat {}({}:{}) `{}`\nstacktrace:\n==========\n{}\n=========\033[0m", msg, loc.file_name(), loc.line(), loc.column(), loc.function_name(), std::to_string(trace));
	asm volatile("int3");
	std::exit(-1);
}

int main(int argc, char** argv)
{
	panic("test");
}
