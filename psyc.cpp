#include <print>
#include <cstdlib>
#include <source_location>
#include <stacktrace>
#include <span>
#include <filesystem>
#include <format>

void crash()
{
	asm volatile("int3");
	std::exit(-1);
}

void panic(const char* msg, std::source_location loc = std::source_location::current(), std::stacktrace trace = std::stacktrace::current())
{
	std::print("\033[1;31minternal compiler error: {}\n\tat {}({}:{}) `{}`\nstacktrace:\n==========\n{}\n=========\033[0m", msg, loc.file_name(), loc.line(), loc.column(), loc.function_name(), std::to_string(trace));
	crash();
}

#define panic_ifnt(cond, msg) if(!cond){panic(msg);}

// errors

struct srcloc
{
	std::filesystem::path file;
	unsigned int line;
	unsigned int column;
};

template <>
struct std::formatter<srcloc> : std::formatter<std::string>
{
	auto format(srcloc loc, format_context& ctx) const
	{
		if(loc.file == std::filesystem::path{})
		{
			return formatter<string>::format("", ctx);
		}
		return formatter<string>::format(std::format("at {}({}:{})", loc.file.string(), loc.line, loc.column), ctx);
	}
};

enum class err
{
	argparse,
	lex,
	parse,
	semal,
	meta,
	codegen,
	assemble,
	link,
	_unknown
};
const char* err_names[] =
{
	"argument",
	"lex",
	"parse",
	"semantic analysis",
	"meta program",
	"code generation",
	"assembly",
	"linker",
	"unknown",
};

void generic_error(err ty, const char* msg, srcloc where, bool should_crash, std::format_args&& args)
{
	std::print("\033[1;31m{} error {}\033[0m: {}", err_names[static_cast<int>(ty)], where, std::vformat(msg, args));
	
	if(should_crash)
	{
		crash();
	}
}
#define COMPILER_STAGE
#define error(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, true, std::make_format_args(__VA_ARGS__))
#define error_ifnt(cond, loc, msg, ...) if(!cond){error(loc, msg, __VA_ARGS__);}

#undef COMPILER_STAGE
#define COMPILER_STAGE argparse
// argument parsing

struct compile_args
{
	bool should_print_help = false;
	std::filesystem::path build_file;
};

compile_args parse_args(std::span<const std::string_view> args)
{
	compile_args ret;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const auto& arg = args[i];
		auto argnext = [allowed = i < args.size() - 1, i, &args](){if(!allowed){error({}, "argument missing value");} return args[i + 1];};

		if(arg == "-v")
		{
			ret.should_print_help = true;
		}
		else
		{
			ret.build_file = arg;
			error_ifnt(std::filesystem::exists(ret.build_file), {}, "could not find build file {}", arg);
		}
	}

	// if user provides no arguments im gonna assume they need some help.
	if(args.empty())
	{
		ret.should_print_help = true;
	}
	return ret;
}

void print_help()
{
	constexpr auto help_string = R"(OVERVIEW: Psy Compiler
USAGE: psyc OPTION... FILE...

OPTION:
	-v	display version info and help

FILE:
	- Can be an absolute path, or a path relative to the current working directory.
	- Files must contain valid .psy source code.
	- Recommended to end in .psy, but does not have to.
	)";
	std::print(help_string);
}

#undef COMPILER_STAGE
#define COMPILER_STAGE lex
// lexer

#undef COMPILER_STAGE
#define COMPILER_STAGE parse
// parser

#undef COMPILER_STAGE
#define COMPILER_STAGE semal
// semal

#undef COMPILER_STAGE
#define COMPILER_STAGE meta
// meta

#undef COMPILER_STAGE
#define COMPILER_STAGE codegen
// codegen llvm-IR

#undef COMPILER_STAGE
#define COMPILER_STAGE assemble
// assemble llvm-IR -> objects

#undef COMPILER_STAGE
#define COMPILER_STAGE link
// link objects -> executable

// entry point

int main(int argc, char** argv)
{
	std::vector<std::string_view> cli_args(argv + 1, argv + argc);
	compile_args args = parse_args(cli_args);
	if(args.should_print_help)
	{
		print_help();
	}

	// todo: build args.build_file
}
