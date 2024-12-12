#include <print>
#include <cstdlib>
#include <source_location>
#include <stacktrace>
#include <span>
#include <filesystem>
#include <format>
#include <fstream>
#include <sstream>
#include <cstdint>
#include <cstddef>

// internals

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

struct slice
{
	std::size_t offset = 0;
	std::size_t length = 0;
};

struct srcloc
{
	std::filesystem::path file;
	unsigned int line;
	unsigned int column;
};

#define panic_ifnt(cond, msg) if(!cond){panic(msg);}

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

// todo: remove this code when c++26 is used.
template <>
struct std::formatter<std::filesystem::path, char> : std::formatter<std::string, char> {
    auto format(const std::filesystem::path& path, std::format_context& ctx) const
	{
        return std::formatter<std::string, char>::format(path.string(), ctx);
    }
};

// user-facing errors

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

//////////////////////////// ARGPARSE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE argparse

struct compile_args
{
	bool should_print_help = false;
	std::filesystem::path build_file = {};
};

compile_args parse_args(std::span<const std::string_view> args)
{
	compile_args ret;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const auto& arg = args[i];
		auto argnext = [allowed = i < args.size() - 1, i, &args](){if(!allowed){error({}, "argument missing value");} return args[i + 1];};

		if(arg == "-h" || arg == "--help")
		{
			ret.should_print_help = true;
		}
		else
		{
			ret.build_file = arg;
			error_ifnt(std::filesystem::exists(ret.build_file), {}, "could not find build file {}", arg);
		}
	}

	if(!ret.should_print_help)
	{
	error_ifnt(!ret.build_file.empty(), {}, "no file specified");
	}
	return ret;
}

void print_help()
{
	constexpr auto help_string = R"(OVERVIEW: Psy Compiler
USAGE: psyc OPTION... FILE

OPTION:
	<empty>
	--help
	-h			display version info and help

FILE:
	- Unlike other compilers, you specify only one file, instead of many.
		- That one source file should act as a build system for your whole project.
	- Can be an absolute path, or a path relative to the current working directory.
	- File must contain valid .psy source code.
	- Recommended to end in .psy, but does not have to.
	)";
	std::print(help_string);
}

//////////////////////////// LEXER -> TOKENS ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE lex

// uh read a file and slurp me all its bytes.
std::string read_file(std::filesystem::path file)
{
	std::ifstream fstr(file);
	error_ifnt(fstr.good(), {}, "failed to read file {}", file);

	std::stringstream buffer;
	buffer >> fstr.rdbuf();
	return buffer.str();
}

// HASH IMPL
constexpr std::uint32_t fnv1a_32(std::string_view str) noexcept {
    constexpr std::uint32_t prime = 0x01000193; // 16777619
    constexpr std::uint32_t offset_basis = 0x811C9DC5; // 2166136261

    std::uint32_t hash = offset_basis;
    for (char c : str) {
        hash ^= static_cast<std::uint8_t>(c);
        hash *= prime;
    }
    return hash;
}
constexpr std::uint32_t string_hash(std::string_view str)
{
	return fnv1a_32(str);
}

enum class token : std::uint32_t
{
	comment = string_hash("//"),
	multicomment = string_hash("/*"),
	iden,
	numlit,
	charlit,
	strlit
};

// when im done lexing i will give the following info to the rest of the compiler
struct lex_output
{
	// what am i lexing
	const std::filesystem::path source_file;

	// the one-true-copy of its source.
	std::string source;
	// list of tokens.
	std::vector<token> tokens = {};
	// lexemes corresponding to each token. slice represents a view into the source (raw bytes) of the file.
	std::vector<slice> lexemes = {};
	// location of each token within the source file.
	std::vector<srcloc> locations = {};
};

// internal lexer state, not exposed to rest of compiler.
struct lex_state
{
	std::string_view src;
	std::size_t cursor = 0;
	std::size_t line = 0;
	std::size_t column = 0;

	// move forward 'count' chars, make sure to keep line count sane. panic if we run out of space.
	void advance(std::size_t count = 1)
	{
		panic_ifnt(this->cursor + count < src.size(), "advance ran over string capacity");
		for(std::size_t i = 0; i < count; i++)
		{
			char next = src[++this->cursor];
			if(next == '\n')
			{
				this->line++;
				this->column = 0;
			}
			else
			{
				this->column++;
			}
		}
	}

	// simple internal helper. move forward n times where n is the length of the string.
	void advance(const char* str)
	{
		this->advance(std::strlen(str));
	}

	// slower but useful. keeping moving forward until a predicate is satisfied for the first time.
	// return the number of chars advanced. panic if predicate never returns true.
	std::size_t advance_until(bool(*pred)(std::string_view))
	{
		std::string_view src_cpy = this->src;
		std::size_t dst = 0;
		// find how long we have to go until predicate first returns true
		while(!pred(src_cpy))
		{
			dst++;
			panic_ifnt(this->cursor + dst < src.size(), "advance_until(pred) ran out of source file before predicate returned true");
			src_cpy.remove_prefix(1);
		}

		// advance that amount, and return that amount.
		this->advance(dst);
		return dst;
	}
};

// lex api. "heres a file i know nothing about, give me all the tokens". panic if anything goes wrong.
lex_output lex(std::filesystem::path file)
{
	lex_output ret{.source_file = file};
	ret.source = read_file(file);
	lex_state state{.src = ret.source};

	while(state.cursor < state.src.size())
	{
		// note: dont pass just cstr to ctor as it will search for \0 every single time.
		std::string_view front = {ret.source.data() + state.cursor, ret.source.size() - state.cursor};
		// todo: try to match this to every known token, and store it in lex output and advance.
	}

	return ret;
}

//////////////////////////// PARSE -> AST ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE parse

//////////////////////////// SEMAL ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE semal

//////////////////////////// META ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE meta

//////////////////////////// CODEGEN -> LLVM-IR ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE codegen

//////////////////////////// ASSEMBLE -> OBJECTS ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE assemble

//////////////////////////// LINK -> EXECUTABLE ////////////////////////////
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
	if(args.build_file == std::filesystem::path{})
	{
		return 0;
	}

	lex_output build_file_lex = lex(args.build_file);
}
