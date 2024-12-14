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
#include <array>
#include <charconv>

// internals

void crash()
{
	asm volatile("int3");
	std::exit(-1);
}

void generic_panic(const char* msg, std::source_location loc, std::stacktrace trace, std::format_args&& args)
{
	std::print("\033[1;31minternal compiler error: {}\n\tat {}({}:{}) `{}`\nstacktrace:\n==========\n{}\n=========\033[0m", std::vformat(msg, args), loc.file_name(), loc.line(), loc.column(), loc.function_name(), std::to_string(trace));
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

#define panic(msg, ...) generic_panic(msg, std::source_location::current(), std::stacktrace::current(), std::make_format_args(__VA_ARGS__))
#define panic_ifnt(cond, msg, ...) if(!(cond)){panic(msg, __VA_ARGS__);}

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
#define error_ifnt(cond, loc, msg, ...) if(!(cond)){error(loc, msg, __VA_ARGS__);}

//////////////////////////// ARGPARSE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE argparse

struct compile_args
{
	bool should_print_help = false;
	bool verbose_lex = false;
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
		else if(arg == "--verbose-lex")
		{
			ret.verbose_lex = true;
		}
		else if(arg == "--verbose-all")
		{
			ret.verbose_lex = true;
		}
		else
		{
			if(arg.starts_with("-"))
			{
				error({}, "unknown option {}", arg);
			}
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
	-h				display version info and help

	--verbose-all	output *all* compiler meta information (for debugging purposes)
	--verbose-lex	output lexer meta information (print all tokens)

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
	buffer << fstr.rdbuf();
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

enum class token : std::uint32_t;
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
	// begin location of each token within the source file.
	std::vector<srcloc> begin_locations = {};
	// end location of each token within the source file.
	std::vector<srcloc> end_locations = {};

	void verbose_print();
};

// internal lexer state, not exposed to rest of compiler.
struct lex_state
{
	std::string_view src;
	std::size_t cursor = 0;
	unsigned int line = 1;
	unsigned int column = 1;

	// move forward 'count' chars, make sure to keep line count sane. panic if we run out of space.
	void advance(std::size_t count = 1)
	{
		panic_ifnt(this->cursor + count <= src.size(), "advance ran over string capacity");
		for(std::size_t i = 0; i < count; i++)
		{
			char next = src[this->cursor++];
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
		std::string_view current = this->src;
		current.remove_prefix(this->cursor);
		std::size_t dst = 0;
		// find how long we have to go until predicate first returns true
		while(!pred(current))
		{
			dst++;
			std::size_t capacity = src.size();
			panic_ifnt(this->cursor + dst <= capacity, "advance_until(pred) ran out of source file before predicate returned true");
			current.remove_prefix(1);
		}

		// advance that amount, and return that amount.
		this->advance(dst);
		return dst;
	}
};

// below is core tokenisation logic. dont skip over this text.
// the enum class token represents all token types. for each token type, the token_traits array has a member that contains the logic for how to parse it and its side effects.
//
// so based upon some source code, how do we identify which token type it is?
// most of the time, you can compare the front of the source code with some special set of characters
//
// e.g comments always start with "//"
// so token_traits[token::comment].front_identifier = "//" and the lexer api will automatically match it
// if front_identifier == nullptr for a certain token type then it is assumed that the match logic is not trivial.
//
// there is where the tokenise_fn (fn) comes in. the tokenise function will take in the current point in the source code and return true if it matches.
// note: even if you do set a front_identifier, the tokenise function's return value is ignored BUT the side-effect of it should properly deal with adding the token data.
// however, there is a special "trivial" flag you can raise for a common case. if your token type is trivial (i.e no lexeme value, just match against the front identifier and dont do anything special) then set trivial to true.
// if trivial is true, then the tokenise_fn is ignored completely and can be nullptr. it will just match and add the token with an empty lexeme (known in the codebase as trivial lexing)
enum class token : std::uint32_t
{
	comment,
	multicomment,
	integer_literal,
	decimal_literal,
	char_literal,
	string_literal,
	symbol,
	initialiser,
	colon,
	compare,
	assign,
	arrow,
	oparen,
	cparen,
	obrace,
	cbrace,
	obrack,
	cbrack,
	oanglebrack,
	canglebrack,
	keyword_if,
	keyword_while,
	keyword_for,
	keyword_return,
	_count
};
using tokenise_fn = bool(*)(std::string_view, lex_state&, lex_output&);
struct tokeniser
{
	const char* name = "<untitled token>";
	const char* front_identifier = nullptr;
	tokenise_fn fn = nullptr;
	bool trivial = false;
};
std::array<tokeniser, static_cast<int>(token::_count)> token_traits
{
	tokeniser
	{
		.name = "comment",
		.front_identifier = "//",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			// rest of the line is ignored.
			std::size_t comment_begin = state.cursor + std::strlen("//");
			std::size_t comment_length = state.advance_until([](std::string_view next){return next.starts_with("\n");});
			out.tokens.push_back(token::comment);
			out.lexemes.push_back({.offset = comment_begin, .length = comment_length - 2});
			return true;
		},
	},

	tokeniser
	{
		.name = "multicomment",
		.front_identifier = "/*",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			std::size_t comment_begin = state.cursor + std::strlen("/*");
			std::size_t comment_length = state.advance_until([](std::string_view next){return next.starts_with("*/");});
			state.advance("*/");
			out.tokens.push_back(token::multicomment);
			out.lexemes.push_back({.offset = comment_begin, .length = comment_length - 2});
			return false;
		},
	},

	tokeniser
	{
		.name = "integer literal",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			std::int64_t val;
			std::from_chars_result result;
			if(front.starts_with("0b"))
			{
				// binary (base 2) literal
				result = std::from_chars(front.data() + 2, front.data() + front.size(), val, 2);
			}
			else if(front.starts_with("0x"))
			{
				// hex (base 16) literal
				result = std::from_chars(front.data() + 2, front.data() + front.size(), val, 16);
			}
			else
			{
				// base 10 integer literal
				result = std::from_chars(front.data(), front.data() + front.size(), val, 10);
			}
			if(result.ec == std::errc() && result.ptr != front.data() && *result.ptr != '.')
			{
				// yes we successfully parsed some numbers.
				std::size_t parse_count = result.ptr - front.data();
				std::size_t cursor_before = state.cursor;

				state.advance(parse_count);
				out.tokens.push_back(token::integer_literal);
				out.lexemes.push_back({.offset = cursor_before, .length = parse_count});
				return true;
			}
			return false;
		},
	},

	tokeniser
	{
		.name = "decimal literal",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			double val;
			auto result = std::from_chars(front.data(), front.data() + front.size(), val);
			if(result.ec == std::errc() && result.ptr != front.data())
			{
				std::size_t parse_count = result.ptr - front.data();
				std::size_t cursor_before = state.cursor;
				state.advance(parse_count);
				out.tokens.push_back(token::decimal_literal);
				out.lexemes.push_back({.offset = cursor_before, .length = parse_count});
				return true;
			}
			return false;
		},
	},

	tokeniser
	{
		.name = "char literal",
		.front_identifier = "\'",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			srcloc cur{.file = out.source_file, .line = static_cast<unsigned int>(state.line), .column = static_cast<unsigned int>(state.column)};
			std::size_t char_begin = state.cursor + 1;
			// careful - advance_until could easily get the same quote as the front, so we nudge the cursor forward once
			state.advance(1);
			std::size_t char_length = state.advance_until([](std::string_view next){return next.starts_with("\'");});
			if(state.cursor < state.src.size())
			{
				state.advance(1);
			}
			out.tokens.push_back(token::char_literal);
			out.lexemes.push_back({.offset = char_begin, .length = char_length});
			return false;
		},
	},

	tokeniser
	{
		.name = "string literal",
		.front_identifier = "\"",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			std::size_t string_begin = state.cursor + 1;
			// careful - advance_until could easily get the same quote as the front, so we nudge the cursor forward once
			state.advance(1);
			std::size_t string_length = state.advance_until([](std::string_view next){return next.starts_with("\"");});
			if(state.cursor < state.src.size())
			{
				state.advance(1);
			}
			out.tokens.push_back(token::string_literal);
			out.lexemes.push_back({.offset = string_begin, .length = string_length});
			return false;
		},
	},

	tokeniser
	{
		.name = "symbol",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool
		{
			// symbol can start with a letter or _, but not a number
			if(std::isalpha(front.front()) || front.front() == '_')
			{
				// however after the first char a symbol can contain a number
				std::size_t symbol_begin = state.cursor;
				std::size_t symbol_length = state.advance_until([](std::string_view next){return !(std::isalnum(next.front()) || next.front() == '_');});
				out.tokens.push_back(token::symbol);
				out.lexemes.push_back({.offset = symbol_begin, .length = symbol_length});
				return true;
			}
			return false;
		},
	},

	tokeniser
	{
		.name = "initialiser",
		.front_identifier = ":=",
		.trivial = true
	},

	tokeniser
	{
		.name = "colon",
		.front_identifier = ":",
		.trivial = true
	},

	tokeniser
	{
		.name = "compare",
		.front_identifier = "==",
		.trivial = true
	},

	tokeniser
	{
		.name = "assign",
		.front_identifier = "=",
		.trivial = true
	},

	tokeniser
	{
		.name = "arrow",
		.front_identifier = "->",
		.trivial = true
	},

	tokeniser
	{
		.name = "oparen",
		.front_identifier = "(",
		.trivial = true
	},

	tokeniser
	{
		.name = "cparen",
		.front_identifier = ")",
		.trivial = true
	},

	tokeniser
	{
		.name = "obrace",
		.front_identifier = "{",
		.trivial = true
	},

	tokeniser
	{
		.name = "cbrace",
		.front_identifier = "}",
		.trivial = true
	},

	tokeniser
	{
		.name = "obrack",
		.front_identifier = "[",
		.trivial = true
	},

	tokeniser
	{
		.name = "cbrack",
		.front_identifier = "]",
		.trivial = true
	},

	tokeniser
	{
		.name = "oanglebrack",
		.front_identifier = "<",
		.trivial = true
	},

	tokeniser
	{
		.name = "canglebrack",
		.front_identifier = ">",
		.trivial = true
	},

	tokeniser
	{
		.name = "if keyword",
		.front_identifier = "if",
		.trivial = true
	},

	tokeniser
	{
		.name = "while keyword",
		.front_identifier = "while",
		.trivial = true
	},

	tokeniser
	{
		.name = "for keyword",
		.front_identifier = "for",
		.trivial = true
	},

	tokeniser
	{
		.name = "return keyword",
		.front_identifier = "return",
		.trivial = true
	},
};

// given a given token trait, try to tokenise the current bit of the source code
// return true if the token matched and the output was updated
// return false (and doesnt touch the output) if the current part of the source code is definitely not this token.
bool try_tokenise(std::string_view front, token tok, lex_output& out, lex_state& state)
{
	const tokeniser& trait = token_traits[static_cast<int>(tok)];
	if(trait.trivial)
	{
		// do trivial lexing
		if(front.starts_with(trait.front_identifier))
		{
			std::size_t cursor_before = state.cursor;
			state.advance(trait.front_identifier);
			out.tokens.push_back(tok);
			out.lexemes.push_back({.offset = cursor_before, .length = std::strlen(trait.front_identifier)});
			return true;
		}
	}
	else
	{
		// ok not going to be that simple
		// firstly we better have a trait function
		if(trait.fn == nullptr)
		{
			const char* description = trait.front_identifier;
			if(description == nullptr)
			{
				description = "<unknown token trait>";
			}
			panic("non-trivial token trait \"{}\" had a nullptr tokenise function, which is a lexer bug.", description);
		}
		// is there still a front identifier?
		if(trait.front_identifier != nullptr)
		{
			if(front.starts_with(trait.front_identifier))
			{
				// ok, run the tokeniser function but ignore the result.
				// assume the side-effects of the function have successfully written to the output. we're done.
				trait.fn(front, state, out);
				return true;
			}
			// otherwise move on and try the next token type.
			return false;
		}
		else
		{
			// no front identifier, so we purely rely on the trait function.
			return trait.fn(front, state, out);
		}
	}
	return false;
}

std::size_t skip_over_whitespace(std::string_view front)
{
	if(std::isspace(front.front()))
	{
		return 1;
	}
	return false;
}


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
		// is this whitespace we just skip over?
		auto whitespace = skip_over_whitespace(front);
		if(whitespace > 0)
		{
			// go again.
			state.advance(whitespace);
			continue;
		}

		srcloc loc{.file = file, .line = state.line, .column = state.column + 1};
		// todo: try to match this to every known token, and store it in lex output and advance.
		bool found_a_token = false;
		for(int i = 0; i < static_cast<int>(token::_count); i++)
		{
			if(try_tokenise(front, static_cast<token>(i), ret, state))
			{
				// ok we succesfully found a token. save the location of the token and then break out of the loop.
				ret.begin_locations.push_back(loc);
				ret.end_locations.push_back({.file = file, .line = state.line, .column = state.column + 1});
				found_a_token = true;
				break;
			}
		}
		// none of the tokens worked.
		// need to error out
		// let's not use the whole source as the snippet, just a hardcoded limit
		if(!found_a_token)
		{
			int cutoff_length = 32;
			int first_semicol_pos = front.find(';');
			if(first_semicol_pos != std::string_view::npos)
			{
				cutoff_length = std::min(cutoff_length, first_semicol_pos - 1);
			}
			if(front.size() > cutoff_length)
			{
				front.remove_suffix(front.size() - cutoff_length);
			}
			error(loc, "invalid tokens {}: \"{}\"", loc, front);
		}
	}

	return ret;
}

void lex_output::verbose_print()
{
	// print all tokens.
	for(std::size_t i = 0; i < tokens.size(); i++)
	{
		token t = this->tokens[i];

		slice lex_slice = this->lexemes[i];
		std::string_view lexeme {this->source.data() + lex_slice.offset, lex_slice.length};
		srcloc begin_loc = this->begin_locations[i];
		srcloc end_loc = this->end_locations[i];

		std::println("{} ({}) at {}({}:{} -> {}:{})", token_traits[static_cast<int>(t)].name, lexeme, begin_loc.file, begin_loc.line, begin_loc.column, end_loc.line, end_loc.column);
	}
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
	if(args.verbose_lex)
	{
		build_file_lex.verbose_print();
	}
}
