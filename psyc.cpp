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
#include <variant>
#include <unordered_map>
#include <ranges>
#include <functional>
#include <chrono>

#define STRINGIFY(...) #__VA_ARGS__

// internals

std::chrono::time_point<std::chrono::system_clock> now;
void timer_restart()
{
	now = std::chrono::system_clock::now();
}

std::uint64_t elapsed_time()
{
	auto right_now = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
	return right_now - std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count();
}

void crash()
{
	std::printf("\033[1;31mcompilation aborted due to error\033[0m");
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
	std::size_t cursor;
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

std::string_view quote_source(std::string_view source_code, srcloc begin_loc, srcloc end_loc)
{
	panic_ifnt(begin_loc.file == end_loc.file, "attempted to quote source where begin and end locations are in different files (begin {}, end {})", begin_loc, end_loc);
	return {source_code.data() + begin_loc.cursor, end_loc.cursor - begin_loc.cursor};
}

std::string format_source(std::string_view source_code, srcloc begin_loc, srcloc end_loc)
{
	const char* begin_point = source_code.data() + begin_loc.cursor;
	std::size_t len = end_loc.cursor - begin_loc.cursor;

	std::string_view first_part{source_code.data(), begin_point};
	auto closest_previous_newline = std::ranges::find(first_part | std::views::reverse, '\n');
	std::size_t dst_from_prev_newline = 0;

	// the begin location will start on some line. let's make sure we have that whole line.
	if(closest_previous_newline != first_part.rend())
	{
		// there is a newline beforehand. lets add the beginning part of the line.
		begin_point = source_code.data() + std::distance(closest_previous_newline, first_part.rend());
		dst_from_prev_newline = (source_code.data() + begin_loc.cursor) - *&begin_point;
		len += dst_from_prev_newline;
	}

	// same with the end location.
	std::string_view next_part{source_code.data() + end_loc.cursor - 1, source_code.data() + source_code.size() - 1};
	auto next_newline = std::ranges::find(next_part, '\n');
	if(next_newline != next_part.end())
	{
		std::size_t dst_to_next_newline = std::distance(next_part.begin(), next_newline);
		len += dst_to_next_newline;
	}

	std::string_view whole_line{begin_point, len};
	// split the source snippet into lines, return them in a formatted manner.
	// also underline the target line and point to the exact start of the source.
	std::size_t line = begin_loc.line;
	std::stringstream s(std::string{whole_line});
	std::string ret;
	std::size_t i = 0;
	for(std::string cur_line; std::getline(s, cur_line);)
	{
		std::size_t curlinenum = line + i++;
		ret += std::format("\t{} | {}\n", curlinenum, cur_line);
		if(i == 1)
		{
			// add an underline
			std::size_t line_char_count = std::to_string(curlinenum).size();
			std::string underline = "\t";
			for(std::size_t j = 0; j < line_char_count + dst_from_prev_newline; j++)
			{
				underline += ' ';
			}
			underline += "   ^\n";
			ret += underline;
		}
	}
	return ret;
}

	std::string escape(std::string_view literal)
	{
		std::string ret;
		static const std::unordered_map<std::string_view, char> escape_map = {
			{"\\0", '\0'}, // Null terminator
			{"\\a", '\a'}, // Bell (alert)
			{"\\b", '\b'}, // Backspace
			{"\\f", '\f'}, // Formfeed
			{"\\n", '\n'}, // Newline (line feed)
			{"\\r", '\r'}, // Carriage return
			{"\\t", '\t'}, // Horizontal tab
			{"\\v", '\v'}, // Vertical tab
			{"\\\\", '\\'}, // Backslash
			{"\\'", '\''}, // Single quote
			{"\\\"", '\"'}, // Double quote
			{"\\?", '\?'}  // Question mark
		};
		if(literal.size() == 1)
		{
			return std::string{literal};
		}
		for(std::size_t i = 0; i < literal.size(); i++)
		{
			std::string_view substr{literal.data() + i, 2};
			auto iter = escape_map.find(substr);
			if(iter != escape_map.end())
			{
				ret += iter->second;
				i++;
			}
			else
			{
				ret += literal[i];
			}
		}
		return ret;
	}

// user-facing errors

enum class err
{
	argparse,
	lex,
	parse,
	semal,
	type,
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
	"type",
	"meta program",
	"code generation",
	"assembly",
	"linker",
	"unknown",
};

void generic_error(err ty, const char* msg, srcloc where, bool should_crash, std::format_args&& args)
{
	std::println("\033[1;31m{} error {}\033[0m: {}", err_names[static_cast<int>(ty)], where, std::vformat(msg, args));
	
	if(should_crash)
	{
		crash();
	}
}

void generic_warning(err ty, const char* msg, srcloc where, std::format_args&& args)
{
	std::println("\033[1;33m{} warning {}\033[0m: {}", err_names[static_cast<int>(ty)], where, std::vformat(msg, args));
}

#define COMPILER_STAGE
#define error(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, true, std::make_format_args(__VA_ARGS__))
#define warning(loc, msg, ...) generic_warning(err::COMPILER_STAGE, msg, loc, std::make_format_args(__VA_ARGS__))
#define error_nonblocking(loc, msg, ...) generic_error(err::COMPILER_STAGE, msg, loc, false, std::make_format_args(__VA_ARGS__))
#define error_ifnt(cond, loc, msg, ...) if(!(cond)){error(loc, msg, __VA_ARGS__);}

//////////////////////////// ARGPARSE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE argparse

struct compile_args
{
	bool should_print_help = false;
	bool verbose_lex = false;
	bool verbose_ast = false;
	bool verbose_parse = false;
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
		else if(arg == "--verbose-ast")
		{
			ret.verbose_ast = true;
		}
		else if(arg == "--verbose-parse")
		{
			ret.verbose_parse = true;
		}
		else if(arg == "--verbose-all")
		{
			ret.verbose_lex = true;
			ret.verbose_ast = true;
			ret.verbose_parse = true;
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

#define deduced_type "auto"

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
	void strip_tokens_that_dont_affect_code();
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
	semicol,
	initialiser,
	colon,
	comma,
	dot,
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
	keyword_func,
	keyword_extern,
	keyword_struct,
	symbol,
	end_of_file,
	_count
};
using tokenise_fn = bool(*)(std::string_view, lex_state&, lex_output&);
struct tokeniser
{
	const char* name = "<untitled token>";
	const char* front_identifier = nullptr;
	tokenise_fn fn = nullptr;
	bool trivial = false;
	bool affects_code = true;
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
		.affects_code = false
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
		.affects_code = false
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
		.name = "semicol",
		.front_identifier = ";",
		.trivial = true
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
		.name = "comma",
		.front_identifier = ",",
		.trivial = true
	},

	tokeniser
	{
		.name = "dot",
		.front_identifier = ".",
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

	tokeniser
	{
		.name = "func keyword",
		.front_identifier = "func",
		.trivial = true
	},

	tokeniser
	{
		.name = "extern keyword",
		.front_identifier = "extern",
		.trivial = true
	},

	tokeniser
	{
		.name = "struct keyword",
		.front_identifier = "struct",
		.trivial = true
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
		.name = "eof",
		.fn = [](std::string_view front, lex_state& state, lex_output& out)->bool{return false;}
	}
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

std::int64_t token_hash(token t)
{
	return std::hash<int>{}(static_cast<int>(t));
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

		srcloc loc{.file = file, .line = state.line, .column = state.column + 1, .cursor = state.cursor};
		// todo: try to match this to every known token, and store it in lex output and advance.
		bool found_a_token = false;
		for(int i = 0; i < static_cast<int>(token::_count); i++)
		{
			if(try_tokenise(front, static_cast<token>(i), ret, state))
			{
				// ok we succesfully found a token. save the location of the token and then break out of the loop.
				ret.begin_locations.push_back(loc);
				ret.end_locations.push_back({.file = file, .line = state.line, .column = state.column + 1, .cursor = state.cursor});
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

	ret.strip_tokens_that_dont_affect_code();
	ret.tokens.push_back(token::end_of_file);
	ret.lexemes.push_back({});
	ret.begin_locations.push_back(ret.begin_locations.size() ? ret.begin_locations.front() : srcloc{});
	ret.end_locations.push_back(ret.end_locations.size() ? ret.end_locations.back() : srcloc{});
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

void lex_output::strip_tokens_that_dont_affect_code()
{
	for(std::size_t i = 0; i < tokens.size();)
	{
		tokeniser trait = token_traits[static_cast<int>(tokens[i])];
		if(!trait.affects_code)
		{
			this->tokens.erase(this->tokens.begin() + i);
			this->lexemes.erase(this->lexemes.begin() + i);
			this->begin_locations.erase(this->begin_locations.begin() + i);
			this->end_locations.erase(this->end_locations.begin() + i);
		}
		else
		{
			i++;
		}
	}
}

//////////////////////////// PARSE -> AST ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE parse

struct ast_token
{
	token tok;
	std::string_view lexeme;

	std::string value_tostring() const
	{
		return std::string{lexeme};
	}
};

struct ast_translation_unit{std::string value_tostring(){return "";}};

struct ast_literal_expr
{
	std::variant
	<
		std::int64_t, double, char, std::string
	> value;
	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(value)>>
		{
			"integer literal",
			"floating-point literal",
			"char literal",
			"string literal"
		}[this->value.index()];
	}

	std::string value_tostring() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
				{
					ret = std::format("{}", arg);
				}, this->value);
		return ret;
	}
};

struct ast_decl;

struct ast_funcdef_expr
{
	std::vector<ast_decl> static_params = {};
	std::vector<ast_decl> params = {};
	std::string return_type;
	bool is_extern = false;

	std::string value_tostring() const
	{
		if(this->is_extern)
		{
			return "extern func";
		}
		return "func";
	}
};

struct ast_expr;

struct ast_partial_callfunc
{
	std::string function_name;
	std::vector<ast_expr> static_params = {};
	std::vector<ast_expr> params = {};
	bool on_static_params = false;
	bool awaiting_next_param = false;

	std::string value_tostring() const
	{
		return "partial call";
	}
};

struct ast_callfunc_expr
{
	std::string function_name;
	std::vector<ast_expr> static_params = {};
	std::vector<ast_expr> params = {};

	std::string value_tostring() const;
};

struct ast_symbol_expr
{
	std::string symbol;

	std::string value_tostring() const
	{
		return "symbol";
	}
};

struct ast_structdef_expr
{
	std::string value_tostring() const
	{
		return "structdef";
	}
};

struct ast_expr
{
	std::variant
	<
		ast_literal_expr,
		ast_funcdef_expr,
		ast_callfunc_expr,
		ast_symbol_expr,
		ast_structdef_expr
	> expr_;

	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(expr_)>>
		{
			"literal",
			"callfunc",
			"symbol"
		}[this->expr_.index()];
	}

	std::string value_tostring() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
				{
					ret = arg.value_tostring();
				}, this->expr_);
		return ret;
	}
};


std::string ast_callfunc_expr::value_tostring() const
{
	return std::format("call<{}>", params.size());
}

struct ast_decl
{
	std::string type_name;
	std::string name;
	std::optional<ast_expr> initialiser = std::nullopt;

	std::string value_tostring() const
	{
		if(this->initialiser.has_value())
		{
			return this->initialiser->value_tostring();
		}
		return "<no initialiser>";
	}
};

struct ast_decl_stmt
{
	ast_decl decl;

	std::string value_tostring()
	{
		return this->decl.value_tostring();
	}
};

struct ast_expr_stmt
{
	ast_expr expr;

	std::string value_tostring()
	{
		return this->expr.value_tostring();
	}
};

struct ast_stmt;
struct ast_blk_stmt
{
	bool capped = false;

	std::string value_tostring(){return "";}
};

struct ast_metaregion_stmt
{
	std::string name;
	bool capped = false;

	std::string value_tostring(){return std::format("{} meta-region", this->name);}
};

struct ast_stmt
{
	std::variant
	<
		ast_decl_stmt,
		ast_expr_stmt,
		ast_blk_stmt,
		ast_metaregion_stmt
	> stmt_;
	const char* type_name() const
	{
		return std::array<const char*, std::variant_size_v<decltype(stmt_)>>
		{
			"declaration",
			"expression",
			"block",
			"metaregion"
		}[this->stmt_.index()];
	}
	std::string value_tostring() const
	{
		std::string ret;
		std::visit([&ret](auto&& arg)
				{
					using T = std::decay_t<decltype(arg)>;
					ret = const_cast<T&>(arg).value_tostring();
				}, this->stmt_);
		return ret;
	}
};

enum class partial_funcdef_stage
{
	defining_static_params,
	awaiting_next_static_param,
	defining_params,
	awaiting_next_param,
	awaiting_arrow,
	awaiting_return_type,
	awaiting_body
};

struct ast_partial_funcdef
{
	std::vector<ast_decl> static_params = {};
	std::vector<ast_decl> params = {};
	std::string return_type;
	partial_funcdef_stage stage = partial_funcdef_stage::defining_params;

	std::string value_tostring() const{return "";}
};

struct ast_funcdef
{
	ast_funcdef_expr func;

	std::string value_tostring() const
	{
		return func.value_tostring();
	}
};

using node_payload = std::variant
<
	std::monostate,
	ast_token,
	ast_translation_unit,
	ast_expr,
	ast_partial_callfunc,
	ast_decl,
	ast_stmt,
	ast_partial_funcdef,
	ast_funcdef
>;
std::array<const char*, std::variant_size_v<node_payload>> node_names
{
	"<error>",
	"_unparsed_token",
	"translation_unit",
	"expression",
	"partial function call",
	"declaration",
	"statement",
	"partial function definition",
	"function definition"
};

template<typename T, typename V = node_payload, std::size_t index_leave_blank = 0>
consteval int payload_index()
{
	static_assert(index_leave_blank < std::variant_size_v<V>, "unknown payload index type");
	if constexpr(std::is_same_v<std::decay_t<T>, std::decay_t<std::variant_alternative_t<index_leave_blank, V>>>)
	{
		return index_leave_blank;
	}
	else
	{
		return payload_index<T, V, index_leave_blank + 1>();
	}
}

struct node
{
	// payload, data varies depending on what type of node we are.
	node_payload payload = std::monostate{};
	std::vector<node> children = {};
	// where does this node begin in the source code
	srcloc begin_location = {};
	// where does the node end in the source code
	srcloc end_location = {};
	// variable that xor's with the resultant hash. useful if you want nodes of the same payload type to still vary in their hash.
	int hash_morph = 0;

	constexpr std::int64_t hash() const
	{
		return std::hash<std::size_t>{}(this->payload.index()) ^ std::hash<int>{}(this->hash_morph);
	}

	bool is_null() const
	{
		return payload.index() == payload_index<std::monostate>();
	}

	void verbose_print(std::string_view full_source, std::string prefix = "") const
	{
		std::string extra = "";
		if(this->payload.index() == payload_index<ast_token>())
		{
			extra = std::format("<{}>", token_traits[static_cast<int>(std::get<ast_token>(this->payload).tok)].name);
		}
		else if(this->payload.index() == payload_index<ast_stmt>())
		{
			extra = std::format("({})", std::get<ast_stmt>(this->payload).type_name());
		}
		std::string value_as_string;
		std::visit([&value_as_string](auto&& arg)
				{
					using T = std::decay_t<decltype(arg)>;
					if constexpr(!std::is_same_v<T, std::monostate>)
					{
						value_as_string = const_cast<T&>(arg).value_tostring();
					}
				}, this->payload);
		std::println("{}{}{} {} [{}, {}] - [{}, {}]", prefix, node_names[this->payload.index()], extra, value_as_string, this->begin_location.line, this->begin_location.column, this->end_location.line, this->end_location.column);
		for(const auto& child : this->children)
		{
			child.verbose_print(full_source, prefix + "\t");
		}
	}

	// when shifting (grabbing a token from the lexer, that is uh not an ast node so how do we add it to the parse stack?
	// easy! use this to wrap it up and add this to the parse stack instead.
	static node wrap_token(const lex_output& lex, std::size_t idx)
	{
		slice s = lex.lexemes[idx];
		token tok = lex.tokens[idx];
		return node
		{
			.payload = ast_token
			{
				.tok = tok,
				.lexeme = {lex.source.data() + s.offset, s.length}
			},
			.begin_location = lex.begin_locations[idx],
			.end_location = lex.end_locations[idx],
			// remember, ast tokens of different types (e.g colon and comments) should return different hashes, or chords can't select a specific token type.
			.hash_morph = static_cast<int>(tok)
		};
	}

	static node wildcard()
	{
		return {};
	}

	static node wrap_imaginary_token(token tok)
	{
		// this node should never be put into an AST, but used as a way to create a node hash based upon an arbitrary token.
		return node
		{
			.payload = ast_token
			{
				.tok = tok
			},
			.hash_morph = static_cast<int>(tok)
		};
	}
};

std::int64_t wildcard_hash()
{
	return node::wildcard().hash();
}

template<std::size_t Begin, std::size_t End>
constexpr void static_for(std::function<void(std::size_t)> function)
{
	if constexpr(Begin < End)
	{
		function(std::integral_constant<std::size_t, Begin>{});
		static_for<Begin + 1, End>(function);
	}
}

void iterate_all_hashes(std::function<void(std::int64_t)> function)
{
	// invoke function(hash) for every single possible hash of a token/node that exists. implementation of chord wildcards.
	auto lambda = [&function]<int B, int E>(auto& lambda_ref)constexpr
	{
		using T = std::variant_alternative_t<B, node_payload>;
		if constexpr(std::is_same_v<T, ast_token>)
		{
			for(int i = 0; i < static_cast<int>(token::_count); i++)
			{
				function(node::wrap_imaginary_token(static_cast<token>(i)).hash());
			}
		}
		else
		{
			function(node{.payload = T{}}.hash());
		}
		if constexpr(B < E - 1)
		{
			lambda_ref.template operator()<B + 1, E>(lambda_ref);
		}
	};
	lambda.template operator()<0, std::variant_size_v<node_payload>>(lambda);
}

// whenever a chord function is invoked, it should return some kind of hint as to what happens next.
enum class parse_action
{
	// i couldn't make a decision, i need another token
	shift,
	// i have performed a reduction on the parse stack.
	reduce,
	// i think you should attempt to reduce again, but offset the nodes by reduction_result_offset.
	recurse,
	// the chord function has reported a parse error, and the parser state is no longer trustworthy.
	error,
	// the reduction results should be set as children of the final AST node. 
	commit,
};
struct chord_result
{
	parse_action action = parse_action::error;
	slice nodes_to_remove = {};
	std::size_t reduction_result_offset = 0;
	std::vector<node> reduction_result = {};
};

struct parser_state;
using chord_function = chord_result(*)(std::span<node> subtrees, parser_state& state);

struct parse_table_entry
{
	chord_function chord_fn = nullptr;
	const char* description = "";
	bool extensible = false;
	std::unordered_map<std::int64_t, parse_table_entry> children = {};
};
std::unordered_map<std::int64_t, parse_table_entry> parse_table = {};

using entry_fn = std::function<void(parse_table_entry&)>;
void foreach_entry_from_hashed_subtrees(std::span<const node> subtrees, entry_fn fn, parse_table_entry* base = nullptr, bool skip_extensible = false)
{
	if(base == nullptr)
	{
		panic_ifnt(subtrees.size() > 0, "attempted to find empty from hashes of a set of subtrees, but the set of subtrees was empty.");
		std::int64_t first_hash = subtrees.front().hash();
		panic_ifnt(first_hash != wildcard_hash(), "the first token/node in a chord state must *not* be a wildcard");
		subtrees = subtrees.subspan<1>();
		base = &parse_table[first_hash];
	}
	std::span<const node> subtree_cpy = subtrees;
	for(const node& n : subtrees)
	{
		std::int64_t hash = n.hash();
		if(!skip_extensible && base->extensible)
		{
			break;
		}
		if(hash == wildcard_hash())
		{
			// for *EVERY* possible hash, recurse.
			iterate_all_hashes([subtree_cpy, &fn, base, skip_extensible](std::int64_t hash)
			{
				auto& child = base->children[hash];	
				foreach_entry_from_hashed_subtrees(subtree_cpy.subspan<1>(), fn, &child, skip_extensible);
			});
			return;
		}
		else
		{
			base = &base->children[hash];
		}
		subtree_cpy = subtree_cpy.subspan<1>();
	}
	fn(*base);
}
/*
parse_table_entry& find_entry_from_hashed_subtrees(std::span<const node> subtrees)
{
	panic_ifnt(subtrees.size() > 0, "attempted to find empty from hashes of a set of subtrees, but the set of subtrees was empty.");
	std::int64_t first_hash = subtrees.front().hash();
	subtrees = subtrees.subspan<1>();
	parse_table_entry& entry = parse_table[first_hash];
	for(const node& n : subtrees)
	{
		entry = entry.children[n.hash()];
	}
	return entry;
}
*/

void add_chord(std::span<const node> subtrees, const char* description, chord_function fn, bool extensible = false)
{
	bool any_wildcards = false;
	for(const node& n : subtrees)
	{
		if(n.hash() == wildcard_hash())
		{
			any_wildcards = true;
		}
	}
	foreach_entry_from_hashed_subtrees(subtrees, [fn, description, any_wildcards, extensible](parse_table_entry& entry)
	{
		if(any_wildcards)
		{
			if(entry.chord_fn == nullptr)
			{
				entry.chord_fn = fn;
				entry.description = description;
				entry.extensible = extensible;
			}
		}
		else
		{
			panic_ifnt(entry.chord_fn == nullptr || entry.chord_fn == fn, "redefinition of chord function");
			entry.chord_fn = fn;
			entry.description = description;
			entry.extensible = extensible;
		}
	}, nullptr, true);
}

#define CHORD_BEGIN add_chord(
#define CHORD_END );
#define EXTENSIBLE , true

#define TOKEN(x) node::wrap_imaginary_token(token::x)
#define NODE(x) node{.payload = x{}}
#define WILDCARD node::wildcard()
#define FN [](std::span<node> nodes, parser_state& state)->chord_result
#define STATE(...) [](){return std::array{node{.payload = ast_translation_unit{}}, __VA_ARGS__};}(), "translation_unit, " STRINGIFY(__VA_ARGS__)
// the difference between STATE and LOOSE state is that STATE means your chord function will only target nodes/tokens right at the beginning of the parse state
// LOOKAHEAD_STATE means it could be offsetted deep in the parse state.
#define LOOKAHEAD_STATE(...) [](){return std::array{__VA_ARGS__};}(), STRINGIFY(__VA_ARGS__)
void populate_chords();

#define chord_error(msg, ...) error_nonblocking(nodes.front().begin_location, msg, __VA_ARGS__); return chord_result{.action = parse_action::error}

struct parser_state
{
	const lex_output& in;
	std::vector<node> nodes = {};
	std::size_t token_cursor = 0;
	std::size_t recursive_offset = 0;
	bool recursing = false;

	std::size_t chord_invocation_count = 0;
	std::size_t shift_count = 0;
	std::size_t reduce_count = 0;
	std::size_t recurse_count = 0;
	std::size_t commit_count = 0;

	bool shift()
	{
		if(this->token_cursor >= this->in.tokens.size())
		{
			return false;
		}
		nodes.push_back(node::wrap_token(this->in, this->token_cursor++));
		return true;
	}
};

node parse(const lex_output& impl_in, bool verbose_parse)
{
	parser_state state{.in = impl_in};
	srcloc tu_begin = impl_in.begin_locations.front();
	srcloc tu_end = impl_in.end_locations.back();
	state.nodes = {node{.payload = ast_translation_unit{}, .begin_location = tu_begin, .end_location = tu_end}};

	// only exit the loop if:
	// token cursor is at the end AND we have exactly 1 node
	// therefore, keep doing the loop if token cursor is not at the end OR we dont have one node
	while(state.token_cursor != state.in.tokens.size() || state.nodes.size() != 1)
	{
		std::span<const node> nodes_view = state.nodes;
		foreach_entry_from_hashed_subtrees(nodes_view.subspan(state.recursive_offset),
		[&state, was_recursing = state.recursing, verbose_parse](parse_table_entry& entry)
		{
			if(was_recursing != state.recursing)
			{
				// a previous call to this callback has triggered a recurse.
				// stop what we're doing
				return;
			}
			if(entry.chord_fn == nullptr)
			{
				std::string formatted_src = format_source(state.in.source, state.nodes[1].begin_location, state.nodes.back().end_location);
				std::string ast_dump;
				error_nonblocking(state.nodes[1].begin_location, "invalid syntax\n{}\n", formatted_src);
				for(const node& n : state.nodes)
				{
					n.verbose_print(state.in.source);
				}
				crash();
			}
			std::span<node> nodes = state.nodes;
			// subspan<1> so it doesnt work on the translation unit initial node.
			if(state.recursing)
			{
				nodes = nodes.subspan(state.recursive_offset);
			}
			else
			{
				nodes = nodes.subspan<1>();
			}
			chord_result result = entry.chord_fn(nodes, state);
			state.chord_invocation_count++;
			if(verbose_parse)
			{
				std::print("{}{}\n\t=> ", entry.description, entry.extensible ? ", ..." : "");
			}
			switch(result.action)
			{
				case parse_action::shift:
					state.shift();
					if(verbose_parse)
					{
						std::println("shift");
					}
					state.shift_count++;
					break;
				break;
				case parse_action::reduce:
				{
					slice rem = result.nodes_to_remove;
					// remove everything according to the slice
					std::size_t begin_offset = rem.offset + 1;
					if(state.recursing)
					{
						begin_offset = rem.offset + state.recursive_offset;
					}
					const srcloc begin_loc = state.nodes[begin_offset].begin_location;
					std::size_t end_offset = rem.offset + rem.length;
					const srcloc end_loc = state.nodes[end_offset].end_location;
					// todo: deal with begin/end loc better, rright now we just set that for all of the results.
					for(node& n : result.reduction_result)
					{
						n.begin_location = begin_loc;
						n.end_location = end_loc;
					}
					state.nodes.erase(state.nodes.begin() + begin_offset, state.nodes.begin() + begin_offset + rem.length);
					// then add the result(s).
					state.nodes.insert(state.nodes.begin() + begin_offset, result.reduction_result.begin(), result.reduction_result.end());
					state.recursive_offset = 0;
					state.recursing = false;
					if(verbose_parse)
					{
						std::println("reduce");
					}
					state.reduce_count++;
					break;
				}
				break;
				case parse_action::recurse:
					state.recursive_offset += result.reduction_result_offset;
					state.recursing = true;
					if(!was_recursing)
					{
						// additional offset of 1 as the chord included a secret translation unit node.
						state.recursive_offset++;
					}
					if(verbose_parse)
					{
						std::println("recurse({})", result.reduction_result_offset);
					}
					state.recurse_count++;
					return;
				break;
				case parse_action::commit:
				{
					slice rem = result.nodes_to_remove;
					// remove everything according to the slice
					const srcloc begin_loc = state.nodes[rem.offset + 1].begin_location;
					const srcloc end_loc = state.nodes[rem.offset + rem.length].end_location;
					// todo: deal with begin/end loc better, rright now we just set that for all of the results.
					for(node& n : result.reduction_result)
					{
						n.begin_location = begin_loc;
						n.end_location = end_loc;
					}
					state.nodes.erase(state.nodes.begin() + rem.offset + 1, state.nodes.begin() + rem.offset + 1 + rem.length);
					// then add the result(s).
					for(const node& n : result.reduction_result)
					{
						state.nodes.front().children.push_back(n);
					}
					if(verbose_parse)
					{
						std::println("commit");
					}
					state.commit_count++;
				}
				break;
				case parse_action::error:
				{
					std::string formatted_src = format_source(state.in.source, state.nodes.front().begin_location, state.nodes.back().end_location);
					std::print("{}\n{}\n", formatted_src, verbose_parse ? entry.description : "");
					crash();
				}
				break;
				default:
					panic("parse chord function returned unknown parse action");
				break;
			}
		});
	}

	auto sz = state.nodes.size();
	panic_ifnt(sz == 1, "expected one final ast node, but there are {} nodes", sz);

	if(verbose_parse)
	{
		constexpr auto verbose_parse_print = R"(+======
	chords = {}
	shifts = {}
	reductions = {}
	recursions = {}
	commits = {}
+======)";
		std::println(verbose_parse_print, state.chord_invocation_count, state.shift_count, state.reduce_count, state.recurse_count, state.commit_count);
	}
	return state.nodes.front();
}

//////////////////////////// SEMAL ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE semal

//////////////////////////// TYPE ////////////////////////////
#undef COMPILER_STAGE
#define COMPILER_STAGE type

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
	std::uint64_t time_setup, time_lex, time_parse;
	timer_restart();
	populate_chords();

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

	time_setup = elapsed_time();
	timer_restart();

	lex_output build_file_lex = lex(args.build_file);
	if(args.verbose_lex)
	{
		build_file_lex.verbose_print();
	}

	time_lex = elapsed_time();
	timer_restart();

	node build_file_ast = parse(build_file_lex, args.verbose_parse);
	if(args.verbose_ast)
	{
		build_file_ast.verbose_print(build_file_lex.source);
	}

	time_parse = elapsed_time();
	timer_restart();

	std::print("setup: {}\nlex:   {}\nparse: {}\ntotal: {}", time_setup / 1000.0f, time_lex / 1000.0f, time_parse / 1000.0f, (time_setup + time_lex + time_parse) / 1000.0f);
}


//////////////////////////// PARSE CHORDS ////////////////////////////
// we are back in parser land - this is where all the chord functions live. they sit here at the bottom because there is going to be *alot* of them.
#undef COMPILER_STAGE
#define COMPILER_STAGE parse
// a chord is a single entry in the parse tree
// so let's say we are in the middle of the parsing process.
// - ours is a shift-reduce parser, meaning it somehow needs to know when to shift and when it can perform a reduction
// - instead of checking the entire subtree state manually everytime, we convert the current parse state to a set of hashes
// - we then use those hashes to find a chord function that tells the parser what to do in the case of that exact parse state
// - all chord functions are hand-written, but to manually put them in the right place in the table would be too insane.
// - so the macro hackery below will take the chord function and magically install it in the correct place in the parse table.
//
// CHORD_BEGIN (signifies the beginning of a chord)
// STATE(...) is how you specify what exact parse state this particular chord is meant to handle
// 		example: STATE(TOKEN(colon), TOKEN(eq)) means that the chord function handles the case that the source contains exactly ":="
// 		note: this example is unrealistic, as := is actually its own token (token::initialiser). ctrl-f above.
// directly after STATE(...), you should put FN and write your c++ code. you are now defining your chord function.
//
// how to write a chord function
// =============================
//
// - you have a writable array of the nodes within the variable "nodes". you should consider it a std::span<node>
// - you already know what type of nodes are in the array - as they will exactly match what you specified in STATE(...)
// - if your state represents a valid reduction, you should do the reduction and return parse_action::reduce
// - if your state represents a syntax error, you should call chord_error(msg, ...) directly in your function. no need to return anything in that case, the macro will handle it for you.
//

void populate_chords(){

// just the translation unit
CHORD_BEGIN
	STATE(), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

// declarations

CHORD_BEGIN
	STATE(TOKEN(symbol), TOKEN(colon)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), TOKEN(symbol)), FN
	{
		// x : y
		// this is a declaration
		ast_decl decl
		{
			.type_name = std::string{std::get<ast_token>(nodes[2].payload).lexeme},
			.name = std::string{std::get<ast_token>(nodes[0].payload).lexeme}
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = decl}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), TOKEN(initialiser)), FN
	{
		ast_decl decl
		{
			.type_name = deduced_type,
			.name = std::string{std::get<ast_token>(nodes[0].payload).lexeme}
		};
		// keep the initialiser
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = decl}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon), WILDCARD), FN
	{
		chord_error("syntax error, looks like a malformed declaration");
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(symbol), TOKEN(colon), WILDCARD), FN
	{
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 0
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(initialiser)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(initialiser)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(initialiser), WILDCARD), FN
	{
		return {.action = parse_action::recurse};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(initialiser), WILDCARD), FN
	{
		auto& decl_node = nodes[0];
		auto& decl = std::get<ast_decl>(decl_node.payload);
		if(decl.initialiser.has_value())
		{
			chord_error("declaration {} appears to have more than one initialiser.", decl.name);
		}

		auto value_node = nodes[2];

		if(value_node.payload.index() == payload_index<ast_token>())
		{
			auto value = std::get<ast_token>(value_node.payload);
			if(value.tok == token::keyword_func || value.tok == token::symbol || value.tok == token::keyword_struct)
			{
					return {.action = parse_action::recurse, .reduction_result_offset = 2};
			}
			decl.initialiser = ast_expr{.expr_ = ast_literal_expr{}};
			auto& literal = std::get<ast_literal_expr>(decl.initialiser->expr_);

			switch(value.tok)
			{
				case token::integer_literal:
					literal.value = std::stol(std::string{value.lexeme});
				break;
				case token::decimal_literal:
					literal.value = std::stod(std::string{value.lexeme});
				break;
				case token::char_literal:
				{
					std::string escaped_chars = escape(value.lexeme);
					std::size_t chars_size = escaped_chars.size();
					if(chars_size != 1)
					{
						error(value_node.begin_location, "char literals must contain only 1 character, '{}' contains {} characters", value.lexeme, chars_size);
					}
					literal.value = escaped_chars.front();
				}
				break;
				case token::string_literal:
					literal.value = escape(value.lexeme);
				break;
				default:
					chord_error("a {} is not a valid initialiser for a declaration", token_traits[static_cast<int>(value.tok)].name);
				break;
			}
		}
		else
		{

			if(value_node.payload.index() == payload_index<ast_partial_funcdef>())
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 2};
			}
			else if(value_node.payload.index() == payload_index<ast_funcdef>())
			{
				auto funcdef = std::get<ast_funcdef>(value_node.payload);
				decl.initialiser = ast_expr{.expr_ = funcdef.func};
			}
			else if(value_node.payload.index() == payload_index<ast_expr>())
			{
				decl.initialiser = std::get<ast_expr>(value_node.payload);
			}
			else if(value_node.payload.index() == payload_index<ast_partial_callfunc>())
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 2};
			}
			else
			{
				chord_error("not sure how to handle a {} as a decl initialiser", node_names[value_node.payload.index()]);
			}
		}
		decl_node.end_location = nodes.back().end_location;
		decl_node.children = value_node.children;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = nodes.size() - 1}
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen), TOKEN(symbol)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(oparen), NODE(ast_decl)), FN
	{
		// this chord only occurs when a partial funcdef has been created with static params initially.
		// this chord wont be invoked if there are no static params, as instead it will be "keyword_func oparen"
		auto& func = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(func.stage == partial_funcdef_stage::defining_params)
		{
			func.params.push_back(std::get<ast_decl>(nodes[2].payload));
		}
		else
		{
			chord_error("syntax error while parsing function definition - expecting to be defining params");
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 2},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(cparen)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::defining_params)
		{
			chord_error("unexpected ')' token while parsing function definition. you have already finished defining the params.");
		}
		def.stage = partial_funcdef_stage::awaiting_arrow;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(canglebrack)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::defining_static_params)
		{
			chord_error("unexpected '>' token while parsing function definition. you have already finished defining the static params.");
		}
		def.stage = partial_funcdef_stage::defining_params;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(comma)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage == partial_funcdef_stage::defining_params)
		{
			def.stage = partial_funcdef_stage::awaiting_next_param;
		}
		else if(def.stage == partial_funcdef_stage::defining_static_params)
		{
			def.stage = partial_funcdef_stage::awaiting_next_static_param;
		}
		else
		{
			chord_error("unexpected ',' token while parsing function definition. you have already finished defining the params/static params.");
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), NODE(ast_decl)), FN
	{
		auto next_param = std::get<ast_decl>(nodes[1].payload);
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage == partial_funcdef_stage::awaiting_next_param)
		{
			def.params.push_back(next_param);
			def.stage = partial_funcdef_stage::defining_params;
		}
		else if(def.stage == partial_funcdef_stage::awaiting_next_static_param)
		{
			def.static_params.push_back(next_param);
			def.stage = partial_funcdef_stage::defining_static_params;
		}
		else
		{
			chord_error("unexpected decl named {} while parsing function definition. you have already finished defining the params/static params, or have you forgotten a comma?", next_param.name);
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(arrow)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_arrow)
		{
			chord_error("unexpected '->' token while parsing function definition. malformed function definition");
		}
		def.stage = partial_funcdef_stage::awaiting_return_type;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(symbol)), FN
	{
		auto return_typename = std::string{std::get<ast_token>(nodes[1].payload).lexeme};
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage == partial_funcdef_stage::awaiting_next_param || def.stage == partial_funcdef_stage::awaiting_next_static_param)
		{
			// so a comma preceded us. we're the start of a decl representing the next param, but havent figured that out yet
			// recurse.
			return
			{
				.action = parse_action::recurse,
				.reduction_result_offset = 1
			};
		}
		if(def.stage != partial_funcdef_stage::awaiting_return_type)
		{
			chord_error("unexpected '{}' token while parsing function definition. i wasnt ready for the return type yet", return_typename);
		}
		def.stage = partial_funcdef_stage::awaiting_body;
		def.return_type = return_typename;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1},
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(initialiser)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_body)
		{
			chord_error("unexpected ':=' token while parsing function definition. this is presumably a typename, but i wasn't ready for the body yet (assuming you are trying to declare as extern)");
		}
		return {.action = parse_action::shift};
	}

CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace), TOKEN(cbrace)), FN
	{
		// function decl with empty body
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_body)
		{
			chord_error("unexpected '{' token(s) while parsing function definition. this is presumably a typename, but i wasn't ready for the body yet (assuming you are trying to define a function body)");
		}
		ast_funcdef complete_funcdef
		{
			.func = 
			{
				.static_params = std::move(def.static_params),
				.params = std::move(def.params),
				.return_type = std::move(def.return_type),
				.is_extern = false
			}
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = complete_funcdef}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrace), NODE(ast_stmt)), FN
	{
		// this is the start of a block statement.
		node ret = {.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}};
		ret.children.push_back(nodes[1]);
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {ret}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), NODE(ast_stmt)), FN
	{
		auto& defnode = nodes[0];
		const auto& def = std::get<ast_partial_funcdef>(defnode.payload);
		const auto& stmt = std::get<ast_stmt>(nodes[1].payload);
		if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			// this is a blk statement
			// remember, we can only collapse this into a full function definition if the block statement is capped (i.e ended with a cbrace)
			// if not, then we need to recurse.
			auto blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(!blk.capped)
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 1};
			}

			ast_funcdef complete_funcdef
			{
				.func = 
				{
					.static_params = std::move(def.static_params),
					.params = std::move(def.params),
					.return_type = std::move(def.return_type),
					.is_extern = false
				}
			};
			defnode.payload = complete_funcdef;
			defnode.children = {nodes[1]};
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 1, .length = 1},
			};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("expected block statement, this is a {} statement", stmt_name);
		}

	}
CHORD_END


CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), NODE(ast_stmt), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(initialiser), TOKEN(keyword_extern)), FN
	{
		auto& def = std::get<ast_partial_funcdef>(nodes[0].payload);
		if(def.stage != partial_funcdef_stage::awaiting_body)
		{
			chord_error("unexpected ':= extern' tokens while parsing function definition. this is presumably a typename, but i wasn't ready for the body yet (assuming you are trying to declare as extern)");
		}
		// aha, the function is extern
		ast_funcdef complete_funcdef
		{
			.func =
			{
				.params = std::move(def.params),
				.return_type = std::move(def.return_type),
				.is_extern = true
			}
		};

		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = complete_funcdef}}
		};

	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oanglebrack)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), TOKEN(symbol)), FN
	{
		// it should be the start of a decl (the first param)
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), TOKEN(cparen)), FN
	{
		// decl with no parameters
		ast_partial_funcdef func
		{
			.static_params = {},
			.params = {},
			.return_type = "???",
			.stage = partial_funcdef_stage::awaiting_arrow
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oanglebrack), TOKEN(symbol)), FN
	{
		// it should be the start of a decl (the first param)
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oparen), NODE(ast_decl)), FN
	{
		// we have the start of a function definition. finally.
		ast_partial_funcdef func
		{
			.params = {std::get<ast_decl>(nodes[2].payload)},
			.return_type = "???"
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), TOKEN(oanglebrack), NODE(ast_decl)), FN
	{
		// we have the start of a function definition. finally.
		ast_partial_funcdef func
		{
			.static_params = {std::get<ast_decl>(nodes[2].payload)},
			.params = {},
			.return_type = "???",
			.stage = partial_funcdef_stage::defining_static_params
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = func}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), TOKEN(obrace)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), NODE(ast_stmt)), FN
	{
		auto& stmt_node = nodes[1];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			const auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(blk.capped)
			{
				return
				{
					.action = parse_action::reduce,
					.nodes_to_remove = {.offset = 0, .length = nodes.size()},
					.reduction_result = {node{.payload = ast_expr{.expr_ = ast_structdef_expr{}}, .children = {stmt_node}}}
				};
			}
			else
			{
				return {.action = parse_action::recurse, .reduction_result_offset = 1};
			}
		}
		else
		{
			const char* stmt_type = stmt.type_name();
			chord_error("struct keyword is followed by a statement. i only expect block statements to follow this keyword, but instead you've provided me with a {}", stmt_type);
		}
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), WILDCARD), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_struct), WILDCARD, WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_decl), TOKEN(semicol)), FN
	{
		return {.action = parse_action::recurse};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oanglebrack)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), TOKEN(cparen)), FN
	{
		// this is just a call with no params
		ast_callfunc_expr call
		{
			.function_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
			.static_params = {},
			.params = {},
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_expr{.expr_ = call}, .end_location = nodes.back().end_location}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), NODE(ast_expr)), FN
	{
		// this is a call with at least one param
		ast_partial_callfunc call
		{
			.function_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
			.params = {std::get<ast_expr>(nodes[2].payload)},
			.on_static_params = false
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {node{.payload = call, .end_location = nodes.back().end_location}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), WILDCARD), FN
	{
		return{.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oanglebrack), NODE(ast_expr)), FN
	{
		// this is a call with at least one static param
		ast_partial_callfunc call
		{
			.function_name = std::string{std::get<ast_token>(nodes[0].payload).lexeme},
			.static_params = {std::get<ast_expr>(nodes[2].payload)},
			.on_static_params = true
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 3},
			.reduction_result = {node{.payload = call, .end_location = nodes.back().end_location}}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oanglebrack), WILDCARD), FN
	{
		return{.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(comma)), FN
	{
		auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got ,");
		}
		call.awaiting_next_param = true;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), NODE(ast_expr)), FN
	{
		auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);
		if(!call.awaiting_next_param)
		{
			chord_error("syntax error while evaluating function call. unexpected expression which is presumably meant to be a parameter. did you forget a preceding comma?");
		}
		if(call.on_static_params)
		{
			call.static_params.push_back(std::get<ast_expr>(nodes[1].payload));
		}
		else
		{
			call.params.push_back(std::get<ast_expr>(nodes[1].payload));
		}
		call.awaiting_next_param = false;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(oparen)), FN
	{
		// we've most likely only hit this path coz we just parsed the beginning of a function call with static params.
		// we just remove the oparen and continue on as if we had already specified a first param (push-back will save us)
		auto& call = std::get<ast_partial_callfunc>(nodes[0].payload);
		call.on_static_params = false;
		call.awaiting_next_param = true;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(cparen)), FN
	{
		auto& call_node = nodes[0];
		auto& call = std::get<ast_partial_callfunc>(call_node.payload);
		if(call.on_static_params)
		{
			chord_error("syntax error while evaluating function call. did not expect a ) yet because i thought you were still providing static params. did you forget a >?");
		}
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got ,");
		}
		ast_callfunc_expr complete_call
		{
			.function_name = call.function_name,
			.static_params = call.static_params,
			.params = call.params
		};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_expr{.expr_ = complete_call}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), TOKEN(canglebrack)), FN
	{
		auto& call_node = nodes[0];
		auto& call = std::get<ast_partial_callfunc>(call_node.payload);
		if(!call.on_static_params)
		{
			chord_error("syntax error while evaluating function call. did not expect a > yet because i didn't think you were providing static params at this point.");
		}
		if(call.awaiting_next_param)
		{
			// i was expecting a param
			chord_error("syntax error while evaluating function call. expected an expression representing a parameter, got ,");
		}
		call.on_static_params = false;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = 1}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_callfunc), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(oparen), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl), TOKEN(semicol)), FN
	{
		auto& decl_node = nodes[0];
		auto decl = std::get<ast_decl>(decl_node.payload);
		
		decl_node.payload = ast_stmt
		{
			.stmt_ = ast_decl_stmt{.decl = decl}
		};

		decl_node.end_location = nodes.back().end_location;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = nodes.size() - 1}
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_expr), TOKEN(semicol)), FN
	{
		return
		{
			.action = parse_action::recurse
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr), TOKEN(semicol)), FN
	{
		auto& expr_node = nodes[0];
		auto expr = std::get<ast_expr>(expr_node.payload);
		
		expr_node.payload = ast_stmt
		{
			.stmt_ = ast_expr_stmt{.expr = expr}
		};

		expr_node.end_location = nodes.back().end_location;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 1, .length = nodes.size() - 1}
		};
		
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_expr)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(integer_literal)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(integer_literal), TOKEN(semicol)), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 0};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(integer_literal), TOKEN(semicol)), FN
	{
		std::int64_t value = std::stol(std::string{std::get<ast_token>(nodes[0].payload).lexeme});
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = value}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(integer_literal), TOKEN(cparen)), FN
	{
		std::int64_t value = std::stol(std::string{std::get<ast_token>(nodes[0].payload).lexeme});
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = value}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(integer_literal), TOKEN(canglebrack)), FN
	{
		std::int64_t value = std::stol(std::string{std::get<ast_token>(nodes[0].payload).lexeme});
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = value}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(integer_literal), TOKEN(comma)), FN
	{
		std::int64_t value = std::stol(std::string{std::get<ast_token>(nodes[0].payload).lexeme});
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_literal_expr{.value = value}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(symbol), TOKEN(semicol)), FN
	{
		return {.action = parse_action::recurse};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(semicol)), FN
	{
		std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_symbol_expr{.symbol = symbol}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(cparen)), FN
	{
		std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_symbol_expr{.symbol = symbol}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(canglebrack)), FN
	{
		std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_symbol_expr{.symbol = symbol}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(comma)), FN
	{
		std::string symbol{std::get<ast_token>(nodes[0].payload).lexeme};
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size() - 1},
			.reduction_result = {node{.payload = ast_expr{.expr_ = ast_symbol_expr{.symbol = symbol}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	STATE(NODE(ast_stmt)), FN
	{
		// statement is right at the beginning.
		// it should become the latest child of the translation unit node.
		const auto& stmt = std::get<ast_stmt>(nodes[0].payload);
		if(stmt.stmt_.index() == payload_index<ast_blk_stmt, decltype(stmt.stmt_)>())
		{
			if(!std::get<ast_blk_stmt>(stmt.stmt_).capped)
			{
				return {.action = parse_action::recurse};
			}
		}
		else if(stmt.stmt_.index() == payload_index<ast_metaregion_stmt, decltype(stmt.stmt_)>())
		{
			if(!std::get<ast_metaregion_stmt>(stmt.stmt_).capped)
			{
				return {.action = parse_action::recurse};
			}
		}
		return
		{
			.action = parse_action::commit,
			.nodes_to_remove = {.offset = 0, .length = 1},
			.reduction_result = {nodes[0]}
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrace), TOKEN(cbrace)), FN
	{
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), TOKEN(cbrace)), FN
	{
		// cap off a block statement if its a block statement.
		// no idea if it isnt a block statement.
		auto& stmt_node = nodes[0];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			if(blk.capped)
			{
				chord_error("extraneous closing brace. already just capped off a block statement.");
			}
			blk.capped = true;
			stmt_node.end_location = nodes[1].end_location;
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 1, .length = 1}
			};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("unexpected {} statement, expected block statement only.", stmt_name);
		}
	}
CHORD_END


CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), TOKEN(obrace)), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), NODE(ast_stmt)), FN
	{
		// append to a block statement
		// no idea if it isnt a block statement.
		auto& stmt_node = nodes[0];
		auto& stmt = std::get<ast_stmt>(stmt_node.payload);
		if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			auto& blk = std::get<ast_blk_stmt>(stmt.stmt_);
			stmt_node.children.push_back(nodes[1]);
			return
			{
				.action = parse_action::reduce,
				.nodes_to_remove = {.offset = 1, .length = 1}
			};
		}
		else if(payload_index<ast_metaregion_stmt, decltype(std::declval<ast_stmt>().stmt_)>() == stmt.stmt_.index())
		{
			// rhs stmt should be a block statement.
			auto& rhs_node = nodes[1];
			auto& rhs_stmt = std::get<ast_stmt>(rhs_node.payload);
			auto& lhs_metaregion = std::get<ast_metaregion_stmt>(stmt.stmt_);
			if(payload_index<ast_blk_stmt, decltype(std::declval<ast_stmt>().stmt_)>() != rhs_stmt.stmt_.index())
			{
				const char* stmt_name = rhs_stmt.type_name();
				chord_error("a metaregion stmt is followed by another statement. that rhs statement should always be a block statement, but instead you have provided a {}", stmt_name);
			}
			const auto& rhs_blk = std::get<ast_blk_stmt>(rhs_stmt.stmt_);
			if(rhs_blk.capped)
			{
				auto sz = stmt_node.children.size();
				panic_ifnt(stmt_node.children.empty(), "did not expect metaregion to have children already. it had {} children", sz);
				stmt_node.children = {rhs_node};
				lhs_metaregion.capped = true;
				stmt_node.end_location = rhs_node.end_location;
				return {.action = parse_action::reduce, .nodes_to_remove = {.offset = 1, .length = 1}};
			}
			return {.action = parse_action::recurse, .reduction_result_offset = 1};
		}
		else
		{
			const char* stmt_name = stmt.type_name();
			chord_error("syntax error concerning two statements together. i've only really thought about this when the lhs is a block statement, but this time its a {} statement", stmt_name);
		}
		
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt), WILDCARD), FN
	{
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 1
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(end_of_file)), FN
	{
		// translation unit -> end of file
		// this means the end.
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()}
		};
	}
CHORD_END

// wildcard chords begin

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(obrace), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 1};
	}
EXTENSIBLE
CHORD_END


CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(keyword_func), WILDCARD), FN
	{
		chord_error("invalid function definition syntax");
	}
CHORD_END

CHORD_BEGIN
	STATE(WILDCARD), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace), NODE(ast_stmt)), FN
	{
		// try to parse a block
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 1
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_partial_funcdef), TOKEN(obrace), WILDCARD), FN
	{
		// try to parse a block
		return
		{
			.action = parse_action::recurse,
			.reduction_result_offset = 2
		};
	}
	EXTENSIBLE
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_decl)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(NODE(ast_stmt)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(oparen)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(colon)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

// uniform function call syntax
// foo.bar(1, 2)
// is equivalent too
// bar(foo, 1, 2)
CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(dot)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(dot), NODE(ast_expr)), FN
	{
		std::string_view symbol = std::get<ast_token>(nodes[0].payload).lexeme;
		auto& expr_node = nodes[2];
		auto& expr = std::get<ast_expr>(expr_node.payload);
		if(expr.expr_.index() == payload_index<ast_callfunc_expr, decltype(expr.expr_)>())
		{
			// ufcs
			auto& call = std::get<ast_callfunc_expr>(expr.expr_);
			call.params.insert(call.params.begin(), ast_expr{.expr_ = ast_symbol_expr{.symbol = std::string{symbol}}});
		}
		else
		{
			const char* expr_type = expr.type_name();
			chord_error("unexpected expression type \"symbol.expr\". expected expr to be a callfunc expression, but instead it is a {}", expr_type);
		}
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = 2}
		};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(symbol), TOKEN(dot), WILDCARD), FN
	{
		return {.action = parse_action::recurse, .reduction_result_offset = 2};
	}
EXTENSIBLE
CHORD_END

CHORD_BEGIN
	STATE(WILDCARD, TOKEN(end_of_file)), FN
	{
		auto wildcard_node = nodes.front();
		const char* node_name = node_names[wildcard_node.payload.index()];
		std::string_view wildcard_src = quote_source(state.in.source, wildcard_node.begin_location, wildcard_node.end_location);
		chord_error("unexpected end of file, was expecting more after {} (\"{}\")", node_name, wildcard_src);
	}
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(compare), TOKEN(symbol)), FN
	{
		return {.action = parse_action::recurse};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(compare), TOKEN(symbol)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(compare), TOKEN(symbol), TOKEN(compare)), FN
	{
		return {.action = parse_action::shift};
	}
CHORD_END

CHORD_BEGIN
	LOOKAHEAD_STATE(TOKEN(compare), TOKEN(symbol), TOKEN(compare), TOKEN(obrace)), FN
	{
		std::string_view name = std::get<ast_token>(nodes[1].payload).lexeme;
		return
		{
			.action = parse_action::reduce,
			.nodes_to_remove = {.offset = 0, .length = nodes.size()},
			.reduction_result = {
				node{.payload = ast_stmt{.stmt_ = ast_metaregion_stmt{.name = std::string{name}}}},
				node{.payload = ast_stmt{.stmt_ = ast_blk_stmt{}}}
			}
		};
	}
CHORD_END

// end of chords
}
