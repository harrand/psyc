#ifndef PSYC_LEX_HPP
#define PSYC_LEX_HPP
#include "srcloc.hpp"
#include <array>
#include <string>
#include <vector>
#include <span>
#include <unordered_map>

namespace lex
{
	enum class type
	{
		eqeq,
		neq,
		eq,
		colcol,
		col,
		semicol,
		oparen,
		cparen,
		obrace,
		cbrace,
		obrack,
		cbrack,
		arrow,
		dotdotdot,
		dot,
		comma,
		exmark,
		qmark,
		cast,
		doc_comment,
		comment,
		mldoc_comment,
		mlcomment,
		keyword_static_if,
		keyword_build,
		keyword_namespace,
		keyword_struct,
		keyword_alias,
		keyword_return,
		keyword_if,
		keyword_else,
		keyword_for,
		keyword_defer,
		keyword_deref,
		keyword_ref,
		keyword_typeinfo,
		plus,
		minus,
		asterisk,
		slash,
		identifier,
		integer_literal,
		decimal_literal,
		string_literal,
		char_literal,
		bool_literal,
		null_literal,
		source_begin,
		_undefined,
		_count
	};

	// info about a token type
	struct trait
	{
		// what is its name (i.e the string we look for when lexing the source code)
		const char* name = "";
		// should the parser skip over this token type?
		bool parse_skip = false;
	};

	constexpr std::array<trait, static_cast<int>(type::_count)> lex_traits =
	{
		trait{.name = "=="},
		trait{.name = "!="},
		trait{.name = "="},
		trait{.name = "::"},
		trait{.name = ":"},
		trait{.name = ";"},
		trait{.name = "("},
		trait{.name = ")"},
		trait{.name = "{"},
		trait{.name = "}"},
		trait{.name = "["},
		trait{.name = "]"},
		trait{.name = "->"},
		trait{.name = "..."},
		trait{.name = "."},
		trait{.name = ","},
		trait{.name = "!"},
		trait{.name = "?"},
		trait{.name = "@"},
		trait{.name = "///", .parse_skip = true},
		trait{.name = "//", .parse_skip = true},
		trait{.name = "/**", .parse_skip = true},
		trait{.name = "/*", .parse_skip = true},
		trait{.name = "static_if"},
		trait{.name = "build"},
		trait{.name = "namespace"},
		trait{.name = "struct"},
		trait{.name = "alias"},
		trait{.name = "return"},
		trait{.name = "if"},
		trait{.name = "else"},
		trait{.name = "for"},
		trait{.name = "defer"},
		trait{.name = "deref"},
		trait{.name = "ref"},
		trait{.name = "typeinfo"},
		trait{.name = "+"},
		trait{.name = "-"},
		trait{.name = "*"},
		trait{.name = "/"},
		trait{.name = ""},
		trait{.name = ""},
		trait{.name = ""},
		trait{.name = ""},
		trait{.name = ""},
		trait{.name = ""},
		trait{.name = ""},
		trait{.name = "null"},
		trait{.name = "<UNDEFINED TOKEN>"}
	};

	constexpr trait get_trait(type t)
	{
		return lex_traits[static_cast<int>(t)];
	}


	struct token
	{
		type t = type::_undefined;
		std::string lexeme = "";
		srcloc meta_srcloc = srcloc::undefined();
		bool operator==(const token& rhs) const
		{
			return this->t == rhs.t && this->lexeme == rhs.lexeme;
		}
	};

	using tokens_list = std::vector<token>;
	using const_token_view = std::span<const token>;
	using token_view = std::span<token>;

	struct output
	{
		tokens_list tokens = {};
		std::string psy_source = {};
	};

	output tokenise(std::filesystem::path psy_file);

	struct state
	{
		std::unordered_map<std::filesystem::path, output> tokenised_input_files = {};
	};
}
#endif // PSYC_LEX_HPP