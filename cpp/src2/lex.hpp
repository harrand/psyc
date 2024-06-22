#ifndef PSYC_LEX_HPP
#define PSYC_LEX_HPP
#include "srcloc.hpp"
#include <array>

namespace lex
{
	enum class type
	{
		eqeq,
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
		doc_comment,
		comment,
		mldoc_comment,
		mlcomment,
		keyword_static_if,
		keyword_build,
		keyword_namespace,
		keyword_struct,
		keyword_return,
		keyword_if,
		keyword_else,
		keyword_for,
		keyword_defer,
		keyword_deref,
		keyword_ref,
		identifier,
		integer_literal,
		decimal_literal,
		string_literal,
		char_literal,
		bool_literal,
		null_literal,
		_undefined,
		_count
	};

	constexpr std::array<const char*, static_cast<int>(type::_count)> type_name =
	{
		"==",
		"=",
		"::",
		":",
		";",
		"(",
		")",
		"{",
		"}",
		"[",
		"->",
		"]",
		"...",
		".",
		",",
		"!",
		"?",
		"///",
		"//",
		"/**",
		"/*",
		"static_if",
		"build",
		"namespace",
		"struct",
		"return",
		"if",
		"else",
		"for",
		"defer",
		"deref",
		"ref",
		"",
		"",
		"",
		"",
		"",
		"",
		"null",
		"<UNDEFINED TOKEN>"
	};

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
}
#endif // PSYC_LEX_HPP