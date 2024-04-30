#ifndef PSYC_LEX_HPP
#define PSYC_LEX_HPP
#include <vector>
#include <span>
#include <string_view>
#include <array>
#include <string>
namespace lexer
{

	struct token
	{
		enum class type
		{
			newline,
			keyword,
			identifier,
			line_comment,
			open_paren,
			close_paren,
			open_brace,
			close_brace,
			open_brack,
			close_brack,
			commar,
			integer_literal,
			decimal_literal,
			char_literal,
			string_literal,
			bool_literal,
			equals,
			double_equals,
			not_equals,
			colon,
			dot,
			arrow,
			semicolon,
			ellipsis,
			plus,
			minus,
			bitwise_complement,
			logical_negation,
			ref,
			deref,
			defer,
			_count
		} id;
		std::string value = "";

		std::string to_string() const;
	};

	constexpr std::array<const char*, static_cast<std::size_t>(token::type::_count)> token_type_names =
	{
		"\n",
		"",
		"",
		"",
		"(",
		")",
		"{",
		"}",
		"[",
		"]",
		",",
		"",
		"",
		"",
		"",
		"",
		"=",
		"==",
		"!=",
		":",
		".",
		" -> ",
		";",
		"...",
		"+",
		"-",
		"~",
		"!",
		"ref",
		"deref",
		"defer"
	};

	using tokens = std::vector<token>;
	using const_token_view = std::span<const token>;
	using token_view = std::span<token>;

	tokens lex(std::string_view psy);

}

#endif // PSYC_LEX_HPP
