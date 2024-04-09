#ifndef PSYC_LEX_HPP
#define PSYC_LEX_HPP
#include <vector>
#include <span>
#include <string_view>

namespace lexer
{

	struct token
	{
		enum class type
		{
			newline,
			keyword,
			identifier,
			open_paren,
			close_paren,
			open_brace,
			close_brace,
			integer_literal,
			decimal_literal,
			string_literal,
			colon,
			arrow,
			semicolon
		} id;
		std::string value = "";
	};

	using tokens = std::vector<token>;
	using const_token_view = std::span<const token>;
	using token_view = std::span<token>;

	tokens lex(std::string_view psy);

}

#endif // PSYC_LEX_HPP
