#ifndef PSYC_LEX_HPP
#define PSYC_LEX_HPP
#include "srcloc.hpp"
#include <string>
#include <vector>
#include <span>
#include <unordered_map>

namespace lex
{
	enum class type
	{
		keyword_struct,
		keyword_if,
		keyword_else,
		identifier,
		semicolon,
		colon,
		ellipsis,
		dot,
		comma,
		arrow_forward,
		arrow_backward,
		line_comment,
		integer_literal,
		decimal_literal,
		string_literal,
		char_literal,
		bool_literal,
		open_paren,
		close_paren,
		open_brace,
		close_brace,
		open_brack,
		close_brack,
		operator_double_equals,
		operator_equals,
		operator_plus,
		operator_minus,
		operator_slash,
		operator_asterisk,
		operator_ref,
		operator_deref,
		question_mark,
		return_statement,
		initialiser,
		dollar_sign,
		_count,
		_undefined,
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