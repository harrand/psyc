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
		keyword,
		identifier,
		semicolon,
		colon,
		arrow_forward,
		arrow_backward,
		line_comment,
		open_paren,
		close_paren,
		open_brace,
		close_brace,
		open_brack,
		close_brack,
		_count,
		_undefined,
	};

	struct token
	{
		type t = type::_undefined;
		std::string lexeme = "";
		srcloc meta_srcloc = srcloc::undefined();
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