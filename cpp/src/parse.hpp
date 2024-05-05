#ifndef PSYC_PARSE_HPP
#define PSYC_PARSE_HPP
#include "ast.hpp"
#include "lex.hpp"

namespace parse
{
	ast tokens(lex::const_token_view toks);

	struct state
	{
		std::unordered_map<std::filesystem::path, ast> parsed_input_files = {};
	};
}

#endif // PSYC_PARSE_HPP