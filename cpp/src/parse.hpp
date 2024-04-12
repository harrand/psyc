#ifndef PSYC_PARSE_HPP
#define PSYC_PARSE_HPP
#include "ast.hpp"
#include "lex.hpp"

namespace parser
{
	ast parse(lexer::const_token_view tokens);
}
#endif // PSYC_PARSE_HPP