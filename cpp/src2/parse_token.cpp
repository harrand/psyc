#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

CHORD_BEGIN
	STATE(TOKEN(integer_literal))
	const lex::token& integer = GETTOKEN();
	REDUCE_TO(std::make_unique<syntax::node::integer_literal>(std::stoi(integer.lexeme)));
	return true;
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(decimal_literal))
	const lex::token& integer = GETTOKEN();
	REDUCE_TO(std::make_unique<syntax::node::decimal_literal>(std::stod(integer.lexeme)));
	return true;
CHORD_END

#ifndef INFUNC
}}
#endif