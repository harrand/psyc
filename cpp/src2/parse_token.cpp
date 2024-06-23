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
	return true;
CHORD_END

#ifndef INFUNC
}}
#endif