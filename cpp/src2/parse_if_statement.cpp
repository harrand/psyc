#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

CHORD_BEGIN
	STATE(NODE(if_statement), NODE(else_statement))
	auto stmt = GETNODE(if_statement);
	auto else_stmt = GETNODE(else_statement);
	stmt.children.push_back(else_stmt.unique_clone());
	REDUCE_TO(if_statement, stmt);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif