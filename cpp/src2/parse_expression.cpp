#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// expr,
// becomes an expression list
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(comma), NODE(expression))
	std::vector<syntax::node::expression> exprs;
	exprs.push_back(GETNODE(expression));
	SETINDEX(2);
	exprs.push_back(GETNODE(expression));
	REDUCE_TO(expression_list, std::move(exprs));
	return {.t = result::type::reduce_success};
CHORD_END

// expression;
// reduces into an expression that is guaranteed to be capped.
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(semicol))
	auto expr = GETNODE(expression);
	expr.capped = true;
	REDUCE_TO(expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif