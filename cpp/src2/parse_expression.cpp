#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// if nothing is preceding an expression, send it to the output.
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(semicol))
	return {.t = reduce.no_prefix() ? result::type::send_to_output : result::type::silent_reject};
CHORD_END

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

#ifndef INFUNC
}}
#endif