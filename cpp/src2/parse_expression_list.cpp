#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// expr-list expr,
// adds the expr to the list.
CHORD_BEGIN
	STATE(NODE(expression_list), TOKEN(comma), NODE(expression))
	syntax::node::expression_list list = GETNODE(expression_list);
	SETINDEX(2);
	list.exprs.push_back(GETNODE(expression));
	REDUCE_TO(expression_list, list);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif