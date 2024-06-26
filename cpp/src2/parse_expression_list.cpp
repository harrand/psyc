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
	STATE(NODE(expression_list), NODE(expression), TOKEN(comma))
	syntax::node::expression_list list = GETNODE(expression_list);
	list.exprs.push_back(GETNODE(expression));
	REDUCE_TO(std::make_unique<syntax::node::expression_list>(list));
	return {.t = result::type::reduce_success};
CHORD_END

// expr-list expr;
// adds the expr to the list but does not consume the semicol
CHORD_BEGIN
	STATE(NODE(expression_list), NODE(expression), TOKEN(semicol))
	syntax::node::expression_list list = GETNODE(expression_list);
	list.exprs.push_back(GETNODE(expression));
	REDUCE_TO_ADVANCED(std::make_unique<syntax::node::expression_list>(list), 0, 1);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif