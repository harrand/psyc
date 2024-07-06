#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// intlit;
// turn into: expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(semicol))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit,
// turn into expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(comma))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit)
// turn into expr but keep the cparen
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(cparen))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit@
// turn into expr but keep the cast
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(cast))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit+
// turn into expr but keep the plus
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(plus))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit-
// turn into expr but keep the minus
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(minus))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit*
// turn into expr but keep the multiply
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(asterisk))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// intlit/
// turn into expr but keep the divide
CHORD_BEGIN
	STATE(NODE(integer_literal), TOKEN(slash))
	syntax::node::integer_literal intlit = GETNODE(integer_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::integer_literal, intlit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif