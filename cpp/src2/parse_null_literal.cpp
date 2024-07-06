#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// nulllit;
// turn into: expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(semicol))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit,
// turn into expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(comma))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit)
// turn into expr but keep the cparen
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(cparen))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit@
// turn into expr but keep the cast
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(cast))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit+
// turn into expr but keep the plus
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(plus))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit-
// turn into expr but keep the minus
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(minus))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit*
// turn into expr but keep the multiply
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(asterisk))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit/
// turn into expr but keep the divide
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(slash))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// nulllit=
// turn into expr but keep the eq
CHORD_BEGIN
	STATE(NODE(null_literal), TOKEN(eq))
	syntax::node::null_literal nulllit = GETNODE(null_literal);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::null_literal, nulllit.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif