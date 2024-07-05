#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// func-call;
// turn into: expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(function_call), TOKEN(semicol))
	syntax::node::function_call call = GETNODE(function_call);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::function_call, call.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// call,
// turn into expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(function_call), TOKEN(comma))
	syntax::node::function_call call = GETNODE(function_call);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::function_call, call.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// call)
// turn into expr but keep the cparen
CHORD_BEGIN
	STATE(NODE(function_call), TOKEN(cparen))
	syntax::node::function_call call = GETNODE(function_call);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::function_call, call.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// call@
// turn into expr but keep the cast
CHORD_BEGIN
	STATE(NODE(function_call), TOKEN(cast))
	syntax::node::function_call call = GETNODE(function_call);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::function_call, call.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif