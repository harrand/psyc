#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// iden;
// turn into: expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(semicol))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden,
// turn into expr but keep the comma
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(comma))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden : iden
// explicitly-typed variable declaration with no initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(col), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::identifier type_name = GETNODE(identifier);
	REDUCE_TO(variable_decl, name, type_name, syntax::node::expression{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden ::= expr
// weakly-typed variable declaration with an initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(eq), NODE(expression))
	
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::expression initialiser = GETNODE(expression);
	REDUCE_TO(variable_decl, name, syntax::node::identifier{syntax::node::inferred_typename}, initialiser);
	return {.t = result::type::reduce_success};
CHORD_END


// iden :: (variable-decl-list) -> iden
// function declaration
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen), NODE(variable_decl_list), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::variable_decl_list params = GETNODE(variable_decl_list);
	SETINDEX(6);
	syntax::node::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, params, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif