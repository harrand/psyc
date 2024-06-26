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
	REDUCE_TO_ADVANCED(std::make_unique<syntax::node::expression>(syntax::node::expression::type::identifier, iden.unique_clone()), 0, 1);
	return {.t = result::type::reduce_success};
CHORD_END

// iden,
// turn into expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(comma))
	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(std::make_unique<syntax::node::expression>(syntax::node::expression::type::identifier, iden.unique_clone()), 0, 1);
	return {.t = result::type::reduce_success};
CHORD_END

// iden : iden;
// explicitly-typed variable declaration with no initialiser. keep the semicol.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(col), NODE(identifier), TOKEN(semicol))
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::identifier type_name = GETNODE(identifier);
	REDUCE_TO_ADVANCED(std::make_unique<syntax::node::variable_decl>(name, type_name, syntax::node::expression{}), 0, 1);
	return {.t = result::type::reduce_success};
CHORD_END

// iden: iden := expr;
// explicitly-typed variable declaration with an initialiser. keep the semicol.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(col), NODE(identifier), TOKEN(col), TOKEN(eq), NODE(expression), TOKEN(semicol))
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::identifier type_name = GETNODE(identifier);
	SETINDEX(5);
	syntax::node::expression initialiser = GETNODE(expression);
	REDUCE_TO_ADVANCED(std::make_unique<syntax::node::variable_decl>(name, type_name, initialiser), 0, 1);
	return {.t = result::type::reduce_success};
CHORD_END

// iden ::= expr;
// weakly-typed variable declaration with an initialiser. keep the semicol
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(eq), NODE(expression), TOKEN(semicol))
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::expression initialiser = GETNODE(expression);
	REDUCE_TO_ADVANCED(std::make_unique<syntax::node::variable_decl>(name, syntax::node::identifier{syntax::node::inferred_typename}, syntax::node::expression{}), 0, 1);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif