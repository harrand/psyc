#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// == iden : namespace ==
// namespace meta region
CHORD_BEGIN
	STATE(TOKEN(eqeq), NODE(identifier), TOKEN(col), TOKEN(keyword_namespace), TOKEN(eqeq))
	SETINDEX(1);
	syntax::node::identifier name = GETNODE(identifier);
	REDUCE_TO(meta_region, name, syntax::node::meta_region::type::name_space);
	return {.t = result::type::reduce_success};
CHORD_END

// == iden : static_if ==
// static_if meta region
CHORD_BEGIN
	STATE(TOKEN(eqeq), NODE(identifier), TOKEN(col), TOKEN(keyword_static_if), TOKEN(eqeq))
	SETINDEX(1);
	syntax::node::identifier name = GETNODE(identifier);
	REDUCE_TO(meta_region, name, syntax::node::meta_region::type::static_if);
	return {.t = result::type::reduce_success};
CHORD_END

// { expr
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(unfinished_block, expr.unique_clone());

	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(variable_decl))
	SETINDEX(1);
	auto decl = GETNODE(variable_decl);
	if(!decl.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(unfinished_block, decl.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif