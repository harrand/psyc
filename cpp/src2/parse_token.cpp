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

#ifndef INFUNC
}}
#endif