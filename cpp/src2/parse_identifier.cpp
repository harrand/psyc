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

#ifndef INFUNC
}}
#endif