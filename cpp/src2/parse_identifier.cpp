#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(semicol))
	const syntax::node::identifier& iden = GETNODE(identifier);
	syntax::node_ptr idenptr = iden.unique_clone();
	REDUCE_TO(std::make_unique<syntax::node::primary_expression>(syntax::node::primary_expression::type::identifier, std::move(idenptr)));
	return {.t = result::type::reduce_success};
CHORD_END

// iden:: could be alot of things. shift for more info.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol))
	const auto& iden = GETNODE(identifier);
	//REDUCE_TO(std::make_unique<syntax::node::primary_expression>(syntax::node::primary_expression::type::identifier, &iden));
	return {.t = result::type::shift};
CHORD_END

// something::else
// cant be a function definition
// could be a namespace access.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), NODE(identifier))
	return {.t = result::type::shift};
CHORD_END

#ifndef INFUNC
}}
#endif