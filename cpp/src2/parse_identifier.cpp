#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

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