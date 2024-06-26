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
	REDUCE_TO(std::make_unique<syntax::node::expression>(syntax::node::expression::type::identifier, std::move(idenptr)));
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif