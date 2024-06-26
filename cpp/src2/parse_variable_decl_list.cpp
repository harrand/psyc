#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// decl-list decl,
// adds the decl to the list.
CHORD_BEGIN
	STATE(NODE(variable_decl_list), TOKEN(comma), NODE(variable_decl))

	syntax::node::variable_decl_list list = GETNODE(variable_decl_list);
	SETINDEX(2);
	list.decls.push_back(GETNODE(variable_decl));
	REDUCE_TO(variable_decl_list, list);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif