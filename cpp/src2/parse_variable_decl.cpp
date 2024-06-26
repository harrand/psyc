#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// decl,
// becomes a decl list
CHORD_BEGIN
	STATE(NODE(variable_decl), TOKEN(comma), NODE(variable_decl))
	std::vector<syntax::node::variable_decl> decls;
	decls.push_back(GETNODE(variable_decl));
	SETINDEX(2);
	decls.push_back(GETNODE(variable_decl));
	REDUCE_TO(variable_decl_list, std::move(decls));
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif