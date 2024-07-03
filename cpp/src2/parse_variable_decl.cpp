#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// decl, decl
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

// decl := expr
// if decl didnt have an initialiser before, add that expr as the initialiser.
// if it did, emit a compile error.
CHORD_BEGIN
	STATE(NODE(variable_decl), TOKEN(col), TOKEN(eq), NODE(expression))

	syntax::node::variable_decl var = GETNODE(variable_decl);
	// two initialisers next to each other?
	if(!var.expr.is_null())
	{
		return {.t = result::type::error, .errmsg = std::format("two adjacent initialisers found for variable \"{}\"", var.var_name.iden)};
	}
	SETINDEX(3);
	auto expr_init = GETNODE(expression);
	REDUCE_TO(variable_decl, var.var_name, var.type_name, expr_init);
	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(NODE(variable_decl), TOKEN(semicol))
	return {.t = result::type::send_to_output};
CHORD_END

#ifndef INFUNC
}}
#endif