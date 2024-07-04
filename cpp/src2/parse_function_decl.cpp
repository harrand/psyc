#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// function-decl := extern;
// mark a function as extern.
CHORD_BEGIN
	STATE(NODE(function_decl), TOKEN(col), TOKEN(eq), NODE(identifier), TOKEN(semicol))
	syntax::node::function_decl fn = GETNODE(function_decl);
	SETINDEX(3);
	syntax::node::identifier value = GETNODE(identifier);
	if(value.iden == "extern")
	{
		if(fn.is_extern)
		{
			return {.t = result::type::error, .errmsg = std::format("function \"{}\" was marked as `extern` twice.", fn.func_name.iden)};
		}
		fn.is_extern = true;
		REDUCE_TO(function_decl, fn);
		return {.t = result::type::reduce_success};
	}
	return {.t = result::type::error, .errmsg = std::format("assignment rhs of a function declaration {} must be one of the following: `extern`. the value \"{}\" is unrecognised", fn.func_name.iden, value.iden)};
CHORD_END

#ifndef INFUNC
}}
#endif