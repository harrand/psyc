
#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(variable_decl))
	auto blk = GETNODE(unfinished_block);
	auto decl = GETNODE(variable_decl);
	if(!decl.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(decl.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(expression))
	auto blk = GETNODE(unfinished_block);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(expr.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(NODE(unfinished_block), TOKEN(cbrace))
	auto blk = GETNODE(unfinished_block);
	auto tok = GETTOKEN();
	REDUCE_TO(block, blk, tok.meta_srcloc);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif