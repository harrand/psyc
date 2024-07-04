
#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// add a variable_decl to the end of the unfinished block.
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

// add an expression to the end of the unfinished block.
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

// add a nested block to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(block))
	auto blk = GETNODE(unfinished_block);
	auto blk2 = GETNODE(block);
	blk.extend(blk2.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a meta region to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(meta_region))
	auto blk = GETNODE(unfinished_block);
	auto reg = GETNODE(meta_region);
	blk.extend(reg.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an alias to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(alias))
	auto blk = GETNODE(unfinished_block);
	auto al = GETNODE(alias);
	blk.extend(al.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// close off an unfinished block, spawning a proper block.
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