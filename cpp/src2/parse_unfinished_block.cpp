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

// add an if-statement to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(if_statement))
	auto blk = GETNODE(unfinished_block);
	auto stmt = GETNODE(if_statement);
	blk.extend(stmt.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an else-statement as a child of the if-statement that is at the end of the unfinished block so far. if the last child of this unfinished block is not an if-statement, then emit an error.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(else_statement))
	auto blk = GETNODE(unfinished_block);
	auto else_stmt = GETNODE(else_statement);
	if(blk.children.empty())
	{
		return {.t = result::type::error, .errmsg = "123"};
	}
	auto last_child = blk.children.back().get();
	if(last_child->hash() != syntax::node::if_statement{}.hash())
	{
		return {.t = result::type::error, .errmsg = std::format("else-statement must proceed an if-statement. it instead seems to proceed a {}", last_child->name())};
	}
	last_child->children.push_back(else_stmt.unique_clone());
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

// unfinished-block function-decl
// add the function decl to the end of the block, and convert the block into an unfinished-struct. this is because if a block has a function declaration inside of it, it must be a struct method.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(function_decl))
	auto blk = GETNODE(unfinished_block);
	auto fn = GETNODE(function_decl);
	if(fn.is_extern)
	{
		return {.t = result::type::error, .errmsg = std::format("block contains method declaration \"{}\" which is marked as extern. methods cannot be extern.", fn.func_name.iden)};
	}
	if(!fn.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.children.push_back(fn.unique_clone());
	REDUCE_TO(unfinished_struct, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a structdata to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(structdata))
	auto blk = GETNODE(unfinished_block);
	auto structd = GETNODE(structdata);
	if(!structd.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(structd.unique_clone());
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif