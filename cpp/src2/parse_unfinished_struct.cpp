
#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// add a variable_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_struct), NODE(variable_decl))
	auto blk = GETNODE(unfinished_struct);
	auto decl = GETNODE(variable_decl);
	if(!decl.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(decl.unique_clone());
	REDUCE_TO(unfinished_struct, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an expression to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_struct), NODE(expression))
	auto blk = GETNODE(unfinished_struct);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(expr.unique_clone());
	REDUCE_TO(unfinished_struct, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a nested block to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_struct), NODE(block))
	auto blk = GETNODE(unfinished_struct);
	auto blk2 = GETNODE(block);
	blk.extend(blk2.unique_clone());
	REDUCE_TO(unfinished_struct, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a meta region to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_struct), NODE(meta_region))
	auto blk = GETNODE(unfinished_struct);
	auto reg = GETNODE(meta_region);
	blk.extend(reg.unique_clone());
	REDUCE_TO(unfinished_struct, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an alias to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_struct), NODE(alias))
	auto blk = GETNODE(unfinished_struct);
	auto al = GETNODE(alias);
	blk.extend(al.unique_clone());
	REDUCE_TO(unfinished_struct, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// close off an unfinished struct, spawning a proper struct
CHORD_BEGIN
	STATE(NODE(unfinished_struct), TOKEN(cbrace))
	auto blk = GETNODE(unfinished_struct);
	std::vector<syntax::node::variable_decl> members = {};
	for(auto iter = blk.children.begin(); iter != blk.children.end();)
	{
		if((*iter)->hash() == syntax::node::variable_decl{}.hash())
		{
			// this is a data member.
			members.push_back(*static_cast<syntax::node::variable_decl*>((*iter).get()));
			iter = blk.children.erase(iter);
		}
		else
		{
			iter++;
		}
	}
	REDUCE_TO(structdata, syntax::node::identifier{}, members);
	return {.t = result::type::reduce_success};
CHORD_END

// unfinished-struct function-decl
// add the function decl to the end of the struct.
CHORD_BEGIN
	STATE(NODE(unfinished_struct), NODE(function_decl))
	auto blk = GETNODE(unfinished_struct);
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

#ifndef INFUNC
}}
#endif