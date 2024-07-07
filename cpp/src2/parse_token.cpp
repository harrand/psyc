#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

CHORD_BEGIN
	STATE(TOKEN(oparen), NODE(expression), TOKEN(cparen))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::parenthesised_expression, expr.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// == iden : namespace ==
// namespace meta region
CHORD_BEGIN
	STATE(TOKEN(eqeq), NODE(identifier), TOKEN(col), TOKEN(keyword_namespace), TOKEN(eqeq))
	SETINDEX(1);
	syntax::node::identifier name = GETNODE(identifier);
	REDUCE_TO(meta_region, name, syntax::node::meta_region::type::name_space);
	return {.t = result::type::reduce_success};
CHORD_END

// == iden : static_if ==
// static_if meta region
CHORD_BEGIN
	STATE(TOKEN(eqeq), NODE(identifier), TOKEN(col), TOKEN(keyword_static_if), TOKEN(eqeq))
	SETINDEX(1);
	syntax::node::identifier name = GETNODE(identifier);
	REDUCE_TO(meta_region, name, syntax::node::meta_region::type::static_if);
	return {.t = result::type::reduce_success};
CHORD_END

// { expr
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(unfinished_block, expr.unique_clone());

	return {.t = result::type::reduce_success};
CHORD_END

// { decl
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(variable_decl))
	SETINDEX(1);
	auto decl = GETNODE(variable_decl);
	if(!decl.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(unfinished_block, decl.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// { block
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(block))
	SETINDEX(1);
	auto blk = GETNODE(block);
	REDUCE_TO(unfinished_block, blk.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// { meta-region
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(meta_region))
	SETINDEX(1);
	auto reg = GETNODE(meta_region);
	REDUCE_TO(unfinished_block, reg.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// { alias
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(alias))
	SETINDEX(1);
	auto al = GETNODE(alias);
	REDUCE_TO(unfinished_block, al.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// { if-statement
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(if_statement))
	SETINDEX(1);
	auto stmt = GETNODE(if_statement);
	REDUCE_TO(unfinished_block, stmt.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// { structdata
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(structdata))
	SETINDEX(1);
	auto structd = GETNODE(structdata);
	REDUCE_TO(unfinished_block, structd.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// ref expr
// creates a ref expression
CHORD_BEGIN
	STATE(TOKEN(keyword_ref), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::ref, expr.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// deref expr
// creates a deref expression
CHORD_BEGIN
	STATE(TOKEN(keyword_deref), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::deref, expr.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// defer expr
// creates a deferred expression
CHORD_BEGIN
	STATE(TOKEN(keyword_defer), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::defer, expr.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// .iden := init
// designated initialiser
CHORD_BEGIN
	STATE(TOKEN(dot), NODE(identifier), TOKEN(col), TOKEN(eq), NODE(expression))
	SETINDEX(1);
	auto member_iden = GETNODE(identifier);
	SETINDEX(4);
	auto initialiser = GETNODE(expression);
	if(!initialiser.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(designated_initialiser, member_iden, initialiser);
	return {.t = result::type::reduce_success};
CHORD_END

// if expr {}
// create an if statement with no code inside (pointless but valid)
CHORD_BEGIN
	STATE(TOKEN(keyword_if), NODE(expression), TOKEN(obrace), TOKEN(cbrace))
	SETINDEX(1);
	auto cond = GETNODE(expression);
	SETINDEX(2);
	auto open = GETTOKEN();
	auto close = GETTOKEN();
	syntax::node::block empty_blk;
	empty_blk.start = open.meta_srcloc;
	empty_blk.finish = close.meta_srcloc;
	REDUCE_TO(if_statement, cond, empty_blk);
	return {.t = result::type::reduce_success};
CHORD_END

// if expr block
// create an if statement with code inside
CHORD_BEGIN
	STATE(TOKEN(keyword_if), NODE(expression), NODE(block))
	SETINDEX(1);
	auto cond = GETNODE(expression);
	auto blk = GETNODE(block);
	REDUCE_TO(if_statement, cond, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// else block
// else statement with no condition but with code.
CHORD_BEGIN
	STATE(TOKEN(keyword_else), NODE(block))
	SETINDEX(1);
	auto blk = GETNODE(block);
	REDUCE_TO(else_statement, syntax::node::expression{}, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// else {}
// else statement with no condition nor code. pointless but valid.
CHORD_BEGIN
	STATE(TOKEN(keyword_else), TOKEN(obrace), TOKEN(cbrace))
	SETINDEX(1);
	auto open = GETTOKEN();
	auto close = GETTOKEN();
	syntax::node::block empty_blk;
	empty_blk.start = open.meta_srcloc;
	empty_blk.finish = close.meta_srcloc;
	REDUCE_TO(else_statement, syntax::node::expression{}, empty_blk);
	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(keyword_else), NODE(if_statement))
	SETINDEX(1);
	auto stmt = GETNODE(if_statement);
	REDUCE_TO(else_statement, stmt.cond, *static_cast<syntax::node::block*>(stmt.children.front().get()));
	return {.t = result::type::reduce_success};
CHORD_END

// source-begin function-decl
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(function_decl))
	SETINDEX(1);
	auto fn = GETNODE(function_decl);
	if(fn.capped || fn.is_extern)
	{
		return {.t = result::type::send_to_output, .offset = 1};
	}
	return {.t = result::type::silent_reject};
CHORD_END

// source-begin meta-region
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(meta_region))
	SETINDEX(1);
	auto reg = GETNODE(meta_region);
	if(reg.capped)
	{
		return {.t = result::type::send_to_output, .offset = 1};
	}
	return {.t = result::type::silent_reject};
CHORD_END

// source-begin meta-region
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(variable_decl))
	SETINDEX(1);
	auto decl = GETNODE(variable_decl);
	if(decl.capped)
	{
		return {.t = result::type::send_to_output, .offset = 1};
	}
	return {.t = result::type::silent_reject};
CHORD_END

// source-begin struct
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(structdata))
	SETINDEX(1);
	auto structd = GETNODE(structdata);
	if(structd.capped)
	{
		return {.t = result::type::send_to_output, .offset = 1};
	}
	return {.t = result::type::silent_reject};
CHORD_END

// source-begin if-statement
// error: if statements must be within a block.
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(if_statement))
	return {.t = result::type::error, .errmsg = "if-statements must be within a block, not in the global scope", .offset = 1};
CHORD_END

#ifndef INFUNC
}}
#endif