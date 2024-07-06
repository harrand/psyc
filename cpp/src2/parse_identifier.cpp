#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// iden;
// turn into: expr but keep the semicol
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(semicol))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden,
// turn into expr but keep the comma
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(comma))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden)
// turn into expr but keep the cparen
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(cparen))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden@
// turn into expr but keep the cast
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(cast))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden+
// turn into expr but keep the plus
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(plus))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden-
// turn into expr but keep the minus
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(minus))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden*
// turn into expr but keep the multiply
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(asterisk))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden/
// turn into expr but keep the divide
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(slash))

	syntax::node::identifier iden = GETNODE(identifier);
	REDUCE_TO_ADVANCED(0, 1, expression, syntax::node::expression::type::identifier, iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// iden : iden
// explicitly-typed variable declaration with no initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(col), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::identifier type_name = GETNODE(identifier);
	REDUCE_TO(variable_decl, name, type_name, syntax::node::expression{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden ::= expr
// weakly-typed variable declaration with an initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(eq), NODE(expression))
	
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::expression initialiser = GETNODE(expression);
	if(!initialiser.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(variable_decl, name, syntax::node::identifier{syntax::node::inferred_typename}, initialiser, true);
	return {.t = result::type::reduce_success};
CHORD_END


// iden :: (variable-decl-list) -> iden
// function declaration
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen), NODE(variable_decl_list), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::variable_decl_list params = GETNODE(variable_decl_list);
	SETINDEX(6);
	syntax::node::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, params, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: () -> iden
// function declaration with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen),TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(5);
	syntax::node::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, syntax::node::variable_decl_list{}, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr-list)
// function call (multiple arguments)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression_list), TOKEN(cparen))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::expression_list params = GETNODE(expression_list);

	REDUCE_TO(function_call, name, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr)
// function call (single argument)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression), TOKEN(cparen))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	std::vector<syntax::node::expression> params = {};
	params.push_back(GETNODE(expression));

	REDUCE_TO(function_call, name, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden()
// function call with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), TOKEN(cparen))

	syntax::node::identifier name = GETNODE(identifier);

	REDUCE_TO(function_call, name, syntax::node::expression_list{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: alias := expr;
// type alias
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_alias), TOKEN(col), TOKEN(eq), NODE(expression), TOKEN(semicol))
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(5);
	syntax::node::expression expr = GETNODE(expression);
	REDUCE_TO(alias, name, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: struct block
// create a named struct (no methods)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_struct), NODE(block))

	auto name = GETNODE(identifier);
	SETINDEX(3);
	auto blk = GETNODE(block);
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
			// todo: error out? this should only ever be variable decls?
			iter++;
		}
	}
	REDUCE_TO(structdata, name, members, true);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: struct structdata
// create a named struct
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_struct), NODE(structdata))

	auto name = GETNODE(identifier);
	SETINDEX(3);
	auto structd = GETNODE(structdata);
	if(structd.capped)
	{
		return {.t = result::type::error, .errmsg = std::format("struct \"{}\" detected dodgy syntax. struct block seen even though the struct is considered capped.", name.iden)};
	}
	structd.struct_name = name;
	structd.capped = true;
	REDUCE_TO(structdata, structd);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif