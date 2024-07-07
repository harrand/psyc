#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// expr,expr
// becomes an expression list
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(comma), NODE(expression))
	std::vector<syntax::node::expression> exprs;
	auto expr1 = GETNODE(expression);
	if(!expr1.capped)
	{
		return {.t = result::type::silent_reject};
	}
	exprs.push_back(expr1);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	if(!expr2.capped)
	{
		return {.t = result::type::silent_reject};
	}
	exprs.push_back(expr2);
	REDUCE_TO(expression_list, std::move(exprs));
	return {.t = result::type::reduce_success};
CHORD_END

// expression;
// reduces into an expression that is guaranteed to be capped.
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(semicol))
	auto expr = GETNODE(expression);
	expr.capped = true;
	REDUCE_TO(expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression,
// reduces into an expression that is guaranteed to be capped. doesn't consume the comma
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(comma))
	auto expr = GETNODE(expression);
	if(expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	expr.capped = true;
	REDUCE_TO_ADVANCED(0, 1, expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression)
// reduces into an expression that is guaranteed to be capped. doesn't consume the comma
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cparen))
	auto expr = GETNODE(expression);
	if(expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	expr.capped = true;
	REDUCE_TO_ADVANCED(0, 1, expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression}
// reduces into an expression that is guaranteed to be capped. doesn't consume the cbrace
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cbrace))
	auto expr = GETNODE(expression);
	if(expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	expr.capped = true;
	REDUCE_TO_ADVANCED(0, 1, expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr@iden
// reduces to a cast
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cast), NODE(identifier))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto iden = GETNODE(identifier);
	REDUCE_TO(expression, syntax::node::expression::type::cast, expr.unique_clone(), iden.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// expr@expr
// reduces to a cast
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cast), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::cast, expr.unique_clone(), typeexpr.unique_clone(), typeexpr.capped);
	return {.t = result::type::reduce_success};
CHORD_END

// expr+expr
// reduces to an addition
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(plus), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::addition, expr.unique_clone(), typeexpr.unique_clone(), typeexpr.capped);
	return {.t = result::type::reduce_success};
CHORD_END

// expr-expr
// reduces to a subtraction
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(minus), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::subtraction, expr.unique_clone(), typeexpr.unique_clone(), typeexpr.capped);
	return {.t = result::type::reduce_success};
CHORD_END

// expr*expr
// reduces to a multiplication
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(asterisk), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::multiplication, expr.unique_clone(), typeexpr.unique_clone(), typeexpr.capped);
	return {.t = result::type::reduce_success};
CHORD_END

// expr/expr
// reduces to a division
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(slash), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::division, expr.unique_clone(), typeexpr.unique_clone(), typeexpr.capped);
	return {.t = result::type::reduce_success};
CHORD_END

// expr=expr
// reduces to an assignment
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(eq), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	// assignment requires rhs expression to be capped. this could be wrong in some cases, the reason i've added it is that this means it has highest precedence. i.e x = y + z reduces to eq(x, plus(y, z)) and not plus(eq(x, y), z)
	if(!typeexpr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(expression, syntax::node::expression::type::assign, expr.unique_clone(), typeexpr.unique_clone(), typeexpr.capped);
	return {.t = result::type::reduce_success};
CHORD_END

// expr.expr
// dot access
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(dot), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::dot_access, expr.unique_clone(), expr2.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// expr == expr
// equality compare
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(eqeq), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::eqcompare, expr.unique_clone(), expr2.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// expr != expr
// inequality compare
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(neq), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::neqcompare, expr.unique_clone(), expr2.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// expr{}
// struct initialiser (no initialisers)
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(obrace), TOKEN(cbrace))
	auto struct_name = GETNODE(expression);
	if(struct_name.t != syntax::node::expression::type::namespace_access)
	{
		return {.t = result::type::error, .errmsg = std::format("pattern: {}{{}} is invalid, the preceding token(s) should instead constitute either an identifier (i.e `my_struct_name` or a namespace access e.g `myapi::struct_name`)", syntax::node::expression::type_names[static_cast<int>(struct_name.t)])};
	}
	std::vector<syntax::node::designated_initialiser> inits = {};
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name.unique_clone(), std::make_unique<syntax::node::designated_initialiser_list>(inits));
	return {.t = result::type::reduce_success};
CHORD_END

// expr{desig-init}
// struct initialiser (single initialiser)
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(obrace), NODE(designated_initialiser), TOKEN(cbrace))
	auto struct_name = GETNODE(expression);
	if(struct_name.t != syntax::node::expression::type::namespace_access)
	{
		return {.t = result::type::error, .errmsg = std::format("pattern: {}{{}} is invalid, the preceding token(s) should instead constitute either an identifier (i.e `my_struct_name` or a namespace access e.g `myapi::struct_name`)", syntax::node::expression::type_names[static_cast<int>(struct_name.t)])};
	}
	SETINDEX(2);
	std::vector<syntax::node::designated_initialiser> inits = {};
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name.unique_clone(), std::make_unique<syntax::node::designated_initialiser_list>(inits));
	return {.t = result::type::reduce_success};
CHORD_END

// expr{desig-init-list}
// struct initialiser (multiple initialisers)
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(obrace), NODE(designated_initialiser_list), TOKEN(cbrace))
	auto struct_name = GETNODE(expression);
	if(struct_name.t != syntax::node::expression::type::namespace_access)
	{
		return {.t = result::type::error, .errmsg = std::format("pattern: {}{{}} is invalid, the preceding token(s) should instead constitute either an identifier (i.e `my_struct_name` or a namespace access e.g `myapi::struct_name`)", syntax::node::expression::type_names[static_cast<int>(struct_name.t)])};
	}
	SETINDEX(2);
	auto inits = GETNODE(designated_initialiser_list);
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name.unique_clone(), inits.unique_clone());
	return {.t = result::type::reduce_success};
CHORD_END

// expr::iden
// namespace access
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(colcol), NODE(identifier))
	auto lhs = GETNODE(expression);
	SETINDEX(2);
	auto rhs = GETNODE(identifier);
	REDUCE_TO(expression, syntax::node::expression::type::namespace_access, lhs.unique_clone(), std::make_unique<syntax::node::expression>(syntax::node::expression::type::identifier, rhs.unique_clone()));
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif