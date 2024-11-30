// cappedexpr,cappedexpr
// becomes an expression list
CHORD_BEGIN
	STATE(NODE(capped_expression), TOKEN(comma), NODE(capped_expression))
	std::vector<syntax::expression> exprs;
	auto expr1 = GETNODE(capped_expression);
	exprs.push_back(expr1);
	SETINDEX(2);
	auto expr2 = GETNODE(capped_expression);
	exprs.push_back(expr2);
	REDUCE_TO(expression_list, std::move(exprs));
	return {.t = result::type::reduce_success};
CHORD_END

// expression;
// reduces into an expression that is guaranteed to be capped.
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(semicol))
	auto expr = GETNODE(expression);
	REDUCE_TO(capped_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression,
// reduces into an expression that is guaranteed to be capped. doesn't consume the comma
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(comma))
	auto expr = GETNODE(expression);
	REDUCE_TO_ADVANCED(0, 1, capped_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression)
// reduces into an expression that is guaranteed to be capped. doesn't consume the comma
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cparen))
	auto expr = GETNODE(expression);
	REDUCE_TO_ADVANCED(0, 1, capped_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression>
// reduces into an expression that is guaranteed to be capped. doesn't consume the comma
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(canglebrack))
	auto expr = GETNODE(expression);
	REDUCE_TO_ADVANCED(0, 1, capped_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression]]
// reduces into an expression that is guaranteed to be capped. doesn't consume the comma
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cbrackbrack))
	auto expr = GETNODE(expression);
	REDUCE_TO_ADVANCED(0, 1, capped_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expression}
// reduces into an expression that is guaranteed to be capped. doesn't consume the cbrace
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cbrace))
	auto expr = GETNODE(expression);
	REDUCE_TO_ADVANCED(0, 1, capped_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr@iden
// reduces to a cast
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cast), NODE(identifier))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto iden = GETNODE(identifier);
	REDUCE_TO(expression, syntax::expression::type::cast, expr, iden);
	return {.t = result::type::reduce_success};
CHORD_END

// expr@expr
// reduces to a cast
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cast), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::cast, expr, typeexpr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr@cappedexpr
// reduces to a cast
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(cast), NODE(capped_expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(capped_expression);
	REDUCE_TO(capped_expression, syntax::expression::type::cast, expr, typeexpr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr+expr
// reduces to an addition
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(plus), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::addition, expr, typeexpr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr-expr
// reduces to a subtraction
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(minus), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::subtraction, expr, typeexpr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr*expr
// reduces to a multiplication
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(asterisk), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::multiplication, expr, typeexpr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr/expr
// reduces to a division
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(slash), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto typeexpr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::division, expr, typeexpr);
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
	REDUCE_TO(expression, syntax::expression::type::assign, expr, typeexpr);
	return {.t = result::type::reduce_success};
CHORD_END

// expr.expr
// dot access
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(dot), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::dot_access, expr, expr2);
	return {.t = result::type::reduce_success};
CHORD_END

// expr.func-call
// uniform function call syntax: x.f(a, b) ==> f(x, a, b)
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(dot), NODE(function_call))

	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto call = GETNODE(function_call);
	call.params.exprs.insert(call.params.exprs.begin(), expr);
	REDUCE_TO(function_call, call);
	return {.t = result::type::reduce_success};
CHORD_END

// expr == expr
// equality compare
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(eqeq), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::eqcompare, expr, expr2);
	return {.t = result::type::reduce_success};
CHORD_END

// expr != expr
// inequality compare
CHORD_BEGIN
	STATE(NODE(expression), TOKEN(neq), NODE(expression))
	auto expr = GETNODE(expression);
	SETINDEX(2);
	auto expr2 = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::neqcompare, expr, expr2);
	return {.t = result::type::reduce_success};
CHORD_END