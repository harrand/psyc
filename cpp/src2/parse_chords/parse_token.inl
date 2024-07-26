CHORD_BEGIN
	STATE(TOKEN(oparen), NODE(expression), TOKEN(cparen))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::parenthesised_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(eqeq), NODE(identifier), TOKEN(col), TOKEN(keyword_build), TOKEN(eqeq))
	SETINDEX(1);
	auto iden = GETNODE(identifier);
	REDUCE_TO(meta_region, iden, syntax::node::meta_region::type::build);
	return {.t = result::type::reduce_success};
CHORD_END

// &()->iden
// identifier (function type name with no parameters)
CHORD_BEGIN
	STATE(TOKEN(ampersand), TOKEN(oparen), TOKEN(cparen), TOKEN(arrow), NODE(identifier))
	SETINDEX(4);
	auto retty = GETNODE(identifier);
	REDUCE_TO(identifier, std::format("&()->{}", retty.iden));
	return {.t = result::type::reduce_success};
CHORD_END

// &expr-parenthesised -> iden
// identifier (function type name with a single parameter)
CHORD_BEGIN
	STATE(TOKEN(ampersand), NODE(expression), TOKEN(arrow), NODE(identifier))
	auto start = GETTOKEN();
	SETINDEX(1);
	auto expr = GETNODE(expression);
	SETINDEX(3);
	auto retty = GETNODE(identifier);

	if(expr.t != syntax::node::expression::type::parenthesised_expression)
	{
		return {.t = result::type::error, .errmsg = std::format("when attempting to parse token(s) as a function type, discovered invalid typename as the only parameter. expected parenthesised expression (containing an identifier), instead got a {} expression", expr.name())};
	}
	auto exprparen = NODE_AS(expr.expr, expression);
	if(exprparen.t != syntax::node::expression::type::identifier)
	{
		return {.t = result::type::error, .errmsg = std::format("when attempting to parse token(s) as a function type, discovered invalid typename as the only parameter. expected parenthesised expression (containing an identifier), instead got a parenthesised expression containing a {}", exprparen.name())};
	}
	auto expriden = NODE_AS(exprparen.expr, identifier);

	REDUCE_TO(identifier, std::format("&({})->{}", expriden.iden, retty.iden));
	return {.t = result::type::reduce_success};
CHORD_END

// &(expr_list)->iden
// identifier (function type name with two or more parameters)
CHORD_BEGIN
	STATE(TOKEN(ampersand), TOKEN(oparen), NODE(expression_list), TOKEN(cparen), TOKEN(arrow), NODE(identifier))
	auto start = GETTOKEN();
	SETINDEX(2);
	auto list = GETNODE(expression_list);
	SETINDEX(5);
	auto retty = GETNODE(identifier);
	std::string param_list;
	for(std::size_t i = 0; i < list.exprs.size(); i++)
	{
		const auto& expr = list.exprs[i];
		if(expr.t != syntax::node::expression::type::identifier)
		{
			return {.t = result::type::error, .errmsg = std::format("when attempting to parse token(s) as a function-type, discovered invalid typename in list of parameters. expected param at id {} ({}) to be an identifier expression, but instead it is a {} expression", i, expr.to_string(), expr.name())};
		}
		auto param = NODE_AS(expr.expr, identifier);
		param_list += param.iden;
		if(i < (list.exprs.size() - 1))
		{
			param_list += ",";
		}
	}
	REDUCE_TO(identifier, std::format("&({})->{}", param_list, retty.iden));
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
	REDUCE_TO(unfinished_block, expr);

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
	REDUCE_TO(unfinished_block, decl);
	return {.t = result::type::reduce_success};
CHORD_END

// { block
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(block))
	SETINDEX(1);
	auto blk = GETNODE(block);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// { meta-region
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(meta_region))
	SETINDEX(1);
	auto reg = GETNODE(meta_region);
	REDUCE_TO(unfinished_block, reg);
	return {.t = result::type::reduce_success};
CHORD_END

// { alias
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(alias))
	SETINDEX(1);
	auto al = GETNODE(alias);
	REDUCE_TO(unfinished_block, al);
	return {.t = result::type::reduce_success};
CHORD_END

// { if-statement
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(if_statement))
	SETINDEX(1);
	auto stmt = GETNODE(if_statement);
	REDUCE_TO(unfinished_block, stmt);
	return {.t = result::type::reduce_success};
CHORD_END

// { struct_decl
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(struct_decl))
	SETINDEX(1);
	auto structd = GETNODE(struct_decl);
	REDUCE_TO(unfinished_block, structd);
	return {.t = result::type::reduce_success};
CHORD_END

// ref expr
// creates a ref expression
CHORD_BEGIN
	STATE(TOKEN(keyword_ref), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::ref, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// deref expr
// creates a deref expression
CHORD_BEGIN
	STATE(TOKEN(keyword_deref), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::deref, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// defer expr
// creates a deferred expression
CHORD_BEGIN
	STATE(TOKEN(keyword_defer), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(expression, syntax::node::expression::type::defer, expr, {}, true);
	return {.t = result::type::reduce_success};
CHORD_END

// typeinfo expr
// creates a typeinfo based upon the type of `expr`
CHORD_BEGIN
	STATE(TOKEN(keyword_typeinfo), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::node::expression::type::typeinfo, expr);
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

// static if expr {}
// create an if statement with no code inside (pointless but valid)
CHORD_BEGIN
	STATE(TOKEN(keyword_static_if), NODE(expression), TOKEN(obrace), TOKEN(cbrace))
	SETINDEX(1);
	auto cond = GETNODE(expression);
	SETINDEX(2);
	auto open = GETTOKEN();
	auto close = GETTOKEN();
	syntax::node::block empty_blk;
	empty_blk.start = open.meta_srcloc;
	empty_blk.finish = close.meta_srcloc;
	REDUCE_TO(if_statement, cond, empty_blk, true);
	return {.t = result::type::reduce_success};
CHORD_END

// static if expr block
// create an if statement with code inside
CHORD_BEGIN
	STATE(TOKEN(keyword_static_if), NODE(expression), NODE(block))
	SETINDEX(1);
	auto cond = GETNODE(expression);
	auto blk = GETNODE(block);
	REDUCE_TO(if_statement, cond, blk, true);
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

// else if-statement
// turn into else-statement with a cond.
CHORD_BEGIN
	STATE(TOKEN(keyword_else), NODE(if_statement))
	SETINDEX(1);
	auto stmt = GETNODE(if_statement);
	REDUCE_TO(else_statement, stmt.cond, NODE_AS(stmt.children.front(), block));
	return {.t = result::type::reduce_success};
CHORD_END

CHORD_BEGIN
	STATE(TOKEN(keyword_return), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(expression, syntax::node::expression::type::return_statement, expr, {}, true);
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
	STATE(TOKEN(source_begin), NODE(struct_decl))
	SETINDEX(1);
	auto structd = GETNODE(struct_decl);
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
	SETINDEX(1);
	auto ifst = GETNODE(if_statement);
	if(!ifst.is_static)
	{
		return {.t = result::type::error, .errmsg = "non-static if-statements must be within a block, not in the global scope", .offset = 1};
	}
	return {.t = result::type::send_to_output, .offset = 1};
CHORD_END