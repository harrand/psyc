CHORD_BEGIN
	STATE(TOKEN(oparen), NODE(expression), TOKEN(cparen))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::parenthesised_expression, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// () -> iden
// function declaration with no args and constargs omitted
CHORD_BEGIN
	STATE(TOKEN(oparen),TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	SETINDEX(3);
	syntax::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, syntax::variable_decl_list{}, syntax::variable_decl_list{}, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// (variable-decl-list) -> iden
// function declaration with args but constargs omitted
CHORD_BEGIN
	STATE(TOKEN(oparen), NODE(variable_decl_list), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	SETINDEX(1);
	syntax::variable_decl_list params = GETNODE(variable_decl_list);
	SETINDEX(4);
	syntax::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, params, syntax::variable_decl_list{}, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// (variable-decl) -> iden
// function declaration with one arg but constargs omitted
CHORD_BEGIN
	STATE(TOKEN(oparen), NODE(variable_decl), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	SETINDEX(1);
	syntax::variable_decl param = GETNODE(variable_decl);
	SETINDEX(4);
	syntax::identifier return_type_name = GETNODE(identifier);
	std::vector<syntax::variable_decl> params;
	params.push_back(param);

	REDUCE_TO(function_decl, params, syntax::variable_decl_list{}, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// <>function-decl
// function declaration with explicitly no constargs
CHORD_BEGIN
	STATE(TOKEN(oanglebrack), TOKEN(canglebrack), NODE(function_decl))

	SETINDEX(2);
	auto fn = GETNODE(function_decl);
	REDUCE_TO(function_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END

// <variable-decl>function-decl
// function declaration with a single constarg
CHORD_BEGIN
	STATE(TOKEN(oanglebrack), NODE(capped_variable_decl), TOKEN(canglebrack), NODE(function_decl))

	SETINDEX(1);
	auto constarg = GETNODE(capped_variable_decl);
	SETINDEX(3);
	auto fn = GETNODE(function_decl);

	fn.constparams.decls.push_back(constarg);

	REDUCE_TO(function_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END

// <variable-decl-list>function-decl
// function declaration with a single constarg
CHORD_BEGIN
	STATE(TOKEN(oanglebrack), NODE(variable_decl_list), TOKEN(canglebrack), NODE(function_decl))

	SETINDEX(1);
	auto constargs = GETNODE(variable_decl_list);
	SETINDEX(3);
	auto fn = GETNODE(function_decl);

	fn.constparams = constargs;

	REDUCE_TO(function_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END


// == iden : build ==
CHORD_BEGIN
	STATE(TOKEN(eqeq), NODE(identifier), TOKEN(col), TOKEN(keyword_build), TOKEN(eqeq))
	SETINDEX(1);
	auto iden = GETNODE(identifier);
	REDUCE_TO(meta_region, iden, syntax::meta_region::type::build);
	return {.t = result::type::reduce_success};
CHORD_END

// { expr
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(capped_expression))
	SETINDEX(1);
	auto expr = GETNODE(capped_expression);
	REDUCE_TO(unfinished_block, expr);

	return {.t = result::type::reduce_success};
CHORD_END

// { decl
// starts an unfinished block
CHORD_BEGIN
	STATE(TOKEN(obrace), NODE(capped_variable_decl))
	SETINDEX(1);
	auto decl = GETNODE(capped_variable_decl);
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
	REDUCE_TO(expression, syntax::expression::type::ref, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// deref expr
// creates a deref expression
CHORD_BEGIN
	STATE(TOKEN(keyword_deref), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::deref, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// defer expr
// creates a deferred expression
CHORD_BEGIN
	STATE(TOKEN(keyword_defer), NODE(capped_expression))
	SETINDEX(1);
	auto expr = GETNODE(capped_expression);
	REDUCE_TO(capped_expression, syntax::expression::type::defer, expr, {});
	return {.t = result::type::reduce_success};
CHORD_END

// typeinfo expr
// creates a typeinfo based upon the type of `expr`
CHORD_BEGIN
	STATE(TOKEN(keyword_typeinfo), NODE(expression))
	SETINDEX(1);
	auto expr = GETNODE(expression);
	REDUCE_TO(expression, syntax::expression::type::typeinfo, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// .iden := init
// designated initialiser
CHORD_BEGIN
	STATE(TOKEN(dot), NODE(identifier), TOKEN(col), TOKEN(eq), NODE(capped_expression))
	SETINDEX(1);
	auto member_iden = GETNODE(identifier);
	SETINDEX(4);
	auto initialiser = GETNODE(capped_expression);
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
	syntax::block empty_blk;
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
	syntax::block empty_blk;
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
	REDUCE_TO(else_statement, syntax::expression{}, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// else {}
// else statement with no condition nor code. pointless but valid.
CHORD_BEGIN
	STATE(TOKEN(keyword_else), TOKEN(obrace), TOKEN(cbrace))
	SETINDEX(1);
	auto open = GETTOKEN();
	auto close = GETTOKEN();
	syntax::block empty_blk;
	empty_blk.start = open.meta_srcloc;
	empty_blk.finish = close.meta_srcloc;
	REDUCE_TO(else_statement, syntax::expression{}, empty_blk);
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

// [[expr-list]] variable-decl
// annotated variable-decl (multiple exprs).
CHORD_BEGIN
	STATE(TOKEN(obrackbrack), NODE(expression_list), TOKEN(cbrackbrack), NODE(capped_variable_decl))
	SETINDEX(1);
	syntax::annotations anno{GETNODE(expression_list)};
	SETINDEX(3);
	auto fn = GETNODE(capped_variable_decl);
	fn.annotations.exprs.insert(fn.annotations.exprs.begin(), anno.exprs.begin(), anno.exprs.end());
	REDUCE_TO(capped_variable_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END

// [[expr]] variable-decl
// annotated variable-decl (single expr).
CHORD_BEGIN
	STATE(TOKEN(obrackbrack), NODE(capped_expression), TOKEN(cbrackbrack), NODE(capped_variable_decl))
	SETINDEX(1);
	syntax::annotations anno;
	anno.exprs.push_back({GETNODE(capped_expression)});
	SETINDEX(3);
	auto var = GETNODE(capped_variable_decl);
	var.annotations.exprs.insert(var.annotations.exprs.begin(), anno.exprs.begin(), anno.exprs.end());
	REDUCE_TO(capped_variable_decl, var);
	return {.t = result::type::reduce_success};
CHORD_END

// [[expr-list]] struct-decl
// annotated struct-decl (multi expr)
CHORD_BEGIN
	STATE(TOKEN(obrackbrack), NODE(expression_list), TOKEN(cbrackbrack), NODE(capped_struct_decl))
	SETINDEX(1);
	syntax::annotations anno{GETNODE(expression_list)};
	SETINDEX(3);
	auto var = GETNODE(capped_struct_decl);
	var.annotations.exprs.insert(var.annotations.exprs.begin(), anno.exprs.begin(), anno.exprs.end());
	REDUCE_TO(capped_struct_decl, var);
	return {.t = result::type::reduce_success};
CHORD_END

// [[expr]] struct-decl
// annotated struct-decl (single expr).
CHORD_BEGIN
	STATE(TOKEN(obrackbrack), NODE(capped_expression), TOKEN(cbrackbrack), NODE(capped_struct_decl))
	SETINDEX(1);
	syntax::annotations anno;
	anno.exprs.push_back({GETNODE(capped_expression)});
	SETINDEX(3);
	auto fn = GETNODE(capped_struct_decl);
	fn.annotations.exprs.insert(fn.annotations.exprs.begin(), anno.exprs.begin(), anno.exprs.end());
	REDUCE_TO(capped_struct_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END

// return cappedexpr
// return a statement.
CHORD_BEGIN
	STATE(TOKEN(keyword_return), NODE(capped_expression))
	SETINDEX(1);
	auto expr = GETNODE(capped_expression);
	REDUCE_TO(capped_expression, syntax::expression::type::return_statement, expr, {});
	return {.t = result::type::reduce_success};
CHORD_END

// source-begin function-decl
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(capped_function_decl))
	return {.t = result::type::send_to_output, .offset = 1};
CHORD_END

// source-begin meta-region
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(capped_meta_region))
	return {.t = result::type::send_to_output, .offset = 1};
CHORD_END

// source-begin meta-region
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(capped_variable_decl))
	return {.t = result::type::send_to_output, .offset = 1};
CHORD_END

// source-begin struct
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(capped_struct_decl))
	return {.t = result::type::send_to_output, .offset = 1};
CHORD_END

// source-begin if-statement
// error: if statements must be within a block.
CHORD_BEGIN
	STATE(TOKEN(source_begin), NODE(if_statement))
	SETINDEX(1);
	auto ifst = GETNODE_CPY(if_statement);
	if(!ifst.is_static)
	{
		return {.t = result::type::error, .errmsg = "non-static if-statements must be within a block, not in the global scope", .offset = 1};
	}
	return {.t = result::type::send_to_output, .offset = 1};
CHORD_END