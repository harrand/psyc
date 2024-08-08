// add a variable_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(variable_decl))
	auto blk = std::move(GETNODE(unfinished_block));
	auto decl = std::move(GETNODE(variable_decl));
	if(!decl.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(decl);
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
	blk.extend(expr);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a nested block to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(block))
	auto blk = std::move(GETNODE(unfinished_block));
	auto blk2 = GETNODE(block);
	blk.extend(blk2);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a meta region to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(meta_region))
	auto blk = std::move(GETNODE(unfinished_block));
	auto reg = std::move(GETNODE(meta_region));
	blk.extend(reg);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an alias to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(alias))
	auto blk = std::move(GETNODE(unfinished_block));
	auto al = std::move(GETNODE(alias));
	blk.extend(al);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an if-statement to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(if_statement))
	auto blk = std::move(GETNODE(unfinished_block));
	auto stmt = std::move(GETNODE(if_statement));
	blk.extend(stmt);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an else-statement as a child of the if-statement that is at the end of the unfinished block so far. if the last child of this unfinished block is not an if-statement, then emit an error.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(else_statement))
	auto blk = std::move(GETNODE(unfinished_block));
	auto else_stmt = GETNODE(else_statement);
	if(blk.children.empty())
	{
		return {.t = result::type::error, .errmsg = "123"};
	}

	auto last_child = blk.children.back();
	if(!NODE_IS(last_child, if_statement))
	{
		return {.t = result::type::error, .errmsg = std::format("else-statement must proceed an if-statement. it instead seems to proceed a {}", last_child->name())};
	}
	last_child->children().push_back(syntax::node{.payload = else_stmt});
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// close off an unfinished block, spawning a proper block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), TOKEN(cbrace))
	auto blk = std::move(GETNODE(unfinished_block));
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
	if(!fn.capped && !fn.is_extern)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(fn);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a struct_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(struct_decl))
	auto blk = GETNODE(unfinished_block);
	auto structd = GETNODE(struct_decl);
	if(!structd.capped)
	{
		return {.t = result::type::silent_reject};
	}
	blk.extend(structd);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END