// add a variable_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_variable_decl))
	auto blk = GETNODE(unfinished_block);
	auto decl = GETNODE(capped_variable_decl);
	blk.extend(decl);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an expression to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_expression))
	auto blk = GETNODE(unfinished_block);
	auto expr = GETNODE(capped_expression);
	blk.extend(expr);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a nested block to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(block))
	auto blk = GETNODE(unfinished_block);
	auto blk2 = GETNODE(block);
	blk.extend(blk2);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a meta region to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(meta_region))
	auto blk = GETNODE(unfinished_block);
	auto reg = GETNODE(meta_region);
	blk.extend(reg);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an alias to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(alias))
	auto blk = GETNODE(unfinished_block);
	auto al = GETNODE(alias);
	blk.extend(al);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add an if-statement to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(if_statement))
	auto blk = GETNODE(unfinished_block);
	auto stmt = GETNODE(if_statement);
	blk.extend(stmt);
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
	auto blk = GETNODE(unfinished_block);
	auto tok = GETTOKEN();
	REDUCE_TO(block, blk, tok.meta_srcloc);
	return {.t = result::type::reduce_success};
CHORD_END

// unfinished-block function-decl
// add the function decl to the end of the block, and convert the block into an unfinished-struct. this is because if a block has a function declaration inside of it, it must be a struct method.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_function_decl))
	auto blk = GETNODE(unfinished_block);
	auto fn = GETNODE(capped_function_decl);
	blk.extend(fn);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END

// add a struct_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_struct_decl))
	auto blk = GETNODE(unfinished_block);
	auto structd = GETNODE(capped_struct_decl);
	blk.extend(structd);
	REDUCE_TO(unfinished_block, blk);
	return {.t = result::type::reduce_success};
CHORD_END