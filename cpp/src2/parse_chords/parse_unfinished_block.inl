// add a variable_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_variable_decl))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(capped_variable_decl));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add an expression to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_expression))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(capped_expression));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add a nested block to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(block))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(block));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add a meta region to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(meta_region))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(meta_region));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add an alias to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(alias))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(alias));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add an if-statement to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(if_statement))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(if_statement));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add an else-statement as a child of the if-statement that is at the end of the unfinished block so far. if the last child of this unfinished block is not an if-statement, then emit an error.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(else_statement))
	auto blk = GETNODE(unfinished_block);
	if(blk.children.empty())
	{
		return {.t = result::type::error, .errmsg = "123"};
	}

	auto last_child = blk.children.back();
	if(!NODE_IS(last_child, if_statement))
	{
		return {.t = result::type::error, .errmsg = std::format("else-statement must proceed an if-statement. it instead seems to proceed a {}", last_child->name())};
	}
	last_child->children().push_back(syntax::node{.payload = GETNODE(else_statement)});
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// close off an unfinished block, spawning a proper block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), TOKEN(cbrace))
	auto blk = GETNODE(unfinished_block);
	auto tok = GETTOKEN();
	REDUCE_TO(block, std::move(blk), tok.meta_srcloc);
	return {.t = result::type::reduce_success};
CHORD_END

// unfinished-block function-decl
// add the function decl to the end of the block, and convert the block into an unfinished-struct. this is because if a block has a function declaration inside of it, it must be a struct method.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_function_decl))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(capped_function_decl));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END

// add a struct_decl to the end of the unfinished block.
CHORD_BEGIN
	STATE(NODE(unfinished_block), NODE(capped_struct_decl))
	auto blk = GETNODE(unfinished_block);
	blk.extend(GETNODE(capped_struct_decl));
	REDUCE_TO(unfinished_block, std::move(blk));
	return {.t = result::type::reduce_success};
CHORD_END