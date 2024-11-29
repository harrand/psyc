// function-decl block
// set the blk as the function-decl's only child. error out if the decl already has one or more child.
CHORD_BEGIN
	STATE(NODE(function_decl), NODE(block))
	syntax::function_decl fn = GETNODE(function_decl);
	syntax::block blk = GETNODE(block);
	fn.children.push_back(syntax::node{.payload = blk});
	REDUCE_TO(capped_expression, syntax::expression::type::function_definition, fn);
	return {.t = result::type::reduce_success};
CHORD_END

// function-decl {}
// function implementation with no code inside. still a valid function implementation.
CHORD_BEGIN
	STATE(NODE(function_decl), TOKEN(obrace), TOKEN(cbrace))
	auto fn = GETNODE(function_decl);
	if(fn.is_extern)
	{
		return {.t = result::type::error, .errmsg = std::format("function  marked as extern, but is also followed by an implementation block. extern functions have no implementation.")};
	}
	REDUCE_TO(capped_expression, syntax::expression::type::function_definition, fn);
	return {.t = result::type::reduce_success};
CHORD_END