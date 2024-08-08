// function-decl := extern;
// mark a function as extern.
CHORD_BEGIN
	STATE(NODE(function_decl), TOKEN(col), TOKEN(eq), NODE(identifier), TOKEN(semicol))
	syntax::function_decl fn = GETNODE(function_decl);
	SETINDEX(3);
	syntax::identifier value = GETNODE(identifier);
	if(value.iden == "extern")
	{
		if(fn.is_extern)
		{
			return {.t = result::type::error, .errmsg = std::format("function \"{}\" was marked as `extern` twice.", fn.func_name.iden)};
		}
		fn.is_extern = true;
		REDUCE_TO(function_decl, fn);
		return {.t = result::type::reduce_success};
	}
	return {.t = result::type::error, .errmsg = std::format("assignment rhs of a function declaration {} must be one of the following: `extern`. the value \"{}\" is unrecognised", fn.func_name.iden, value.iden)};
CHORD_END

// function-decl block
// set the blk as the function-decl's only child. error out if the decl already has one or more child.
CHORD_BEGIN
	STATE(NODE(function_decl), NODE(block))
	syntax::function_decl fn = GETNODE(function_decl);
	syntax::block blk = GETNODE(block);
	if(fn.capped)
	{
		return {.t = result::type::error, .errmsg = std::format("detected multiple implementation blocks for function \"{}\"", fn.func_name.iden)};
	}
	fn.children.push_back(syntax::node{.payload = blk});
	fn.capped = true;
	REDUCE_TO(function_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END

// function-decl {}
// function implementation with no code inside. still a valid function implementation.
CHORD_BEGIN
	STATE(NODE(function_decl), TOKEN(obrace), TOKEN(cbrace))
	auto fn = std::move(GETNODE(function_decl));
	if(fn.is_extern)
	{
		return {.t = result::type::error, .errmsg = std::format("function \"{}\" marked as extern, but is also followed by an implementation block. extern functions have no implementation.", fn.func_name.iden)};
	}
	if(fn.capped)
	{
		return {.t = result::type::error, .errmsg = std::format("function \"{}\" has two implementation blocks? it must have only one, or be marked as extern with no block at all.", fn.func_name.iden)};
	}
	fn.capped = true;
	REDUCE_TO(function_decl, fn);
	return {.t = result::type::reduce_success};
CHORD_END