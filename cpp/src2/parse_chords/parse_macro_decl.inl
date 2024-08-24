// macro-decl block
// set the blk as the macro-decl's only child. error out if the decl already has one or more child.
CHORD_BEGIN
	STATE(NODE(macro_decl), NODE(block))
	syntax::macro_decl mac = GETNODE(macro_decl);
	syntax::block blk = GETNODE(block);
	mac.children.push_back(syntax::node{.payload = blk});
	REDUCE_TO(capped_macro_decl, mac);
	return {.t = result::type::reduce_success};
CHORD_END

// macro-decl {}
// macro implementation with no code inside. still a valid macro implementation.
CHORD_BEGIN
	STATE(NODE(macro_decl), TOKEN(obrace), TOKEN(cbrace))
	auto mac = GETNODE(macro_decl);
	REDUCE_TO(capped_macro_decl, mac);
	return {.t = result::type::reduce_success};
CHORD_END