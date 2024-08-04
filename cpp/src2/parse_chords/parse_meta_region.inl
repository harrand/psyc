// meta-region block
// set the blk as the meta-region's only child. error out if the decl already has one or more child.
CHORD_BEGIN
	STATE(NODE(meta_region), NODE(block))
	syntax::meta_region reg = GETNODE(meta_region);
	syntax::block blk = GETNODE(block);
	if(reg.capped)
	{
		return {.t = result::type::error, .errmsg = std::format("detected multiple implementation blocks for meta-region \"{}\"", reg.metaname.iden)};
	}
	reg.children.push_back(syntax::node{.payload = blk});
	reg.capped = true;
	REDUCE_TO(meta_region, reg);
	return {.t = result::type::reduce_success};
CHORD_END

// meta-region{}
CHORD_BEGIN
	STATE(NODE(meta_region), TOKEN(obrace), TOKEN(cbrace))
	syntax::meta_region reg = GETNODE(meta_region);
	syntax::block blk;
	blk.loc = GETTOKEN().meta_srcloc;
	blk.start = blk.loc;
	blk.finish = GETTOKEN().meta_srcloc;
	if(reg.capped)
	{
		return {.t = result::type::error, .errmsg = std::format("detected multiple implementation blocks for meta-region \"{}\"", reg.metaname.iden)};
	}
	reg.children.push_back(syntax::node{.payload = blk});
	reg.capped = true;
	REDUCE_TO(meta_region, reg);
	return {.t = result::type::reduce_success};
CHORD_END