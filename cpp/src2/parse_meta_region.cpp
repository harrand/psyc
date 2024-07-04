#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// meta-region block
// set the blk as the meta-region's only child. error out if the decl already has one or more child.
CHORD_BEGIN
	STATE(NODE(meta_region), NODE(block))
	syntax::node::meta_region reg = GETNODE(meta_region);
	syntax::node::block blk = GETNODE(block);
	if(reg.capped)
	{
		return {.t = result::type::error, .errmsg = std::format("detected multiple implementation blocks for meta-region \"{}\"", reg.metaname.iden)};
	}
	reg.children.push_back(blk.unique_clone());
	reg.capped = true;
	REDUCE_TO(meta_region, reg);
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif