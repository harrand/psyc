#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// remember, we always try to munch the biggest reduction possible. this one is tiny, so it will only happen as a last resort.
CHORD_BEGIN
	STATE(NODE(identifier))
	const auto& iden = GETNODE(identifier);
	REDUCE_TO(std::make_unique<syntax::node::primary_expression>(syntax::node::primary_expression::type::identifier, &iden));
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif