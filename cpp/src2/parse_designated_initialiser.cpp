#include "parse_macros.hpp"
#ifndef INFUNC
#include "diag.hpp"
#include "parse.hpp"
namespace parse{
void foo(){
#endif

// desiginit, desiginit
// becomes a desiginit list
CHORD_BEGIN
	STATE(NODE(designated_initialiser), TOKEN(comma), NODE(designated_initialiser))
	std::vector<syntax::node::designated_initialiser> inits;
	inits.push_back(GETNODE(designated_initialiser));
	SETINDEX(2);
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(designated_initialiser_list, std::move(inits));
	return {.t = result::type::reduce_success};
CHORD_END

#ifndef INFUNC
}}
#endif