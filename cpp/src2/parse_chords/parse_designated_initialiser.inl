// desiginit, desiginit
// becomes a desiginit list
CHORD_BEGIN
	STATE(NODE(designated_initialiser), TOKEN(comma), NODE(designated_initialiser))
	std::vector<syntax::designated_initialiser> inits;
	inits.push_back(GETNODE(designated_initialiser));
	SETINDEX(2);
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(designated_initialiser_list, std::move(inits));
	return {.t = result::type::reduce_success};
CHORD_END