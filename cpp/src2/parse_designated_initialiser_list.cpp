// desiglist, desiginit
// add to the end of a desiglist
CHORD_BEGIN
	STATE(NODE(designated_initialiser_list), TOKEN(comma), NODE(designated_initialiser))
	auto list = GETNODE(designated_initialiser_list);
	SETINDEX(2);
	list.inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(designated_initialiser_list, list);
	return {.t = result::type::reduce_success};
CHORD_END