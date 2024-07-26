// expr-list expr,
// adds the expr to the list.
CHORD_BEGIN
	STATE(NODE(expression_list), TOKEN(comma), NODE(expression))
	syntax::node::expression_list list = GETNODE(expression_list);
	SETINDEX(2);
	auto expr = GETNODE(expression);
	if(!expr.capped)
	{
		return {.t = result::type::silent_reject};
	}
	list.exprs.push_back(expr);
	REDUCE_TO(expression_list, list);
	return {.t = result::type::reduce_success};
CHORD_END