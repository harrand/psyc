// expr-list expr,
// adds the expr to the list.
CHORD_BEGIN
	STATE(NODE(expression_list), TOKEN(comma), NODE(capped_expression))
	syntax::expression_list list = GETNODE(expression_list);
	SETINDEX(2);
	auto expr = GETNODE(capped_expression);
	list.exprs.push_back(expr);
	REDUCE_TO(expression_list, list);
	return {.t = result::type::reduce_success};
CHORD_END