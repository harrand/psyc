CHORD_BEGIN
	STATE(NODE(if_statement), NODE(else_statement))
	auto stmt = std::move(GETNODE(if_statement));
	auto else_stmt = GETNODE(else_statement);
	stmt.children.push_back(syntax::node{.payload = else_stmt});
	REDUCE_TO(if_statement, stmt);
	return {.t = result::type::reduce_success};
CHORD_END