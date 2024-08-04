// decl-list decl,
// adds the decl to the list.
CHORD_BEGIN
	STATE(NODE(variable_decl_list), TOKEN(comma), NODE(variable_decl))

	syntax::variable_decl_list list = GETNODE(variable_decl_list);
	SETINDEX(2);
	list.decls.push_back(GETNODE(variable_decl));
	REDUCE_TO(variable_decl_list, list);
	return {.t = result::type::reduce_success};
CHORD_END

// decl-list := expr
// adds the initialiser to the last element in the decl list
CHORD_BEGIN
	STATE(NODE(variable_decl_list), TOKEN(col), TOKEN(eq), NODE(expression))
	syntax::variable_decl_list list = GETNODE(variable_decl_list);
	SETINDEX(3);
	syntax::expression initialiser = GETNODE(expression);
	if(!initialiser.capped)
	{
		return {.t = result::type::silent_reject};
	}
	if(list.decls.empty())
	{
		return {.t = result::type::error, .errmsg = std::format("attempt to assign an initialiser expression to an empty decl list")};
	}
	if(!list.decls.back().expr.is_null())
	{
		return {.t = result::type::error, .errmsg = std::format("attempt to provide default initialiser expression to variable \"{}\" twice", list.decls.back().var_name.iden)};
	}
	list.decls.back().expr = initialiser;
	REDUCE_TO(variable_decl_list, list);
	return {.t = result::type::reduce_success};
CHORD_END