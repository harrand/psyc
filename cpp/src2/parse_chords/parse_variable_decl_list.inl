// decl-list, decl
// adds the decl to the list.
CHORD_BEGIN
	STATE(NODE(variable_decl_list), TOKEN(comma), NODE(capped_variable_decl))
	auto list = GETNODE(variable_decl_list);
	SETINDEX(2);
	list.decls.push_back(GETNODE(capped_variable_decl));
	REDUCE_TO(variable_decl_list, list);
	return {.t = result::type::reduce_success};
CHORD_END

// decl-list, decl-list
// combines two decl lists
CHORD_BEGIN
	STATE(NODE(variable_decl_list), TOKEN(comma), NODE(variable_decl_list))
	auto list = GETNODE(variable_decl_list);
	SETINDEX(2);
	auto rhs = GETNODE(variable_decl_list);
	for(const auto& decl : rhs.decls)
	{
		list.decls.push_back(decl);
	}
	REDUCE_TO(variable_decl_list, list);
	return {.t = result::type::reduce_success};
CHORD_END

// decl-list := expr
// adds the initialiser to the last element in the decl list
CHORD_BEGIN
	STATE(NODE(variable_decl_list), TOKEN(col), TOKEN(eq), NODE(capped_expression))
	syntax::variable_decl_list list = GETNODE(variable_decl_list);
	SETINDEX(3);
	auto initialiser = GETNODE(capped_expression);
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