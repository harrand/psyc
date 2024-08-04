// namespace-access{}
// struct initialiser where the struct is in a namespace (no initialisers)
CHORD_BEGIN
	STATE(NODE(namespace_access), TOKEN(obrace), TOKEN(cbrace))
	auto struct_name = GETNODE(namespace_access);
	// rhs of the namespace access better be an identifier.
	if(struct_name.rhs.t != syntax::expression::type::identifier)
	{
		return {.t = result::type::error, .errmsg = std::format("namespace-access followed by {{}} is considered an empty struct initialiser. however, the right-most term of the namespace access \"{}\" was not an identifier as expected, but instead an expression", struct_name.rhs.to_string())};
	}
	std::vector<syntax::designated_initialiser> inits = {};
	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, struct_name, syntax::designated_initialiser_list{inits});
	return {.t = result::type::reduce_success};
CHORD_END

// namespace-access{desig-init}
// struct initialiser where the struct is in a namespace (single initialiser)
CHORD_BEGIN
	STATE(NODE(namespace_access), TOKEN(obrace), NODE(designated_initialiser), TOKEN(cbrace))

	auto struct_name = GETNODE(namespace_access);
	// rhs of the namespace access better be an identifier.
	if(struct_name.rhs.t != syntax::expression::type::identifier)
	{
		return {.t = result::type::error, .errmsg = std::format("namespace-access followed by {{...}} is considered a struct initialiser. however, the right-most term of the namespace access \"{}\" was not an identifier as expected, but instead an expression", struct_name.rhs.to_string())};
	}
	SETINDEX(2);
	std::vector<syntax::designated_initialiser> inits = {};
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, struct_name, syntax::designated_initialiser_list{inits});
	return {.t = result::type::reduce_success};
CHORD_END