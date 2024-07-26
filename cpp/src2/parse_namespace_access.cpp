// namespace-access{}
// struct initialiser where the struct is in a namespace (no initialisers)
CHORD_BEGIN
	STATE(NODE(namespace_access), TOKEN(obrace), TOKEN(cbrace))
	auto struct_name = GETNODE(namespace_access);
	// rhs of the namespace access better be an identifier.
	if(struct_name.rhs.t != syntax::node::expression::type::identifier)
	{
		return {.t = result::type::error, .errmsg = std::format("namespace-access followed by {{}} is considered an empty struct initialiser. however, the right-most term of the namespace access \"{}\" was not an identifier as expected, but instead a {}", struct_name.rhs.to_string(), struct_name.rhs.name())};
	}
	std::vector<syntax::node::designated_initialiser> inits = {};
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name.unique_clone(), std::make_unique<syntax::node::designated_initialiser_list>(inits));
	return {.t = result::type::reduce_success};
CHORD_END

// namespace-access{desig-init}
// struct initialiser where the struct is in a namespace (single initialiser)
CHORD_BEGIN
	STATE(NODE(namespace_access), TOKEN(obrace), NODE(designated_initialiser), TOKEN(cbrace))

	auto struct_name = GETNODE(namespace_access);
	// rhs of the namespace access better be an identifier.
	if(struct_name.rhs.t != syntax::node::expression::type::identifier)
	{
		return {.t = result::type::error, .errmsg = std::format("namespace-access followed by {{...}} is considered a struct initialiser. however, the right-most term of the namespace access \"{}\" was not an identifier as expected, but instead a {}", struct_name.rhs.to_string(), struct_name.rhs.name())};
	}
	SETINDEX(2);
	std::vector<syntax::node::designated_initialiser> inits = {};
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name.unique_clone(), std::make_unique<syntax::node::designated_initialiser_list>(inits));
	return {.t = result::type::reduce_success};
CHORD_END