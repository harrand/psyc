// iden : iden
// explicitly-typed variable declaration with no initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(col), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::identifier type_name = GETNODE(identifier);
	REDUCE_TO(variable_decl, name, type_name, syntax::node::expression{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden ::= expr
// weakly-typed variable declaration with an initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(eq), NODE(expression))
	
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::expression initialiser = GETNODE(expression);
	if(!initialiser.capped)
	{
		return {.t = result::type::silent_reject};
	}
	REDUCE_TO(variable_decl, name, syntax::node::identifier{syntax::node::inferred_typename}, initialiser, true);
	return {.t = result::type::reduce_success};
CHORD_END


// iden :: (variable-decl-list) -> iden
// function declaration
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen), NODE(variable_decl_list), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::variable_decl_list params = GETNODE(variable_decl_list);
	SETINDEX(6);
	syntax::node::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, params, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: () -> iden
// function declaration with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen),TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(5);
	syntax::node::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, syntax::node::variable_decl_list{}, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: (variable-decl) -> iden
// function declaration with one arg
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen), NODE(variable_decl), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::node::variable_decl param = GETNODE(variable_decl);
	SETINDEX(6);
	syntax::node::identifier return_type_name = GETNODE(identifier);
	std::vector<syntax::node::variable_decl> params;
	params.push_back(param);

	REDUCE_TO(function_decl, name, params, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr-list)
// function call (multiple arguments)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression_list), TOKEN(cparen))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::node::expression_list params = GETNODE(expression_list);

	REDUCE_TO(function_call, name, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr)
// function call (single argument)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression), TOKEN(cparen))

	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(2);
	std::vector<syntax::node::expression> params = {};
	params.push_back(GETNODE(expression));

	REDUCE_TO(function_call, name, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden()
// function call with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), TOKEN(cparen))

	syntax::node::identifier name = GETNODE(identifier);

	REDUCE_TO(function_call, name, syntax::node::expression_list{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: namespace
// namespace meta region
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_namespace))
	syntax::node::identifier name = GETNODE(identifier);
	REDUCE_TO(meta_region, name, syntax::node::meta_region::type::name_space);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: alias := expr;
// type alias
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_alias), TOKEN(col), TOKEN(eq), NODE(expression), TOKEN(semicol))
	syntax::node::identifier name = GETNODE(identifier);
	SETINDEX(5);
	syntax::node::expression expr = GETNODE(expression);
	REDUCE_TO(alias, name, expr);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: struct block
// create a named struct (no methods)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_struct), NODE(block))

	auto name = GETNODE(identifier);
	SETINDEX(3);
	auto blk = GETNODE(block);
	auto result_struct = syntax::node::struct_decl{name, true};
	result_struct.children.push_back(blk);
	REDUCE_TO(struct_decl, result_struct);
	return {.t = result::type::reduce_success};
CHORD_END

// iden{}
// struct initialiser (no initialisers)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), TOKEN(cbrace))
	auto struct_name = GETNODE(identifier);
	std::vector<syntax::node::designated_initialiser> inits = {};
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name, std::make_unique<syntax::node::designated_initialiser_list>(inits));
	return {.t = result::type::reduce_success};
CHORD_END

// iden{desig-init}
// struct initialiser (single initialiser)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), NODE(designated_initialiser), TOKEN(cbrace))
	auto struct_name = GETNODE(identifier);
	SETINDEX(2);
	std::vector<syntax::node::designated_initialiser> inits = {};
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name, std::make_unique<syntax::node::designated_initialiser_list>(inits));
	return {.t = result::type::reduce_success};
CHORD_END

// iden{desig-init-list}
// struct initialiser (multiple initialisers)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), NODE(designated_initialiser_list), TOKEN(cbrace))
	auto struct_name = GETNODE(identifier);
	SETINDEX(2);
	auto inits = GETNODE(designated_initialiser_list);
	REDUCE_TO(expression, syntax::node::expression::type::struct_initialiser, struct_name, inits);
	return {.t = result::type::reduce_success};
CHORD_END

// iden::iden
// namespace access
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), NODE(identifier))
	auto lhs = GETNODE(identifier);
	SETINDEX(2);
	auto rhs = GETNODE(identifier);
	REDUCE_TO(namespace_access, lhs, syntax::node::expression{syntax::node::expression::type::identifier, rhs});
	return {.t = result::type::reduce_success};
CHORD_END

// iden::namespace-access
// nested namespace access
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), NODE(namespace_access))
	auto lhs = GETNODE(identifier);
	SETINDEX(2);
	auto rhs = GETNODE(namespace_access);
	REDUCE_TO(namespace_access, lhs, rhs);
	return {.t = result::type::reduce_success};
CHORD_END