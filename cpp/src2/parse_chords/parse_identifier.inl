// iden : iden
// explicitly-typed variable declaration with no initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(col), NODE(identifier))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::identifier type_name = GETNODE(identifier);
	REDUCE_TO(variable_decl, name, type_name, syntax::expression{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden ::= expr
// weakly-typed variable declaration with an initialiser.
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(eq), NODE(capped_expression))
	
	syntax::identifier name = GETNODE(identifier);
	SETINDEX(3);
	auto initialiser = GETNODE(capped_expression);
	REDUCE_TO(capped_variable_decl, name, syntax::identifier{syntax::inferred_typename}, initialiser);
	return {.t = result::type::reduce_success};
CHORD_END


// iden :: (variable-decl-list) -> iden
// function declaration
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen), NODE(variable_decl_list), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::variable_decl_list params = GETNODE(variable_decl_list);
	SETINDEX(6);
	syntax::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, params, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: () -> iden
// function declaration with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen),TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(5);
	syntax::identifier return_type_name = GETNODE(identifier);

	REDUCE_TO(function_decl, name, syntax::variable_decl_list{}, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: (variable-decl) -> iden
// function declaration with one arg
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(oparen), NODE(variable_decl), TOKEN(cparen), TOKEN(arrow), NODE(identifier))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(3);
	syntax::variable_decl param = GETNODE(variable_decl);
	SETINDEX(6);
	syntax::identifier return_type_name = GETNODE(identifier);
	std::vector<syntax::variable_decl> params;
	params.push_back(param);

	REDUCE_TO(function_decl, name, params, return_type_name);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr-list)
// function call (multiple arguments)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression_list), TOKEN(cparen))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::expression_list params = GETNODE(expression_list);

	REDUCE_TO(function_call, name, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr)
// function call (single argument)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression), TOKEN(cparen))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(2);
	std::vector<syntax::expression> params = {};
	params.push_back(GETNODE(expression));

	REDUCE_TO(function_call, name, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden()
// function call with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), TOKEN(cparen))

	syntax::identifier name = GETNODE(identifier);

	REDUCE_TO(function_call, name, syntax::expression_list{});
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: namespace
// namespace meta region
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_namespace))
	syntax::identifier name = GETNODE(identifier);
	REDUCE_TO(meta_region, name, syntax::meta_region::type::name_space);
	return {.t = result::type::reduce_success};
CHORD_END

// iden :: alias := expr;
// type alias
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), TOKEN(keyword_alias), TOKEN(col), TOKEN(eq), NODE(expression), TOKEN(semicol))
	syntax::identifier name = GETNODE(identifier);
	SETINDEX(5);
	syntax::expression expr = GETNODE(expression);
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
	auto result_struct = syntax::struct_decl{name, true};
	result_struct.children.push_back(syntax::node{.payload = blk});
	REDUCE_TO(capped_struct_decl, result_struct);
	return {.t = result::type::reduce_success};
CHORD_END

// iden{}
// struct initialiser (no initialisers)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), TOKEN(cbrace))
	auto struct_name = GETNODE(identifier);
	std::vector<syntax::designated_initialiser> inits = {};
	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, struct_name, syntax::designated_initialiser_list(inits));
	return {.t = result::type::reduce_success};
CHORD_END

// iden{desig-init}
// struct initialiser (single initialiser)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), NODE(designated_initialiser), TOKEN(cbrace))
	auto struct_name = GETNODE(identifier);
	SETINDEX(2);
	std::vector<syntax::designated_initialiser> inits = {};
	inits.push_back(GETNODE(designated_initialiser));
	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, struct_name, syntax::designated_initialiser_list{inits});
	return {.t = result::type::reduce_success};
CHORD_END

// iden{desig-init-list}
// struct initialiser (multiple initialisers)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), NODE(designated_initialiser_list), TOKEN(cbrace))
	auto struct_name = GETNODE(identifier);
	SETINDEX(2);
	auto inits = GETNODE(designated_initialiser_list);
	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, struct_name, inits);
	return {.t = result::type::reduce_success};
CHORD_END

// iden::iden
// namespace access
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(colcol), NODE(identifier))
	auto lhs = GETNODE(identifier);
	SETINDEX(2);
	auto rhs = GETNODE(identifier);
	REDUCE_TO(namespace_access, lhs, syntax::expression{syntax::expression::type::identifier, rhs});
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

// iden#iden
// token concatenation
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(hash), NODE(identifier))
	auto lhs = GETNODE(identifier);
	SETINDEX(2);
	auto rhs = GETNODE(identifier);
	REDUCE_TO(identifier, lhs.iden + '#' + rhs.iden);
	return {.t = result::type::reduce_success};
CHORD_END