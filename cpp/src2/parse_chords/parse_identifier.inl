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

// iden<expr-list>(expr-list)
// function call (multiple arguments, multiple constparams)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oanglebrack), NODE(expression_list), TOKEN(canglebrack), TOKEN(oparen), NODE(expression_list), TOKEN(cparen))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::expression_list constparams = GETNODE(expression_list);
	SETINDEX(5);
	syntax::expression_list params = GETNODE(expression_list);

	REDUCE_TO(function_call, name, constparams, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden(expr-list)
// function call (multiple arguments, constparams omitted)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), NODE(expression_list), TOKEN(cparen))

	syntax::identifier name = GETNODE(identifier);
	SETINDEX(2);
	syntax::expression_list params = GETNODE(expression_list);

	REDUCE_TO(function_call, name, syntax::expression_list{}, params);
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

	REDUCE_TO(function_call, name, syntax::expression_list{}, params);
	return {.t = result::type::reduce_success};
CHORD_END

// iden()
// function call with no args
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oparen), TOKEN(cparen))

	syntax::identifier name = GETNODE(identifier);

	REDUCE_TO(function_call, name, syntax::expression_list{}, syntax::expression_list{});
	return {.t = result::type::reduce_success};
CHORD_END

// struct <variable-decl> block
// create a struct with a single constparam
CHORD_BEGIN
	STATE(TOKEN(keyword_struct), TOKEN(oanglebrack), NODE(capped_variable_decl), TOKEN(canglebrack), NODE(block))

	SETINDEX(2);
	syntax::variable_decl_list constparams;
	constparams.decls.push_back(GETNODE(capped_variable_decl));

	SETINDEX(4);
	auto blk = GETNODE(block);

	auto result_struct = syntax::capped_struct_decl{};
	result_struct.children.push_back(syntax::node{.payload = blk});
	result_struct.constparams = constparams;

	REDUCE_TO(expression, syntax::expression::type::struct_decl, result_struct);
	return {.t = result::type::reduce_success};
CHORD_END

// struct block
// create a struct but omitting constparams
CHORD_BEGIN
	STATE(TOKEN(keyword_struct), NODE(block))

	SETINDEX(1);
	auto blk = GETNODE(block);
	auto result_struct = syntax::capped_struct_decl{};
	result_struct.children.push_back(syntax::node{.payload = blk});
	REDUCE_TO(expression, syntax::expression::type::struct_decl, result_struct);
	return {.t = result::type::reduce_success};
CHORD_END

// struct {}
// create an empty struct but omitting constparams
CHORD_BEGIN
	STATE(TOKEN(keyword_struct), TOKEN(obrace), TOKEN(cbrace))

	auto result_struct = syntax::capped_struct_decl{};
	REDUCE_TO(expression, syntax::expression::type::struct_decl, result_struct);
	return {.t = result::type::reduce_success};
CHORD_END

// iden{}
// struct initialiser (no initialisers, constinits omitted)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), TOKEN(cbrace))
	syntax::struct_initialiser structinit;
	structinit.struct_name = GETNODE(identifier);

	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, structinit);
	return {.t = result::type::reduce_success};
CHORD_END

// iden<desig-init>{}
// struct initialiser (no initialisers, one constinit)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(oanglebrack), NODE(designated_initialiser), TOKEN(canglebrack), TOKEN(obrace), TOKEN(cbrace))

	syntax::struct_initialiser structinit;
	structinit.struct_name = GETNODE(identifier);

	SETINDEX(2);
	structinit.constinits.inits.push_back(GETNODE(designated_initialiser));

	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, structinit);
	return {.t = result::type::reduce_success};
CHORD_END

// iden{desig-init}
// struct initialiser (single initialiser, constinits omitted)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), NODE(designated_initialiser), TOKEN(cbrace))

	syntax::struct_initialiser structinit;
	structinit.struct_name = GETNODE(identifier);

	SETINDEX(2);
	structinit.inits.inits.push_back(GETNODE(designated_initialiser));

	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, structinit);
	return {.t = result::type::reduce_success};
CHORD_END

// iden{desig-init-list}
// struct initialiser (multiple initialisers, constinits omitted)
CHORD_BEGIN
	STATE(NODE(identifier), TOKEN(obrace), NODE(designated_initialiser_list), TOKEN(cbrace))

	syntax::struct_initialiser structinit;
	structinit.struct_name = GETNODE(identifier);
	SETINDEX(2);
	structinit.inits = GETNODE(designated_initialiser_list);

	REDUCE_TO(expression, syntax::expression::type::struct_initialiser, structinit);
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