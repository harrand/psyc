#include "semal.hpp"
#include "type.hpp"
#include "builtin.hpp"
#include "util.hpp"

namespace semal
{
	const srcloc& context::location() const
	{
		return tree->get(this->path).meta;
	}

	void output::combine(const output& rhs)
	{
		// add functions, global variables, structs etc... to *this
		for(const auto& [name, fn] : rhs.functions)
		{
			if(this->functions.contains(name))
			{
				diag::error(error_code::semal, "at: {}: duplicate definition of function \"{}\" (previously defined in different file at: {})", this->functions.at(name).ctx.location().to_string(), fn.name, fn.ctx.location().to_string());
			}
			this->functions[name] = fn;
		}

		for(const auto& [name, gvar] : rhs.global_variables)
		{
			if(this->global_variables.contains(name))
			{
				diag::error(error_code::semal, "at: {}: duplicate definition of global variable \"{}\" (previously defined in different file at: {})", this->functions.at(name).ctx.location().to_string(), gvar.name, gvar.ctx.location().to_string());
			}
			this->global_variables[name] = gvar;
		}

		for(const auto& [name, structdata] : rhs.struct_decls)
		{
			if(this->struct_decls.contains(name))
			{
				diag::error(error_code::semal, "at: {}: duplicate definition of struct \"{}\" (previously defined at: {})", this->functions.at(name).ctx.location().to_string(), structdata.ty.name, structdata.ctx.location().to_string());
			}
			this->struct_decls[name] = structdata;
		}
	}

	type output::get_type_from_name(std::string type_name)
	{
		type ret = type::undefined();
		std::string cur_type = "";
		auto check_const = [&cur_type, &ret, &type_name](){if(type_name == "const"){ret.qualifiers = static_cast<type_qualifier>(static_cast<int>(ret.qualifiers) | qualifier_const);}};
		for(std::size_t i = 0; i < type_name.size(); i++)
		{
			char c = type_name[i];
			if(c == '&')
			{
				check_const();
				ret = get_type_from_name(cur_type).pointer_to();
				cur_type = "";
			}
			else if(c == ' ')
			{
				check_const();
				ret = get_type_from_name(cur_type);
				cur_type = "";
			}
			else
			{
				cur_type += c;
			}
		}
		if(cur_type.size())
		{
			std::string without_qualifiers = cur_type;
			auto iter = without_qualifiers.find("const");
			if(iter != std::string::npos)
			{
				without_qualifiers.erase(iter, sizeof("const") - 1); // -1 as sizeof includes null terminator
			}
			type_qualifier quals = iter != std::string::npos ? qualifier_const : qualifier_none;
			if(!ret.is_undefined())
			{
				ret.qualifiers = quals;
			}
			if(ret.is_undefined())
			{
				// try a struct.
				for(const auto& [struct_name, structdata] : this->struct_decls)
				{
					if(cur_type == struct_name)
					{
						ret = type::from_struct(structdata.ty, quals);
						break;
					}
				}
			}
			if(ret.is_undefined())
			{
				// try a primitive.
				for(int i = 0; i < static_cast<int>(primitive_type::_count); i++)
				{
					if(cur_type == primitive_type_names[i])
					{
						ret = type::from_primitive(static_cast<primitive_type>(i), quals);
						break;
					}
				}
			}
		}
		return ret;
	}

	void output::register_function(function_t fn)
	{
		if(this->functions.contains(fn.name))
		{
			fn.ctx.semal_error("redefinition of function \"{}\". previously defined in same file at: {}", fn.name, this->functions.at(fn.name).ctx.location().to_string());
		}
		this->functions[fn.name] = fn;
	}

	void output::register_global_variable(local_variable_t gvar)
	{
		if(this->global_variables.contains(gvar.name))
		{
			gvar.ctx.semal_error("redeclaration of global variable \"{}\". previously defined in same file at: {}", gvar.name, this->global_variables.at(gvar.name).ctx.location().to_string());
		}
		this->global_variables[gvar.name] = gvar;
	}

	void output::register_local_variable(local_variable_t var)
	{
		ast::path_view_t ctx = var.ctx.path;
		scope_reference* current_scope = &this->variables;
		if(ctx.size())
		{
			for(std::size_t i : ctx.subspan(0, ctx.size() - 1))
			{
				current_scope = &current_scope->children[i];
			}
		}
		current_scope->variables[var.name] = var;
	}

	void output::register_struct(struct_t structdata)
	{
		if(this->struct_decls.contains(structdata.ty.name))
		{
			structdata.ctx.semal_error("redeclaration of struct \"{}\". previously defined in same file at: {}", structdata.ty.name, this->struct_decls.at(structdata.ty.name).ctx.location().to_string());
		}
		this->struct_decls[structdata.ty.name] = structdata;
	}

	const function_t* output::try_find_function(const char* name) const
	{
		for(const auto& [fnname, funcdata] : this->functions)
		{
			if(fnname == name)
			{
				return &funcdata;
			}
		}
		builtin maybe_builtin = try_find_builtin(name);
		if(maybe_builtin != builtin::_undefined)
		{
			return &get_builtin_function(maybe_builtin);
		}
		return nullptr;
	}

	const function_t* output::try_find_parent_function(const ast& tree, ast::path_t path) const
	{
		// slowly check parent node of the path until we hit a function declaration.	
		if(path.size() <= 1)
		{
			// we're in the top scope, so cant possibly be in a function.
			return nullptr;
		}
		// get initial parent by popping the path first (no need to check the node provided, just its ancestors).
		path.pop_back();
		while(path.size())
		{
			const ast::node& ancestor = tree.get(path);
			if(std::holds_alternative<ast::function_definition>(ancestor.payload))
			{
				const std::string& function_name = std::get<ast::function_definition>(ancestor.payload).func_name;
				const function_t* found_func = this->try_find_function(function_name.c_str());
				if(found_func == nullptr)
				{
					diag::ice("at: {}: internal compiler error: found a parent function of an AST node (named \"{}\"), but could not then retrieve the function data from semantic analysis state.", ancestor.meta.to_string(), function_name);
				}
				return found_func;
			}
			path.pop_back();
		}
		return nullptr;	
	}

	const local_variable_t* output::try_find_global_variable(const char* name) const
	{
		for(const auto& [varname, gvar] : this->global_variables)
		{
			if(varname == name)
			{
				return &gvar;
			}
		}
		return nullptr;
	}

	const local_variable_t* output::try_find_local_variable(const ast::path_t& context, const char* name) const
	{
		ast::path_const_view_t cv = context;
		const scope_reference* current_scope = &this->variables;
		if(context.size())
		{
			for(std::size_t idx : context)
			{
				auto iter = current_scope->variables.find(name);
				if(iter != current_scope->variables.end())
				{
					return &iter->second;
				}
				current_scope = &const_cast<std::unordered_map<std::size_t, scope_reference>&>(current_scope->children)[idx];
			}
		}
		return nullptr;

	}

	const struct_t* output::try_find_struct(const char* name) const
	{
		for(const auto& [structname, structdata] : this->struct_decls)
		{
			if(structname == name)
			{
				return &structdata;
			}
		}
		return nullptr;
	}

	output analyse_predecl(ast tree)
	{
		output ret;
		auto semal_assert = [&tree](ast::path_t path, bool expr, const char* fmt, auto... ts)
		{
			context{&tree, path}.semal_assert(expr, fmt, ts...);
		};

		for(std::size_t i = 0; i < tree.root.children.size(); i++)
		{
			const ast::node& node = tree.root.children[i];
			const ast::path_t path{i};
			if(std::holds_alternative<ast::struct_definition>(node.payload))
			{
				// cool, go through node children to get the data members.
				const auto& data = std::get<ast::struct_definition>(node.payload);
				struct_t ty;
				ty.ty.name = data.name;
				ty.ctx = {.tree = &tree, .path = path};
				// go through its children.
				auto block_node = node.children.front();
				semal_assert(path, std::holds_alternative<ast::block>(block_node.payload), "struct definition should *always* be followed by an implementation block!");
				const auto& blk = std::get<ast::block>(block_node.payload);
				auto block_path = path;
				block_path.push_back(0);
				for(std::size_t i = 0; i < block_node.children.size(); i++)
				{
					const ast::node& child = block_node.children[i];
					auto child_path = block_path;
					child_path.push_back(i);
					if(std::holds_alternative<ast::expression>(child.payload))
					{
						const auto& expr = std::get<ast::expression>(child.payload);	
						if(std::holds_alternative<ast::variable_declaration>(expr.expr))
						{
							const auto& decl = std::get<ast::variable_declaration>(expr.expr);	
							type param_type = ret.get_type_from_name(decl.type_name);
							semal_assert(child_path, !param_type.is_undefined(), "could not decipher type of {}::{} (type: \"{}\"). if \"{}\" is meant to be a struct, it should be defined before this struct.", data.name, decl.var_name, decl.type_name, decl.type_name);
							ty.ty.data_members.push_back
							({
								.member_name = decl.var_name,
								.ty = param_type
							});
						}
						else
						{
							semal_assert(child_path, false, "every expression within a struct definition block should be a variable declaration. these token(s) represent neither.");
						}
					}
					else if(std::holds_alternative<ast::function_definition>(child.payload))
					{
						auto decl = std::get<ast::function_definition>(child.payload);

						diag::error(error_code::nyi, "at: {}: attempt to define a method {}::{}", child.meta.to_string(), data.name, decl.func_name);
						// this is a method.
						/*
						function_t fn;
						fn.is_method = true;
						fn.name = ast::mangle_method_name(data.struct_name, decl.function_name);
						auto child_path = path;
						child_path.push_back(i);
						fn.context = child_path;
						fn.params.push_back({
							.ty = type::from_struct(ty.ty).pointer_to(),
							.name = "this",
							.context = path
						});
						fn.return_ty = this->get_type_from_name(decl.return_type).first;
						for(const auto& param : decl.params)
						{
							type param_type = this->get_type_from_name(param.type_name).first;
							if(param_type.is_undefined())
							{
								this->last_error = std::format("semal error on line {} - could not decipher type of method parameter {} (typename: {}). if its a struct, it must be defined before this struct.", node.meta.line_number, param.var_name, param.type_name);
								return;
							}
							fn.params.push_back
							(local_variable_t{
								.ty = param_type,
								.name = param.var_name,
								.context = path,
							});
						}
						if(this->functions.contains(fn.name))
						{
							std::size_t previously_defined_on_line = tree.get(this->functions.at(fn.name).context).meta.line_number;
							this->last_error = std::format("semal error on line {} - double definition of function {} (previous definition on line {})", node.meta.line_number, fn.name, previously_defined_on_line);
							return;
						}
						this->register_function(fn);
						ty.methods[decl.function_name] = fn;
						*/
					}
					else
					{
						semal_assert(child_path, false, "everything within a struct definition block should be either a variable declaration (via expression) or a function definition representing a method. these token(s) represent neither.");
					}
				}
				if(ret.struct_decls.contains(ty.ty.name))
				{
					semal_assert(path, false, "double definition of struct \"{}\" (previously defined at: {})", ty.ty.name, ret.struct_decls.at(ty.ty.name).ctx.location().to_string());
				}
				ret.register_struct(ty);
			}
		}

		// then, functions and globals can be done at the same time.
		for(std::size_t i = 0; i < tree.root.children.size(); i++)
		{
			const ast::node& node = tree.root.children[i];
			const ast::path_t path{i};
			if(std::holds_alternative<ast::function_definition>(node.payload))
			{
				function_t fn;
				auto func = std::get<ast::function_definition>(node.payload);	
				if(func.is_extern)
				{
					semal_assert(path, node.children.empty(), "detected extern function \"{}\" with child AST nodes, implying it has a body. extern functions cannot have implementation blocks.", func.func_name);
				}
				fn.name = func.func_name;
				fn.ctx = {&tree, path};
				fn.return_ty = ret.get_type_from_name(func.ret_type);
				semal_assert(path, !fn.return_ty.is_undefined(), "unknown return-type \"{}\"", func.ret_type);
				for(const auto& param : func.params)
				{
					type param_type = ret.get_type_from_name(param.type_name);
					semal_assert(path, !param_type.is_undefined(), "unknown type \"{}\" of parameter \"{}\" within function \"{}\"", param.type_name, param.var_name, func.func_name);
					fn.params.push_back
					(local_variable_t{
						.ty = param_type,
						.name = param.var_name,
						.ctx = {&tree, path},
					});
				}
				ret.register_function(fn);
			}

			if(std::holds_alternative<ast::expression>(node.payload))
			{
				auto expr = std::get<ast::expression>(node.payload);
				if(std::holds_alternative<ast::variable_declaration>(expr.expr))
				{
					// global
					auto gvar = std::get<ast::variable_declaration>(expr.expr);
					/*
					if(gvar.array_size != 0)
					{
						this->last_error = std::format("semal error on line {} - detected global variable of array type. this is not yet implemented.", node.meta.line_number);
						return;
					}
					*/
					local_variable_t gv;
					gv.ctx = {&tree, path};
					gv.name = gvar.var_name;
					type var_type = ret.get_type_from_name(gvar.type_name);
					semal_assert(path, !var_type.is_undefined(), "unknown type \"{}\" of global variable \"{}\"", gvar.type_name, gvar.var_name);
					gv.ty = var_type;
					ret.register_global_variable(gv);
				}
			}
		}
		// analyse function declarations, structs and global variables.
		// do *not* analyse function/method implementations.
		return ret;
	}

	struct data
	{
		output& state;
		ast::path_t path;
		const ast& tree;

		const srcloc& meta() const
		{
			return this->tree.get(path).meta;
		}

		void fatal_error(std::string msg) const
		{
			diag::error(error_code::semal, "at: {}: {}", this->meta().to_string(), msg);
		}

		void internal_error(std::string msg) const
		{
			diag::error(error_code::ice, "at: {}: {}", this->meta().to_string(), msg);
		}

		void warning(std::string msg) const
		{
			diag::warning("at: {}: {}", this->meta().to_string(), msg);
		}

		void assert_that(bool expr, std::string msg) const
		{
			if(!expr)
			{
				this->fatal_error(msg);	
			}
		}
	};

	template<typename T>
	type generic(const data& d, T payload);

	output analyse_full(const ast& tree, output predecl)
	{
		output ret = predecl;
		for(std::size_t i = 0; i < tree.root.children.size(); i++)
		{
			const ast::node& node = tree.root.children[i];
			const ast::path_t path{i};
			generic({.state = ret, .path = path, .tree = tree}, tree.get(path).payload);
			// analyse all function implementation blocks.
			if(std::holds_alternative<ast::function_definition>(node.payload))
			{
				auto func = std::get<ast::function_definition>(node.payload);	
				diag::assert_that(predecl.functions.contains(func.func_name), error_code::ice, "function \"{}\" was not found in the predecl semal output, when this should always be the case.", func.func_name);

				function_t fn = predecl.functions[func.func_name];
				if(!func.is_extern)
				{
					fn.ctx.semal_assert(node.children.size() == 1, "non-extern functions must have an implementation.");
					fn.ctx.semal_assert(std::holds_alternative<ast::block>(node.children.front().payload), "function implementation should consist of a block of code surrounded by braces");

					auto block_path = path;
					block_path.push_back(0);
					const auto& block_node = node.children.front();

					for(std::size_t c = 0; c < block_node.children.size(); c++)
					{
						auto child_path = block_path;
						child_path.push_back(c);
						generic({.state = ret, .path = child_path, .tree = tree}, tree.get(child_path).payload);
					}
				}
			}
		}
		return ret;
	}

	type output::get_type_from_payload(const ast::node::payload_t& payload, const ast& tree, const ast::path_t& path) const
	{
		return generic({.state = const_cast<output&>(*this), .path = path, .tree = tree}, payload);
	}

	type expression(const data& d, const ast::expression& payload)
	{
		return generic(d, payload.expr);
	}

	type if_statement(const data& d, const ast::if_statement& payload)
	{
		type if_expr_ty = expression(d, *payload.if_expr);
		d.assert_that(if_expr_ty.is_primitive() && if_expr_ty.as_primitive() == primitive_type::boolean, std::format("expression of if-statement operand must be a boolean, you passed a {} you stupid cunt", if_expr_ty.name()));
		return type::undefined();
	}

	// node types.
	type binary_operator(const data& d, const ast::binary_operator& payload)
	{
		type lhs = expression(d, *payload.lhs_expr);
		type rhs = expression(d, *payload.rhs_expr);
		switch(payload.op.t)
		{
			case lex::type::operator_equals:
				d.assert_that(lhs == rhs, std::format("cannot assign from type \"{}\" to a type \"{}\"", lhs.name(), rhs.name()));
				return rhs;
			break;
			case lex::type::operator_asterisk:
			[[fallthrough]];
			case lex::type::operator_slash:
			[[fallthrough]];
			case lex::type::operator_plus:
			[[fallthrough]];
			case lex::type::operator_minus:
				d.assert_that(lhs == rhs, std::format("both sides of a \"{}\" binary operation must have matching types - passed \"{}\" and \"{}\"", payload.op.lexeme, lhs.name(), rhs.name()));
				d.assert_that(lhs.is_integer_type() || lhs.is_floating_point_type(), std::format("both sides of a \"{}\" binary operator must both be numeric types (e.g any integer or floating point type). you passed \"{}\" ({} numeric) and \"{}\" ({} numeric)", payload.op.lexeme, lhs.name(), lhs.is_integer_type() || lhs.is_floating_point_type() ? "is" : "isn't", rhs.name(), rhs.is_integer_type() || rhs.is_floating_point_type() ? "is" : "isn't"));
				return rhs;
			break;
			case lex::type::operator_double_equals:
				d.assert_that(lhs == rhs, std::format("both sides of a \"{}\" binary operation must have matching types - passed \"{}\" and \"{}\"", payload.op.lexeme, lhs.name(), rhs.name()));
				return type::from_primitive(primitive_type::boolean);
			break;
			default:
				d.internal_error(std::format("unknown binary operator token \"{}\"", payload.op.lexeme));
			break;
		}
		return type::undefined();
	}

	type unary_operator(const data& d, const ast::unary_operator& payload)
	{
		type expr = expression(d, *payload.expr);
		switch(payload.op.t)
		{
			case lex::type::operator_ref:
				return expr.pointer_to();
			break;
			case lex::type::operator_deref:
				d.assert_that(expr.is_pointer(), std::format("cannot deref non-pointer type \"{}\"", expr.name()));
				return expr.dereference();
			break;
			case lex::type::operator_minus:
				d.assert_that(expr.is_integer_type() || expr.is_floating_point_type(), std::format("unary operator \"{}\" can only be used on either a floating-point or integral-type. you provided a \"{}\"", payload.op.lexeme, expr.name()));
				d.assert_that(!expr.is_unsigned_integer_type(), std::format("unary operator\"{}\" cannot be used on the unsigned integer type \"{}\"", payload.op.lexeme, expr.name()));
				return expr;
			break;
			default:
				d.internal_error(std::format("unknown unary operator token \"{}\"", payload.op.lexeme));
			break;
		}
		return type::undefined();
	}

	type identifier(const data& d, const ast::identifier& payload)
	{
		const local_variable_t* maybe_global_variable = d.state.try_find_global_variable(payload.iden.c_str());
		if(maybe_global_variable != nullptr)
		{
			return maybe_global_variable->ty;
		}
		
		const local_variable_t* maybe_local_variable = d.state.try_find_local_variable(d.path, payload.iden.c_str());
		if(maybe_local_variable != nullptr)
		{
			return maybe_local_variable->ty;
		}

		const function_t* func = d.state.try_find_parent_function(d.tree, d.path);
		if(func != nullptr)
		{
			for(const auto& param : func->params)
			{
				if(param.name == payload.iden)
				{
					return param.ty;
				}
			}
		}

		d.fatal_error(std::format("undeclared variable \"{}\".", payload.iden));
		return type::undefined();
	}

	type function_call(const data& d, const ast::function_call& payload)
	{
		const function_t* maybe_function = d.state.try_find_function(payload.function_name.c_str());
		d.assert_that(maybe_function != nullptr, std::format("call to undeclared function \"{}\"", payload.function_name));
		return maybe_function->return_ty;
	}

	type return_statement(const data& d, const ast::return_statement& payload)
	{
		const function_t* maybe_parent = d.state.try_find_parent_function(d.tree, d.path);
		d.assert_that(maybe_parent != nullptr, "detected return statement outside of a function. return statements are only valid within function implementation blocks.");
		if(payload.expr.has_value())
		{
			type ret = expression(d, *payload.expr.value());
			d.assert_that(!maybe_parent->return_ty.is_void(), std::format("return statement was an expression of type \"{}\", but the enclosing function \"{}\" returns {}. do not return an expression.", ret.name(), maybe_parent->name, maybe_parent->return_ty.name()));
			d.assert_that(ret == maybe_parent->return_ty, std::format("type of `return` statement \"{}\" does not match the return-type of the enclosing function \"{}\", which is a \"{}\"", ret.name(), maybe_parent->name, maybe_parent->return_ty.name()));
			return ret;
		}
		d.assert_that(maybe_parent->return_ty.is_void(), std::format("detected empty `return` statement within function \"{}\" which doesn't return u0. `return;` is only valid in functions that return `u0`.", maybe_parent->name));
		return type::from_primitive(primitive_type::u0);
	}

	type variable_declaration(const data& d, const ast::variable_declaration& payload)
	{
		if(d.path.size() <= 1)
		{
			// we already sorted out global variables.
			return d.state.get_type_from_name(payload.type_name);
		}
		auto global_already = d.state.try_find_global_variable(payload.var_name.c_str());
		if(global_already != nullptr)
		{
			d.assert_that(false, std::format("variable named \"{}\" would shadow a global variable (defined at: {})", payload.var_name, global_already->ctx.location().to_string()));
		}
		auto local_already = d.state.try_find_local_variable(d.path, payload.var_name.c_str());
		if(local_already != nullptr)
		{
			d.assert_that(false, std::format("variable named \"{}\" would shadow a previously-defined local variable (defined at: {})", payload.var_name, local_already->ctx.location().to_string()));
		}

		const auto& ty = d.state.get_type_from_name(payload.type_name);
		d.assert_that(!ty.is_undefined(), std::format("undefined type \"{}\"", payload.type_name));
		d.state.register_local_variable
		({
			.ty = ty,
			.name = payload.var_name,
			.ctx = {.tree = &d.tree, .path = d.path}
		});

		if(payload.initialiser.has_value())
		{
			type expr_ty = expression(d, *payload.initialiser.value());
			d.assert_that(expr_ty == ty, std::format("initialiser of variable \"{}\" is of type \"{}\", which does not match the variable's type of \"{}\". perhaps you forgot to insert a cast?", payload.var_name, expr_ty.name(), ty.name()));
		}
		return ty;
	}

	type function_definition(const data& d, const ast::function_definition& payload)
	{
		if(d.path.size() > 1)
		{
			// not at the top-level scope.
			// its parent better be a struct (making this a method).
			// if its not, then the code is ill-formed.
			auto parent_path = d.path;
			parent_path.pop_back();
			const ast::node& parent = d.tree.get(parent_path);
			/*
			d.assert_that(std::holds_alternative<ast::struct_definition>(parent.payload), std::format("detected a non-method function definition \"{}\" within another block. functions can only be defined at the top-level scope.", payload.function_name));
			*/
			d.fatal_error("methods are not yet implemented, or this function is not defined at top-level scope.");
		}
		return d.state.get_type_from_name(payload.ret_type);
	}

	type struct_definition(const data& d, const ast::struct_definition& payload)
	{
		if(d.path.size() > 1)
		{
			// not at the top-level scope.
			// its parent better be a struct (making this a method).
			// if its not, then the code is ill-formed.
			auto parent_path = d.path;
			parent_path.pop_back();
			const ast::node& parent = d.tree.get(parent_path);
			/*
			d.assert_that(std::holds_alternative<ast::struct_definition>(parent.payload), std::format("detected a non-method function definition \"{}\" within another block. functions can only be defined at the top-level scope.", payload.function_name));
			*/
			d.fatal_error("structs must be defined at the top-level scope.");
		}
		return d.state.get_type_from_name(payload.name);
	}

	type meta_region(const data& d, const ast::meta_region& payload)
	{
		return type::undefined();
	}

	template<typename T>
	type generic(const data& d, T payload)
	{
		type ret = type::undefined();

		auto dispatch = util::overload
		{
			[&](ast::integer_literal lit)
			{
				ret = type::from_primitive(primitive_type::i64);
			},
			[&](ast::decimal_literal lit)
			{
				ret = type::from_primitive(primitive_type::f64);
			},
			[&](ast::bool_literal lit)
			{
				ret = type::from_primitive(primitive_type::boolean);
			},
			/*
			[&](ast::char_literal lit)
			{
				ret = type::from_primitive(primitive_type::i8);
			},
			[&](ast::string_literal lit)
			{
				ret = type::from_primitive(primitive_type::i8).pointer_to();
			},
			*/
			[&](ast::binary_operator op)
			{
				ret = binary_operator(d, op);
				//d.fatal_error("dispatch error");
			},
			[&](ast::unary_operator op)
			{
				ret = unary_operator(d, op);
				//d.fatal_error("dispatch error");
			},
			[&](ast::block blk)
			{
			},
			[&](ast::identifier id)
			{
				ret = identifier(d, id);
			},
			[&](ast::function_call call)
			{
				ret = function_call(d, call);
			},
			/*
			[&](ast::member_access mem)
			{
				ret = member_access(d, mem);
			},
			*/
			[&](ast::expression expr)
			{
				ret = expression(d, expr);
			},
			[&](ast::if_statement ifst)
			{
				ret = if_statement(d, ifst);
			},
			/*
			[&](ast::else_statement elst)
			{
				ret = else_statement(d, elst);
			},
			[&](ast::for_statement forst)
			{
				ret = for_statement(d, forst);
			},
			*/
			[&](ast::return_statement returnst)
			{
				ret = return_statement(d, returnst);
			},
			[&](ast::variable_declaration decl)
			{
				ret = variable_declaration(d, decl);
			},
			[&](ast::function_definition fdef)
			{
				ret = function_definition(d, fdef);
			},
			[&](ast::struct_definition sdef)
			{
				ret = struct_definition(d, sdef);
			},
			[&](ast::meta_region meta)
			{
				ret = meta_region(d, meta);
			},
			[&](auto)
			{
				d.fatal_error("unknown node payload.");
			}
		};

		std::visit(dispatch, payload);
		return ret;
	}
}