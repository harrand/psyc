#include "semal.hpp"
#include "type.hpp"
#include "builtin.hpp"
#include "util.hpp"

namespace semal
{
	const ast::node& context::node() const
	{
		return this->tree->get(this->path);
	}
	const srcloc& context::location() const
	{
		return this->node().meta;
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

	type output::get_type_from_name(std::string type_name) const
	{
		type ret = type::undefined();
		std::string cur_type = "";
		for(std::size_t i = 0; i < type_name.size(); i++)
		{
			if(std::string_view(type_name.data() + i).starts_with(" const"))
			{
				if(ret.is_undefined())
				{
					ret = get_type_from_name(cur_type);
				}
				ret.qualifiers = (type_qualifier)((int)ret.qualifiers | qualifier_const);
				i += sizeof(" const") - 2;
				continue;
			}
			if(std::string_view(type_name.data() + i).starts_with(" weak"))
			{
				if(ret.is_undefined())
				{
					ret = get_type_from_name(cur_type);
				}
				ret.qualifiers = (type_qualifier)((int)ret.qualifiers | qualifier_weak);
				i += sizeof(" weak") - 2;
				continue;
			}
			char c = type_name[i];
			if(c == '&')
			{
				if(ret.is_undefined())
				{
					ret = get_type_from_name(cur_type).pointer_to();
				}
				else
				{
					ret = ret.pointer_to();
				}
				cur_type = "";
			}
			else
			{
				cur_type += c;
			}
		}
		if(cur_type.size())
		{
			if(ret.is_undefined())
			{
				// try a struct.
				for(const auto& [struct_name, structdata] : this->struct_decls)
				{
					if(cur_type == struct_name)
					{
						ret = type::from_struct(structdata.ty);
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
						ret = type::from_primitive(static_cast<primitive_type>(i));
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
			fn.ctx.error(error_code::semal, "redefinition of function \"{}\". previously defined in same file at: {}", fn.name, this->functions.at(fn.name).ctx.location().to_string());
		}
		this->functions[fn.name] = fn;
	}

	void output::register_global_variable(local_variable_t gvar)
	{
		if(this->global_variables.contains(gvar.name))
		{
			gvar.ctx.error(error_code::semal, "redeclaration of global variable \"{}\". previously defined in same file at: {}", gvar.name, this->global_variables.at(gvar.name).ctx.location().to_string());
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
			structdata.ctx.error(error_code::semal, "redeclaration of struct \"{}\". previously defined in same file at: {}", structdata.ty.name, this->struct_decls.at(structdata.ty.name).ctx.location().to_string());
		}
		this->struct_decls[structdata.ty.name] = structdata;
	}

	const function_t* output::try_find_function(std::string_view name) const
	{
		for(const auto& [fnname, funcdata] : this->functions)
		{
			if(fnname == name)
			{
				return &funcdata;
			}
		}
		// how about a class method?
		for(const auto& [structname, structdata] : this->struct_decls)
		{
			for(const auto& [mthname, mthdata] : structdata.methods)
			{
				if(mangle_method_name(mthname) == name)
				{
					return &mthdata;
				}
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
			const ast::node* distant_ancestor = nullptr;
			if(path.size() >= 3)
			{
				auto distant_path = path; distant_path.pop_back(); distant_path.pop_back();
				distant_ancestor = &tree.get(distant_path);
			}
			if(std::holds_alternative<ast::function_definition>(ancestor.payload))
			{
				std::string function_name = std::get<ast::function_definition>(ancestor.payload).func_name;
				bool is_method = distant_ancestor != nullptr && std::holds_alternative<ast::struct_definition>(distant_ancestor->payload);
				if(is_method)
				{
					function_name = semal::mangle_method_name(function_name);
				}
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

	const local_variable_t* output::try_find_global_variable(std::string_view name) const
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

	const local_variable_t* output::try_find_local_variable(const ast::path_t& context, std::string_view name) const
	{
		ast::path_const_view_t cv = context;
		const scope_reference* current_scope = &this->variables;
		if(context.size())
		{
			for(std::size_t idx : context)
			{
				auto iter = current_scope->variables.find(name.data());
				if(iter != current_scope->variables.end())
				{
					return &iter->second;
				}
				current_scope = &const_cast<std::unordered_map<std::size_t, scope_reference>&>(current_scope->children)[idx];
			}
		}
		return nullptr;

	}

	const struct_t* output::try_find_struct(std::string_view name) const
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

	output analyse_predecl(const ast& tree)
	{
		output ret;
		auto semal_assert = [&tree](ast::path_t path, bool expr, const char* fmt, auto... ts)
		{
			context{&tree, path}.assert_that(expr, error_code::semal, fmt, ts...);
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

						//diag::error(error_code::nyi, "at: {}: attempt to define a method {}::{}", child.meta.to_string(), data.name, decl.func_name);
						function_t fn;
						fn.is_method = true;
						fn.method_owner_struct_name = data.name;
						fn.name = decl.func_name;
						fn.ctx = {.tree = &tree, .path = child_path};
						fn.return_ty = ret.get_type_from_name(decl.ret_type);
						for(const auto& param : decl.params)
						{
							type param_type = ret.get_type_from_name(param.type_name);
							semal_assert(child_path, !param_type.is_undefined(), "could not decipher type of method parameter {} (type: \"{}\"). if its a struct, it must be defined before this struct.", param.var_name, param.type_name);	
							fn.params.push_back
							(local_variable_t{
								.ty = param_type,
								.name = param.var_name,
								.ctx = fn.ctx
							});
						}
						if(ret.functions.contains(decl.func_name))
						{
							const function_t& shadowed_fn = ret.functions[decl.func_name];
							diag::warning("at {}: method \"{}.{}\" shares a name with global function {} (defined at: {}). this is not a problem, but something you might want to be aware of.", fn.ctx.location().to_string(), data.name, fn.name, shadowed_fn.name, shadowed_fn.ctx.location().to_string());
						}
						ty.methods[decl.func_name] = fn;
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
			ast::path_t path{i};
			generic({.state = ret, .path = path, .tree = tree}, tree.get(path).payload);
			// analyse all function implementation blocks.
			auto semal_func = [&predecl, &tree, &ret](ast::function_definition func, ast::path_t path, std::optional<std::string> maybe_owner_struct)
			{
				// if maybe owner struct has a value, then we're a method.
				const auto& node = tree.get(path);
				function_t fn;
				if(maybe_owner_struct.has_value())
				{
					diag::assert_that(predecl.struct_decls[maybe_owner_struct.value()].methods.contains(func.func_name), error_code::ice, "method \"{}\" was not found in the predecl semal output for struct \"{}\", when this should always be the case", func.func_name, maybe_owner_struct.value());
					fn = predecl.struct_decls[maybe_owner_struct.value()].methods[func.func_name];
				}
				else
				{
					diag::assert_that(predecl.functions.contains(func.func_name), error_code::ice, "function \"{}\" was not found in the predecl semal output, when this should always be the case.", func.func_name);
					fn = predecl.functions[func.func_name];
				}

				if(!func.is_extern)
				{
					fn.ctx.assert_that(node.children.size() == 1, error_code::semal, "non-extern functions must have an implementation.");
					fn.ctx.assert_that(std::holds_alternative<ast::block>(node.children.front().payload), error_code::semal, "function implementation should consist of a block of code surrounded by braces");

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
				else
				{
					diag::assert_that(!maybe_owner_struct.has_value(), error_code::semal, "methods cannot be extern.");
				}
			};
			if(std::holds_alternative<ast::function_definition>(node.payload))
			{
				//func = std::get<ast::function_definition>(node.payload);
				// do func
				semal_func(std::get<ast::function_definition>(node.payload), path, std::nullopt);
			}
			else if(std::holds_alternative<ast::struct_definition>(node.payload))
			{
				auto structty = std::get<ast::struct_definition>(node.payload);
				auto blk_node = node.children.front();
				path.push_back(0);
				for(std::size_t j = 0; j < blk_node.children.size(); j++)
				{
					const auto& child_node = blk_node.children[j];
					if(std::holds_alternative<ast::function_definition>(child_node.payload))
					{
						auto child_path = path;
						child_path.push_back(j);
						semal_func(std::get<ast::function_definition>(child_node.payload), child_path, structty.name);
					}
				}
			}
		}
		return ret;
	}

	std::string mangle_method_name(std::string method_name)
	{
		return "__method_" + method_name;
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
		d.assert_that(if_expr_ty.is_primitive() && if_expr_ty.as_primitive() == primitive_type::boolean, std::format("expression of if-statement operand must be a boolean, you passed a {}", if_expr_ty.name()));

		const auto& node = d.tree.get(d.path);
		d.assert_that(node.children.size() == 1 || node.children.size() == 2, std::format("if-statement AST node is expected to have *1* child representing its if-true-clause (or *2* children if it's an if-then-else), but the node actually has \"{}\"", node.children.size()));
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			const ast::node& child = node.children[i];
			d.assert_that(std::holds_alternative<ast::block>(child.payload), std::format("child nodes of an if-statement node must only ever be blocks. detected a non-block AST child node."));
			for(std::size_t j = 0; j < child.children.size(); j++)
			{
				const ast::node& grandchild = child.children[j];
				data d2 = d;
				d2.path.push_back(i);
				d2.path.push_back(j);
				generic(d2, grandchild.payload);
			}
		}
		return type::undefined();
	}

	type for_statement(const data& d, const ast::for_statement& payload)
	{
		type cond_expr_ty = expression(d, *payload.cond_expr);
		d.assert_that(cond_expr_ty.is_primitive() && cond_expr_ty.as_primitive() == primitive_type::boolean, std::format("condition-expression (2nd part) of for-statement operand must be a boolean, you passed a {}", cond_expr_ty.name()));
		const auto& node = d.tree.get(d.path);

		expression(d, *payload.init_expr);
		expression(d, *payload.iter_expr);

		d.assert_that(node.children.size() == 1, std::format("for-statement AST node is expected to have *1* child representing its loop block, but the node actually has \"{}\"", node.children.size()));
		const ast::node& child = node.children.front();
		d.assert_that(std::holds_alternative<ast::block>(child.payload), std::format("child nodes of an for-statement node must only ever be blocks. detected a non-block AST child node."));
		for(std::size_t j = 0; j < child.children.size(); j++)
		{
			const ast::node& grandchild = child.children[j];
			data d2 = d;
			d2.path.push_back(0);
			d2.path.push_back(j);
			generic(d2, grandchild.payload);
		}
		return type::undefined();
	}

	type struct_initialiser(const data& d, const ast::struct_initialiser& payload)
	{
		type ret = d.state.get_type_from_name(payload.name);
		d.assert_that(!ret.is_undefined(), std::format("struct initialiser type \"{}\" is not recognised as a valid type.", payload.name));
		d.assert_that(ret.is_struct(), std::format("struct initialiser type \"{}\" is not recognised as a struct type.", payload.name));

		auto try_get_member = [structty = ret.as_struct()](std::string_view member_name)->std::optional<type>
		{
			for(const auto& member : structty.data_members)
			{
				if(member.member_name == member_name)
				{
					return *member.ty;
				}
			}
			return std::nullopt;
		};

		// go through each initialiser and make sure both that the member exists and type checks.
		for(const auto& [member, init] : payload.designated_initialisers)
		{
			auto member_ty = try_get_member(member);
			d.assert_that(member_ty.has_value(), std::format("in designated initialiser for struct \"{}\": no such data member \"{}\"", payload.name, member));
			type expr_ty = expression(d, *init);
			d.assert_that(typecon_valid(member_ty.value().is_implicitly_convertible_to(expr_ty)), std::format("designator \"{}\" for struct \"{}\" is of type \"{}\". the initialiser you passed is of type \"{}\" which is not implicitly convertible. did you forget to cast?", member, payload.name, member_ty->name(), expr_ty.name()));
		}
		return ret;
	}

	// node types.
	type binary_operator(const data& d, const ast::binary_operator& payload)
	{
		type lhs = expression(d, *payload.lhs_expr);
		type rhs = type::undefined();
		if(payload.op.t == lex::type::operator_cast)
		{
			d.assert_that(std::holds_alternative<ast::identifier>(payload.rhs_expr->expr), std::format("in a cast, rhs of the cast token \"{}\" must be an identifier, not an expression or anything else.", payload.op.lexeme));
			std::string type_name = std::get<ast::identifier>(payload.rhs_expr->expr).iden;
			rhs = d.state.get_type_from_name(type_name);
			d.assert_that(typecon_valid(lhs.is_explicitly_convertible_to(rhs)), std::format("cast from \"{}\" to \"{}\" is impossible", lhs.name(), rhs.name()));
			d.assert_that(!rhs.is_undefined(), std::format("unknown cast destination type \"{}\"", type_name));
			// todo: confirm that lhs can actually be casted to rhs.
		}
		else
		{
			rhs = expression(d, *payload.rhs_expr);
		}
		switch(payload.op.t)
		{
			case lex::type::operator_equals:
				d.assert_that(!lhs.is_const(), std::format("lhs of assignment is const: \"{}\"", lhs.name()));
				d.assert_that(typecon_valid(rhs.is_implicitly_convertible_to(lhs)), std::format("cannot assign from type \"{}\" to a type \"{}\"", rhs.name(), lhs.name()));
				return rhs;
			break;
			case lex::type::operator_asterisk:
			[[fallthrough]];
			case lex::type::operator_slash:
			[[fallthrough]];
			case lex::type::operator_plus:
			[[fallthrough]];
			case lex::type::operator_minus:
				d.assert_that(typecon_valid(rhs.is_implicitly_convertible_to(lhs)), std::format("both sides of a \"{}\" binary operation must have matching types - passed \"{}\" and \"{}\"", payload.op.lexeme, lhs.name(), rhs.name()));
				d.assert_that(lhs.is_integer_type() || lhs.is_floating_point_type(), std::format("both sides of a \"{}\" binary operator must both be numeric types (e.g any integer or floating point type). you passed \"{}\" ({} numeric) and \"{}\" ({} numeric)", payload.op.lexeme, lhs.name(), lhs.is_integer_type() || lhs.is_floating_point_type() ? "is" : "isn't", rhs.name(), rhs.is_integer_type() || rhs.is_floating_point_type() ? "is" : "isn't"));
				return rhs;
			break;
			case lex::type::operator_double_equals:
			[[fallthrough]];
			case lex::type::operator_notequals:
				d.assert_that(typecon_valid(rhs.is_implicitly_convertible_to(lhs)), std::format("both sides of a \"{}\" binary operation must have matching types - passed \"{}\" and \"{}\"", payload.op.lexeme, lhs.name(), rhs.name()));
				return type::from_primitive(primitive_type::boolean);
			break;
			case lex::type::operator_cast:
				return rhs;
			break;
			default:
				d.internal_error(std::format("unknown binary operator token \"{}\"", payload.op.lexeme));
			break;
		}
		return type::undefined();
	}

	type unary_operator(const data& d, const ast::unary_operator& payload)
	{
		if(payload.op.t == lex::type::operator_sizeof)
		{
			d.assert_that(std::holds_alternative<ast::identifier>(payload.expr->expr), std::format("unary operator \"{}\" must have operand equal to a typename.", payload.op.lexeme));
			return type::from_primitive(primitive_type::i64);
		}
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
			case lex::type::operator_defer:
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
			if(func->is_method && payload.iden == "this")
			{
				return type::from_struct(d.state.try_find_struct(func->method_owner_struct_name)->ty).pointer_to();
			}
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
		std::size_t argc = payload.params.size();
		d.assert_that(argc == maybe_function->params.size(), std::format("wrong number of arguments passed to call to \"{}\". expected {} arguments, but you provided {}", maybe_function->name, maybe_function->params.size(), argc));
		for(std::size_t i = 0; i < argc; i++)
		{
			type passed_ty = expression(d, *payload.params[i]);
			d.assert_that(typecon_valid(passed_ty.is_implicitly_convertible_to(maybe_function->params[i].ty)), std::format("type mismatch to argument {} (\"{}\") - expected \"{}\", but you provided \"{}\". do you need an explicit cast?", i, maybe_function->params[i].name, maybe_function->params[i].ty.name(), passed_ty.name()));
		}
		return maybe_function->return_ty;
	}

	type method_call(const data& d, const ast::method_call& payload)
	{
		type lhs_ty = expression(d, *payload.lhs);
		type initial_ty = lhs_ty;
		if(lhs_ty.is_pointer())
		{
			lhs_ty = lhs_ty.dereference();
		}
		d.assert_that(lhs_ty.is_struct(), std::format("detected use of member-access token within method call `.`. the left-hand-side of the token must be a struct or a pointer-to-struct type, which \"{}\" is not.", initial_ty.name()));
		struct_type struct_ty = lhs_ty.as_struct();
		const struct_t* structdata = d.state.try_find_struct(struct_ty.name);
		d.assert_that(structdata != nullptr, std::format("in method call, lhs expression is of type \"{}\", which is meant to be a struct, but cannot find this type.", struct_ty.name));

		auto method_iter = structdata->methods.find(payload.function_name);
		d.assert_that(method_iter != structdata->methods.end(), std::format("in method call, struct \"{}\" has no method named \"{}\"", struct_ty.name, payload.function_name));
		const function_t& func = method_iter->second;
		std::size_t argc = payload.params.size();
		d.assert_that(argc == func.params.size(), std::format("wrong number of arguments passed to call to method \"{}.{}\". expected {} arguments, but you provided {}", struct_ty.name, func.name, func.params.size(), argc));
		for(std::size_t i = 0; i < argc; i++)
		{
			type passed_ty = expression(d, *payload.params[i]);
			d.assert_that(typecon_valid(passed_ty.is_implicitly_convertible_to(func.params[i].ty)), std::format("type mismatch to argument {} (\"{}\") - expected \"{}\", but you provided \"{}\". do you need an explicit cast?", i, func.params[i].name, func.params[i].ty.name(), passed_ty.name()));
		}
		return func.return_ty;
		/*
		const function_t* maybe_function = d.state.try_find_function(payload.function_name.c_str());
		d.assert_that(maybe_function != nullptr, std::format("call to undeclared function \"{}\"", payload.function_name));
		std::size_t argc = payload.params.size();
		d.assert_that(argc == maybe_function->params.size(), std::format("wrong number of arguments passed to call to \"{}\". expected {} arguments, but you provided {}", maybe_function->name, maybe_function->params.size(), argc));
		for(std::size_t i = 0; i < argc; i++)
		{
			type passed_ty = expression(d, *payload.params[i]);
			d.assert_that(typecon_valid(passed_ty.is_implicitly_convertible_to(maybe_function->params[i].ty)), std::format("type mismatch to argument {} (\"{}\") - expected \"{}\", but you provided \"{}\". do you need an explicit cast?", i, maybe_function->params[i].name, maybe_function->params[i].ty.name(), passed_ty.name()));
		}
		return maybe_function->return_ty;
		*/
	}

	type member_access(const data& d, const ast::member_access& payload)
	{
		type lhs_ty = expression(d, *payload.lhs);
		type initial_ty = lhs_ty;
		if(lhs_ty.is_pointer())
		{
			lhs_ty = lhs_ty.dereference();
		}
		d.assert_that(lhs_ty.is_struct(), std::format("detected use of member-access token `.`. the left-hand-side of the token must be a struct or a pointer-to-struct type, which \"{}\" is not.", initial_ty.name()));
		struct_type struct_ty = lhs_ty.as_struct();
		for(std::size_t i = 0; i < struct_ty.data_members.size(); i++)
		{
			const auto& member = struct_ty.data_members[i];
			if(member.member_name == payload.rhs)
			{
				return *member.ty;
			}
		}
		d.fatal_error(std::format("struct \"{}\" has no data member named \"{}\"", struct_ty.name, payload.rhs));
		return type::undefined();
	}

	type array_access(const data& d, const ast::array_access& payload)
	{
		type lhs_ty = expression(d, *payload.expr);
		type rhs_ty = expression(d, *payload.index);
		d.assert_that(lhs_ty.is_pointer(), std::format("detected use of array-access token `.`. the left-hand-side of the token must be a pointer type (coz array types are NYI), which \"{}\" is not.", lhs_ty.name()));
		d.assert_that(typecon_valid(rhs_ty.is_implicitly_convertible_to(type::from_primitive(primitive_type::i64))), std::format("index of array access must be of type that is implicitly convertible to i64, which \"{}\" is not", rhs_ty.name()));
		return lhs_ty.dereference();
	}

	type return_statement(const data& d, const ast::return_statement& payload)
	{
		const function_t* maybe_parent = d.state.try_find_parent_function(d.tree, d.path);
		d.assert_that(maybe_parent != nullptr, "detected return statement outside of a function. return statements are only valid within function implementation blocks.");
		if(payload.expr.has_value())
		{
			type ret = expression(d, *payload.expr.value());
			d.assert_that(!maybe_parent->return_ty.is_void(), std::format("return statement was an expression of type \"{}\", but the enclosing function \"{}\" returns {}. do not return an expression.", ret.name(), maybe_parent->name, maybe_parent->return_ty.name()));
			d.assert_that(typecon_valid(ret.is_implicitly_convertible_to(maybe_parent->return_ty)), std::format("type of `return` statement \"{}\" does not match the return-type of the enclosing function \"{}\", which is a \"{}\"", ret.name(), maybe_parent->name, maybe_parent->return_ty.name()));
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
		d.assert_that(payload.type_name == ty.name(), std::format("parse-type-from-string error. input string: \"{}\". output type.name(): \"{}\"", payload.type_name, ty.name()));
		d.state.register_local_variable
		({
			.ty = ty,
			.name = payload.var_name,
			.ctx = {.tree = &d.tree, .path = d.path}
		});

		if(payload.initialiser.has_value())
		{
			type expr_ty = expression(d, *payload.initialiser.value());
			d.assert_that(typecon_valid(expr_ty.is_implicitly_convertible_to(ty)), std::format("initialiser of variable \"{}\" is of type \"{}\", which is not implicitly convertible to the variable's type of \"{}\". perhaps you forgot to insert an explicit cast?", payload.var_name, expr_ty.name(), ty.name()));
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
			[&](ast::char_literal lit)
			{
				ret = type::from_primitive(primitive_type::i8);
			},
			[&](ast::string_literal lit)
			{
				ret = type::from_primitive(primitive_type::i8).pointer_to(qualifier_const);
			},
			[&](ast::bool_literal lit)
			{
				ret = type::from_primitive(primitive_type::boolean);
			},
			[&](ast::null_literal lit)
			{
				ret = type::from_primitive(primitive_type::i8).pointer_to(qualifier_weak);
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
			},
			[&](ast::unary_operator op)
			{
				ret = unary_operator(d, op);
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
			[&](ast::method_call call)
			{
				ret = method_call(d, call);
			},
			[&](ast::member_access mem)
			{
				ret = member_access(d, mem);
			},
			[&](ast::array_access mem)
			{
				ret = array_access(d, mem);
			},
			[&](ast::expression expr)
			{
				ret = expression(d, expr);
			},
			[&](ast::if_statement ifst)
			{
				ret = if_statement(d, ifst);
			},
			[&](ast::for_statement forst)
			{
				ret = for_statement(d, forst);
			},
			[&](ast::struct_initialiser strinit)
			{
				ret = struct_initialiser(d, strinit);
			},
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