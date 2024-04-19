#include "semantic.hpp"
#include "diag.hpp"
#include "lex.hpp"
#include "type.hpp"
#include <format>
#include <unordered_map>
#include <variant>

namespace semantic
{

	// semantic analysis is made up of 2 stages:
	// 1.) pre-pass. gather all global variables, struct and function declarations as they are all top-level and global.
	// 2.) process. step through the AST properly and make sure everything makes sense
	state analysis(const ast& tree)
	{
		state s;
		s.pre_pass(tree);
		if(s.last_error.size())
		{
			diag::fatal_error(s.last_error);
		}
		s.process(tree);
		if(s.last_error.size())
		{
			diag::fatal_error(s.last_error);
		}
		return s;
	}

	void state::pre_pass(const ast& tree)
	{
		// firstly, gather all struct types.
		for(std::size_t i = 0; i < tree.program.children.size(); i++)
		{
			const ast::node& node = tree.program.children[i];
			const ast::path_t path{i};
			// does it define a struct?
			if(std::holds_alternative<ast::struct_definition>(node.payload))
			{
				// cool, go through node children to get the data members.
				const auto& data = std::get<ast::struct_definition>(node.payload);
				struct_t ty;
				ty.ty.name = data.struct_name;
				ty.context = path;
				// go through its children.
				for(std::size_t i = 0; i < node.children.size(); i++)
				{
					const ast::node& child = node.children[i];
					if(std::holds_alternative<ast::variable_declaration>(child.payload))
					{
						const auto& decl = std::get<ast::variable_declaration>(child.payload);	
						if(decl.array_size != 0)
						{
							this->last_error = std::format("semal error on line {} - detected data member of array type. this is not yet implemented.", child.meta.line_number);
							return;
						}
						type param_type = this->get_type_from_name(decl.type_name).first;
						if(param_type.is_undefined())
						{
							this->last_error = std::format("semal error on line {} - could not decipher type of data member {} (typename: {}). if its a struct, it must be defined before this struct.", child.meta.line_number, decl.var_name, decl.type_name);
							return;
						}
						ty.ty.data_members.push_back(param_type);
					}
					else
					{
						this->last_error = std::format("semal error on line {} - every expression within a struct definition should be a variable declaration.", child.meta.line_number);
						return;
					}
				}
				if(this->struct_decls.contains(ty.ty.name))
				{
					std::size_t previously_defined_on_line = tree.get(this->struct_decls.at(ty.ty.name).context).meta.line_number;
					this->last_error = std::format("semal error on line {} - double definition of struct {} (previous definition on line {})", node.meta.line_number, ty.ty.name, previously_defined_on_line);
					return;
				}
				this->register_struct(ty);
			}
		}

		// then, functions and globals can be done at the same time.
		for(std::size_t i = 0; i < tree.program.children.size(); i++)
		{
			const ast::node& node = tree.program.children[i];
			const ast::path_t path{i};
			if(std::holds_alternative<ast::function_definition>(node.payload))
			{
				function_t fn;
				auto func = std::get<ast::function_definition>(node.payload);	
				if(func.is_extern && node.children.size())
				{
					this->last_error = std::format("semal error on line {} - detected extern function {} but with child AST nodes, implying it has a body. extern functions cannot have implementations.", node.meta.line_number, func.function_name);
					return;
				}
				fn.name = func.function_name;
				fn.context = path;
				for(const auto& param : func.params)
				{
					type param_type = this->get_type_from_name(param.type_name).first;
					if(param_type.is_undefined())
					{
						this->last_error = std::format("semal error on line {} - could not decipher type of function parameter {} (typename: {}). if its a struct, it must be defined before this struct.", node.meta.line_number, param.var_name, param.type_name);
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
			}

			if(std::holds_alternative<ast::variable_declaration>(node.payload))
			{
				// global
				auto gvar = std::get<ast::variable_declaration>(node.payload);
				if(gvar.array_size != 0)
				{
					this->last_error = std::format("semal error on line {} - detected global variable of array type. this is not yet implemented.", node.meta.line_number);
					return;
				}
				local_variable_t gv;
				gv.context = path;
				gv.name = gvar.var_name;
				type var_type = this->get_type_from_name(gvar.type_name).first;
				if(var_type.is_undefined())
				{
					this->last_error = std::format("semal error on line {} - could not decipher type of global variable {} (typename: {}). if its a struct, it must be defined before this struct.", node.meta.line_number, gvar.var_name, gvar.type_name);
					return;
				}
				if(this->global_variables.contains(gv.name))
				{
					std::size_t previously_defined_on_line = tree.get(this->global_variables.at(gv.name).context).meta.line_number;
					this->last_error = std::format("semal error on line {} - double definition of global variable {} (previous definition on line {})", node.meta.line_number, gv.name, previously_defined_on_line);
					return;
				}
				this->register_global_variable(gv);
			}
		}
	}

	void state::process(const ast& tree)
	{
		for(std::size_t i = 0; i < tree.program.children.size(); i++)
		{
			ast::path_t path = {i};
			this->process_node(path, tree);
		}
	}

	void state::process_node(ast::path_t path, const ast& tree)
	{
		const ast::node& node = tree.get(path);
		this->process_single_node(path, tree);
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			ast::path_t child_path = path;
			child_path.push_back(i);
			this->process_node(child_path, tree);
		}
	}

	void process_node_impl(state& s, ast::path_t path, const ast& tree);

	void state::process_single_node(ast::path_t path, const ast& tree)
	{
		process_node_impl(*this, path, tree);
	}

	std::pair<type, ast::path_t> state::get_type_from_name(std::string_view type_name) const
	{
		// is it a struct type?
		for(const auto& [struct_name, struct_data] : this->struct_decls)
		{
			if(struct_name == type_name)
			{
				return {type::from_struct(struct_data.ty), struct_data.context};
			}
		}
		for(int i = 0; i < static_cast<int>(primitive_type::_count); i++)
		{
			if(type_name == primitive_type_names[i])
			{
				return {type::from_primitive(static_cast<primitive_type>(i)), ast::path_t{}};
			}
		}

		return {type::undefined(), ast::path_t{}};
	}

	const function_t* state::try_find_function(const std::string& function_name) const
	{
		auto iter = this->functions.find(function_name);
		if(iter == this->functions.end())
		{
			return nullptr;
		}
		return &iter->second;
	}

	const struct_t* state::try_find_struct(const std::string& struct_name) const
	{
		auto iter = this->struct_decls.find(struct_name);
		if(iter == this->struct_decls.end())
		{
			return nullptr;
		}
		return &iter->second;
	}

	const local_variable_t* state::try_find_global_variable(const std::string& variable_name) const
	{
		auto iter = this->global_variables.find(variable_name);
		if(iter == this->global_variables.end())
		{
			return nullptr;
		}
		return &iter->second;
	}

	const local_variable_t* state::try_find_local_variable(const ast::path_t& context, const std::string& variable_name) const
	{
		ast::path_view_t cv = context;
		const scope_reference* current_scope = &this->variables;
		if(context.size())
		{
			for(std::size_t idx : context)
			{
				auto iter = current_scope->variables.find(variable_name);
				if(iter != current_scope->variables.end())
				{
					return &iter->second;
				}
				current_scope = &const_cast<std::unordered_map<std::size_t, scope_reference>&>(current_scope->children)[idx];
			}
		}
		return nullptr;
	}

	void state::register_struct(struct_t str)
	{
		this->struct_decls[str.ty.name] = str;
	}

	void state::register_function(function_t func)
	{
		this->functions[func.name] = func;
	}

	void state::register_global_variable(local_variable_t var)
	{
		this->global_variables[var.name] = var;
	}

	void state::register_local_variable(local_variable_t var)
	{
		ast::path_view_t ctx = var.context;
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

	/////////////////////////////////////// NODE PROCESSING ///////////////////////////////////////

	struct data
	{
		state& state;
		ast::path_t path;
		const ast& tree;

		std::size_t line() const
		{
			return this->tree.get(path).meta.line_number;
		}

		void fatal_error(std::string msg) const
		{
			diag::fatal_error(std::format("line {} - {}", this->line(), msg));
		}

		void assert_that(bool expr, std::string msg) const
		{
			if(!expr)
			{
				this->fatal_error(msg);	
			}
		}
	};

	using unary_expression_t = std::pair<ast::unary_operator, util::box<ast::expression>>;

	void unary_expression(const data& d, unary_expression_t payload)
	{
		const auto& [op, expr] = payload;
	}

	using binary_expression_t = std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>;

	void binary_expression(const data& d, binary_expression_t payload)
	{
		const auto& [op, expr1, expr2] = payload;
	}

	void identifier(const data& d, ast::identifier payload)
	{

	}

	void function_call(const data& d, ast::function_call payload)
	{

	}

	void member_access(const data& d, ast::member_access payload)
	{

	}

	void if_statement(const data& d, ast::if_statement payload)
	{

	}

	void for_statement(const data& d, ast::for_statement payload)
	{

	}

	void return_statement(const data& d, ast::return_statement payload)
	{

	}


	void variable_declaration(const data& d, ast::variable_declaration payload)
	{
		if(d.path.size() <= 1)
		{
			// we already sorted out global variables.
			return;
		}
		auto global_already = d.state.try_find_global_variable(payload.var_name);
		d.assert_that(global_already == nullptr, std::format("variable named \"{}\" would shadow a global variable (defined on line {})", payload.var_name, global_already != nullptr ? d.tree.get(global_already->context).meta.line_number : 0));
		auto local_already = d.state.try_find_local_variable(d.path, payload.var_name);
		d.assert_that(local_already == nullptr, std::format("variable named \"{}\" would shadow a previously-defined local variable (defined on line {})", payload.var_name, local_already != nullptr ? d.tree.get(local_already->context).meta.line_number : 0));

		const auto& [ty, defnode] = d.state.get_type_from_name(payload.type_name);
		d.assert_that(!ty.is_undefined(), std::format("undefined type \"{}\"", payload.type_name));
		d.state.register_local_variable
		({
			.ty = ty,
			.name = payload.var_name,
			.context = d.path
		});
	}

	void function_definition(const data& d, ast::function_definition payload)
	{
		d.assert_that(d.path.size() <= 1, std::format("detected a function definition \"{}\" within another block. functions can only be defined at the top-level scope.", payload.function_name));
	}

	void struct_definition(const data& d, ast::struct_definition payload)
	{
		d.assert_that(d.path.size() <= 1, std::format("detected a struct definition \"{}\" within another block. structs can only be defined at the top-level scope.", payload.struct_name));
	}

	void meta_region(const data& d, ast::meta_region payload)
	{

	}

	template<typename ... Ts> 
	struct overload : Ts ... { 
		using Ts::operator() ...;
	};
	template<class... Ts> overload(Ts...) -> overload<Ts...>;

	template<typename T>
	void generic(const data& d, T payload)
	{
		auto dispatch = overload
		{
			[&](ast::binary_operator op)
			{
				d.fatal_error("dispatch error");
			},
			[&](ast::unary_operator op)
			{
				d.fatal_error("dispatch error");
			},
			[&](unary_expression_t uexpr)
			{
				unary_expression(d, uexpr);
			},
			[&](binary_expression_t bexpr)
			{
				binary_expression(d, bexpr);
			},
			[&](ast::identifier id)
			{
				identifier(d, id);
			},
			[&](ast::function_call call)
			{
				function_call(d, call);
			},
			[&](ast::member_access mem)
			{
				member_access(d, mem);
			},
			[&](ast::expression expr)
			{
				// recurse on underlying expr.
				generic(d, expr.expr);
			},
			[&](ast::if_statement ifst)
			{
				if_statement(d, ifst);
			},
			[&](ast::for_statement forst)
			{
				for_statement(d, forst);
			},
			[&](ast::return_statement returnst)
			{
				return_statement(d, returnst);
			},
			[&](ast::variable_declaration decl)
			{
				variable_declaration(d, decl);
			},
			[&](ast::function_definition fdef)
			{
				function_definition(d, fdef);
			},
			[&](ast::struct_definition sdef)
			{
				struct_definition(d, sdef);
			},
			[&](ast::meta_region meta)
			{
				meta_region(d, meta);
			},
			[&](auto)
			{

			}
		};

		std::visit(dispatch, payload);
	}

	void process_node_impl(state& s, ast::path_t path, const ast& tree)
	{
		const ast::node& node = tree.get(path);
		data d{.state = s, .path = path, .tree = tree};
		generic(d, node.payload);
	}
}