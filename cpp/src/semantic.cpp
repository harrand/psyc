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
	// 1.) gathering. gather all global variables, struct and function declarations as they are all top-level and global.
	// 2.) validation. step through the AST properly and make sure everything makes sense
	state analysis(const ast& tree)
	{
		state s;
		s.pre_pass(tree);
		if(s.last_error.size())
		{
			diag::error(s.last_error);
		}
		s.process(tree);
		if(s.last_error.size())
		{
			diag::error(s.last_error);
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
		// processing goes here.
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			ast::path_t child_path = path;
			child_path.push_back(i);
			this->process_node(child_path, tree);
		}
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
		this->variables[var.context][var.name] = var;
	}
}