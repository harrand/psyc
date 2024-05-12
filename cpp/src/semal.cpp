#include "semal.hpp"
#include "type.hpp"

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
			if(c == '*')
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

	void output::register_struct(struct_t structdata)
	{
		if(this->struct_decls.contains(structdata.ty.name))
		{
			structdata.ctx.semal_error("redeclaration of struct \"{}\". previously defined in same file at: {}", structdata.ty.name, this->struct_decls.at(structdata.ty.name).ctx.location().to_string());
		}
	}

	output analyse_predecl(const ast& tree)
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
			// does it define a struct?
			/*
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
						ty.ty.data_members.push_back
						({
							.member_name = decl.var_name,
							.type = param_type
						});
					}
					else if(std::holds_alternative<ast::function_definition>(child.payload))
					{
						// this is a method.
						auto decl = std::get<ast::function_definition>(child.payload);
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
			*/
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

	output analyse_full(const ast& tree, output predecl)
	{
		output ret = predecl;
		// todo: analyse all code (mainly implementations).
		
		return ret;
	}
}