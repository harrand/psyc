#include "semal.hpp"
#include "diag.hpp"
#include "profile.hpp"

namespace semal
{
	void unit::merge(const unit& u)
	{
		for(const auto& struct_ : u.structs)
		{
			this->structs.push_back(struct_);
		}
		for(const auto& func_ : u.functions)
		{
			this->functions.push_back(func_);
		}
	}

	std::unordered_map<std::size_t, void(*)(program&, syntax::node::path_view_t, const syntax::node&, const std::string&)> semal_table;

	void analyse_node(program& u, syntax::node::path_view_t path, const syntax::node& n, const std::string& filename)
	{
		PROFZONE("semal node");
		const char* name = n.name();
		PROFNAMEF("node: %s", name);
		PROFZONE_BEGIN(visit);
		auto hash = n.hash();
		auto iter = semal_table.find(hash);
		if(iter != semal_table.end())
		{
			iter->second(u, path, n, filename);
		}
		PROFZONE_END(visit);
	}

	program analyse_file(const syntax::node& ast, type_system& tsys)
	{
		PROFZONE("semal file");
		diag::assert_that(std::holds_alternative<syntax::root>(ast.payload), error_code::ice, "root-node of file AST is not a root node, it is instead a {}", ast.name());
		std::filesystem::path filename = std::get<syntax::root>(ast.payload).source_file;
		program ret;
		PROFNAMEF("file: %s", filename.string().c_str());
		ast.iterate([&ret, &filename](syntax::node::path_view_t path, const syntax::node& node)
		{
			analyse_node(ret, path, node, filename.string());
		});
		return ret;
	}

	syntax::node get_annotation_value(std::string anno_name, const syntax::annotations& anno)
	{
		for(const auto& expr : anno.exprs)
		{
			if(expr.t != syntax::expression::type::assign)
			{
				continue;
			}
			auto lhs = NODE_AS(expr.expr, expression);
			if(lhs.t == syntax::expression::type::identifier)
			{
				auto name = NODE_AS(lhs.expr, identifier);
				if(name.iden == anno_name)
				{
					return *expr.extra;
				}
			}
		}
		return syntax::node{};
	}

	template<typename N>
	std::string get_module_name_of(const N& n)
	{
		syntax::node module_name_node = get_annotation_value("module", n.annotations);
		if(module_name_node.has_value())
		{
			auto rhs = NODE_AS(module_name_node, expression);
			diag::assert_that(rhs.t == syntax::expression::type::identifier, error_code::semal, "'module' annotation must be an identifier expression, but it is instead a {} expression", syntax::expression::type_names[static_cast<int>(rhs.t)]);
			return NODE_AS(rhs.expr, identifier).iden;
		}
		else
		{
			return "";
		}
	}

	unit& ensure_module(program& prog, const std::string& module_name)
	{
		prog.module_units[module_name].name = module_name;
		return prog.module_units[module_name];
	}

	unit& ensure_local_file(program& prog, const std::string& file)
	{
		prog.local_file_units[file].name = file;
		return prog.local_file_units[file];
	}

	std::string current_module_scope = "";
	std::optional<syntax::variable_decl> auto_function_prefix = {};

	template<typename N>
	unit& get_unit(program& prog, const N& node, const std::string& file)
	{
		// which unit should this node be a part of?
		std::string module_name = get_module_name_of(node);
		if(module_name.empty())
		{
			// no annotation specifying module
			if(current_module_scope.empty())
			{
				// no existing module in scope (e.g within a struct block). default to local file unit
				return ensure_local_file(prog, file);
			}
			// in a module scope (e.g struct block), use that module's name.
			return ensure_module(prog, current_module_scope);
		}
		// module has been specified. update the current module scope and return this module.
		current_module_scope = module_name;
		return ensure_module(prog, module_name);
	}

	void populate_semal_table()
	{
		PROFZONE("semal table populate");
		#define SEMAL_CHORD(ty) semal_table[syntax::node{.payload = syntax::ty{}}.hash()] = [](program& prog, syntax::node::path_view_t path, const syntax::node& node_, const std::string& file){auto node = NODE_AS(node_, ty);
		#define SEMAL_END };
		SEMAL_CHORD(capped_struct_decl)
			get_unit(prog, node, file).structs.push_back
			({
				.node = node,
				.vis = visibility::global
			});
			auto_function_prefix = syntax::variable_decl
			{
				syntax::identifier{"this"},
				syntax::identifier{node.struct_name.iden + "&"}
			};
			// do children under current module scope.
			const auto& direct_child = node.children.front();
			diag::assert_that(NODE_IS(*direct_child, block), error_code::semal, "ruh roh raggyu");

			for(std::size_t i = 0; i < direct_child->children().size(); i++)
			{
				const auto& child = direct_child->children()[i];
				syntax::node::path_t child_path = {path.begin(), path.end()};
				child_path.push_back(0);
				child_path.push_back(i);
				analyse_node(prog, child_path, *child, file);
			}
			auto_function_prefix = {};
			current_module_scope.clear();
		SEMAL_END
		SEMAL_CHORD(capped_function_decl)
			// if function prefix was set, add that to the front of the parameters (this is to handle methods)
			if(auto_function_prefix.has_value())
			{
				node.params.decls.insert(node.params.decls.begin(), auto_function_prefix.value());
			}
			get_unit(prog, node, file).functions.push_back
			({
				.node = node,
				.vis = visibility::global
			});

			current_module_scope.clear();
		SEMAL_END
	}
}