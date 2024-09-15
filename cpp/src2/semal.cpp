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

	std::unordered_map<std::size_t, void(*)(program&, syntax::node::path_view_t, const syntax::node&)> semal_table;

	void analyse_node(program& u, syntax::node::path_view_t path, const syntax::node& n)
	{
		PROFZONE("semal node");
		const char* name = n.name();
		PROFNAMEF("node: %s", name);
		PROFZONE_BEGIN(visit);
		auto hash = n.hash();
		auto iter = semal_table.find(hash);
		if(iter != semal_table.end())
		{
			iter->second(u, path, n);
		}
		PROFZONE_END(visit);
	}

	program analyse_file(const syntax::node& ast, type_system& tsys)
	{
		PROFZONE("semal file");
		diag::assert_that(std::holds_alternative<syntax::root>(ast.payload), error_code::ice, "root-node of file AST is not a root node, it is instead a {}", ast.name());
		program ret;
		PROFNAMEF("file: %s", std::get<syntax::root>(ast.payload).source_file.string().c_str());
		ast.iterate([&ret](syntax::node::path_view_t path, const syntax::node& node)
		{
			analyse_node(ret, path, node);
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

	void populate_semal_table()
	{
		PROFZONE("semal table populate");
		#define SEMAL_CHORD(ty) semal_table[syntax::node{.payload = syntax::ty{}}.hash()] = [](program& prog, syntax::node::path_view_t path, const syntax::node& node_){auto node = NODE_AS(node_, ty);
		#define SEMAL_END };
		SEMAL_CHORD(capped_struct_decl)
			std::string module_name = get_module_name_of(node);
			if(module_name.size())
			{
				ensure_module(prog, module_name).structs.push_back
				({
					.node = node,
					.vis = visibility::global
				});
			}
		SEMAL_END
		SEMAL_CHORD(capped_function_decl)
			std::string module_name = get_module_name_of(node);
			if(module_name.size())
			{
				ensure_module(prog, module_name).functions.push_back
				({
					.node = node,
					.vis = visibility::global
				});
			}
		SEMAL_END
	}
}