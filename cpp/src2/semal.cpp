#include "semal.hpp"
#include "diag.hpp"
#include "profile.hpp"

namespace semal
{
	std::unordered_map<std::size_t, void(*)(unit&, syntax::node::path_view_t, const syntax::node&)> semal_table;

	void analyse_node(unit& u, syntax::node::path_view_t path, const syntax::node& n)
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

	unit analyse_file(const syntax::node& ast, type_system& tsys)
	{
		PROFZONE("semal file");
		diag::assert_that(std::holds_alternative<syntax::root>(ast.payload), error_code::ice, "root-node of file AST is not a root node, it is instead a {}", ast.name());
		unit ret
		{
			.type = unit_type::source_file,
			.name = std::get<syntax::root>(ast.payload).source_file.string(),
			.ast = ast
		};
		PROFNAMEF("file: %s", ret.name.c_str());
		ast.iterate([&ret](syntax::node::path_view_t path, const syntax::node& node)
		{
			analyse_node(ret, path, node);
		});
		return ret;
	}

	void populate_semal_table()
	{
		PROFZONE("semal table populate");
		#define SEMAL_CHORD(ty) semal_table[syntax::node{.payload = syntax::ty{}}.hash()] = [](unit& u, syntax::node::path_view_t path, const syntax::node& node)
		SEMAL_CHORD(struct_decl)
		{

		};
	}
}