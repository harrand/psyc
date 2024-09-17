#ifndef PSYC_SEMAL_HPP
#define PSYC_SEMAL_HPP
#include "ast.hpp"
#include "type.hpp"

namespace semal
{
	enum class visibility
	{
		local,
		global,
		_count,
	};

	struct struct_decl
	{
		syntax::struct_decl node;
		visibility vis = visibility::local;
	};

	struct function_decl
	{
		syntax::function_decl node;
		visibility vis = visibility::local;
	};

	struct variable_scope
	{
		std::unordered_map<std::string, syntax::variable_decl> decls = {};
		std::unordered_map<std::size_t, variable_scope> children = {};
		void merge(const variable_scope& v);
	};

	enum class unit_type
	{
		source_file,
		api_module,
	};

	struct unit
	{
		unit_type type;
		std::string name;
		std::vector<struct_decl> structs = {};
		std::vector<function_decl> functions = {};
		variable_scope variables = {};

		void merge(const unit& u);
		const syntax::variable_decl* try_find_variable(syntax::node::path_view_t path, const std::string& varname) const;
		void push_variable_decl(syntax::node::path_view_t path, syntax::variable_decl decl);
	};

	struct program
	{
		std::unordered_map<std::filesystem::path, unit> local_file_units = {};
		std::unordered_map<std::string, unit> module_units = {};
	};

	program analyse_file(const syntax::node& ast, type_system& tsys);
	void populate_semal_table();
}

#endif // PSYC_SEMAL_HPP