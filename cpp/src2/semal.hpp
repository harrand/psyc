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

		void merge(const unit& u);
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