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

	enum class unit_type
	{
		source_file,
		api_module,
	};
	struct unit
	{
		unit_type type;
		std::string name;
		const syntax::node& ast;
	};
	unit analyse_file(const syntax::node& ast, type_system& tsys);
	void populate_semal_table();
}

#endif // PSYC_SEMAL_HPP