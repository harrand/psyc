#ifndef PSYC_SEMAL_HPP
#define PSYC_SEMAL_HPP
#include "type.hpp"
#include "ast.hpp"

namespace semal
{
	struct identifier
	{
		identifier(const type_system& tsys, syntax::node::identifier& node);

		type_ptr ty;
		syntax::node::identifier* node;

		const itype& get_type() const;
	};

	struct expression
	{
		expression(const type_system& tsys, syntax::node::expression& node);
		type_ptr ty;
		syntax::node::expression* node;

		const itype& get_type() const;
	};

	struct variable
	{
		variable(const type_system& tsys, syntax::node::variable_decl& node);
		type_ptr ty;
		syntax::node::variable_decl* node;

		std::string_view get_name() const;
		const itype& get_type() const;
	};

	struct function
	{
		function(const type_system& tsys, syntax::node::function_decl& node);
		type_ptr return_ty;
		syntax::node::function_decl* node;
		std::vector<type_ptr> param_types;

		std::string_view get_name() const;
		const itype& get_return_type() const;
		std::span<const type_ptr> get_param_types() const;
	};

	struct struct_decl
	{
		struct_decl(type_system& tsys, syntax::node::structdata& node);
		type_ptr ty;
		syntax::node::structdata* node;

		const itype& get_type() const;
	};
}

#endif // PSYC_SEMAL_HPP