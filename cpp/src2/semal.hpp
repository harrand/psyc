#ifndef PSYC_SEMAL_HPP
#define PSYC_SEMAL_HPP
#include "type.hpp"
#include "ast.hpp"

namespace semal
{
	struct identifier
	{
		identifier(const type_system& tsys, const syntax::node::identifier& node);

		type_ptr ty;
		const syntax::node::identifier* node;

		const itype& get_type() const;
	};

	struct expression
	{
		expression(const type_system& tsys, const syntax::node::expression& node);
		type_ptr ty;
		const syntax::node::expression* node;

		const itype& get_type() const;
	};

	struct variable_decl
	{
		variable_decl(const type_system& tsys, const syntax::node::variable_decl& node);
		type_ptr ty;
		const syntax::node::variable_decl* node;

		std::string_view get_name() const;
		const itype& get_type() const;
	};

	struct function_decl
	{
		function_decl(const type_system& tsys, const syntax::node::function_decl& node);
		type_ptr return_ty;
		const syntax::node::function_decl* node;
		std::vector<type_ptr> param_types;

		std::string_view get_name() const;
		const itype& get_return_type() const;
		std::span<const type_ptr> get_param_types() const;
	};

	struct struct_decl
	{
		struct_decl(type_system& tsys, const syntax::node::struct_decl& node);
		type_ptr ty;
		const syntax::node::struct_decl* node;

		const itype& get_type() const;
	};

	struct namespace_access
	{
		namespace_access(const type_system& tsys, const syntax::node::identifier& lhs, const syntax::node::expression& rhs);
		std::string namespace_name;
		type_ptr evaluated_ty;
	};

	struct block
	{
		block(type_system& tsys, const syntax::node::block& node);
		const syntax::node::block* node;
	};

	void analyse(const syntax::inode* ast, type_system& tsys);
}

#endif // PSYC_SEMAL_HPP