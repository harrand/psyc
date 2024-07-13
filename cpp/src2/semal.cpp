#include "semal.hpp"
#include "diag.hpp"

namespace semal
{
	#define sem_assert_ice(expr, fmt, ...) diag::assert_that(expr, error_code::ice, "at {}: {}", node.loc.to_string(), std::format(fmt, ##__VA_ARGS__))
	#define sem_assert(expr, fmt, ...) diag::assert_that(expr, error_code::semal, "at {}: {}", node.loc.to_string(), std::format(fmt, ##__VA_ARGS__))

	std::unordered_map<std::size_t, type_ptr(*)(const syntax::inode* node)> type_checking_table;
	#define TYPECHECK_BEGIN(x) type_checking_table[syntax::node::x{}.hash()] = [](const syntax::inode* base_node)->type_ptr{ \
	const auto& node = *static_cast<const syntax::node::x*>(base_node);
	#define TYPECHECK_END };
	#define ILL_FORMED return incomplete_type("badtype").unique_clone()
	type_system tsys;

	void analyse(const syntax::inode* ast, type_system& tsys)
	{
		auto iter = type_checking_table.find(ast->hash());
		diag::assert_that(iter != type_checking_table.end(), error_code::nyi, "type checking NYI for \"{}\" nodes (hash: {})", ast->name(), ast->hash());
		type_ptr ty = iter->second(ast);
		if(ty != nullptr)
		{
			diag::assert_that(ty->is_well_formed(), error_code::type, "type checking of AST node {} (at {}) yielded a {} type: {}", ast->name(), ast->loc.to_string(), ty->hint_name(), ty->get_name());
		}
		// todo: verify the type?
		for(const auto& child : ast->children)
		{
			if(child != nullptr)
			{
				analyse(child.get(), tsys);
			}	
		}
	}

	void populate_table()
	{
		TYPECHECK_BEGIN(identifier)
			type_ptr ret = tsys.get_type(node.iden);
			if(ret == nullptr)
			{
				sem_assert(false, "semantic analysis for identifiers that aren't directly typenames (iden: {}) is NYI", node.iden);
				ILL_FORMED;
			}
			return ret;
		TYPECHECK_END

		TYPECHECK_BEGIN(root)
			return nullptr;
		TYPECHECK_END
	}
}