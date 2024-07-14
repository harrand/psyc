#include "semal.hpp"
#include "diag.hpp"
#include <vector>
#include <queue>

namespace semal
{
	#define sem_assert_ice(expr, fmt, ...) diag::assert_that(expr, error_code::ice, "at {}: {}", node.loc.to_string(), std::format(fmt, ##__VA_ARGS__))
	#define sem_assert(expr, fmt, ...) diag::assert_that(expr, error_code::semal, "at {}: {}", node.loc.to_string(), std::format(fmt, ##__VA_ARGS__))

	struct var_scope
	{
		std::unordered_map<std::string, const syntax::node::variable_decl*> decls = {};
		const syntax::node::block* block = nullptr;
	};
	// stack starts with a single empty scope (this is global scope. should expect global variables to be defined here.)
	std::vector<var_scope> var_stack = {var_scope{}};
	// sometimes variable decls appear before the block scope they're in (e.g a function-decl will semal its parameters before its block (first child)).
	// when this happens, we put them here instead. next time a scope is pushed, move everything in the queue into the new scope.
	std::queue<const syntax::node::variable_decl*> premature_scope_variables = {};
	const syntax::node::variable_decl* find_variable(const std::string& name)
	{
		for(var_scope& scope : var_stack)
		{
			auto iter = scope.decls.find(name);
			if(iter != scope.decls.end())
			{
				return iter->second;
			}
		}
		return nullptr;
	}
	var_scope& push_scope()
	{
		var_scope& ret = var_stack.emplace_back();
		while(premature_scope_variables.size())
		{
			const auto* var = premature_scope_variables.front();
			ret.decls[var->var_name.iden] = var;
			premature_scope_variables.pop();
		}
		return ret;
	}
	void pop_scope(){diag::assert_that(var_stack.size(), error_code::ice, "attempt to pop_scope when var stack was already empty."); var_stack.pop_back();}
	void add_var(const syntax::node::variable_decl& decl)
	{
		if(decl.impl_should_add_to_current_scope)
		{
			if(decl.impl_is_defined_before_parent_block)
			{
				premature_scope_variables.push(&decl);
			}
			else
			{
				var_stack.back().decls[decl.var_name.iden] = &decl;
			}
		}
	}

	std::unordered_map<std::size_t, type_ptr(*)(const syntax::inode* node)> type_checking_table;
	#define TYPECHECK_BEGIN(x) type_checking_table[syntax::node::x{}.hash()] = [](const syntax::inode* base_node)->type_ptr{ \
	const auto& node = *static_cast<const syntax::node::x*>(base_node);
	#define TYPECHECK_END };
	#define ILL_FORMED return incomplete_type("badtype").unique_clone()
	#define GETTYPE(n) [&](){diag::assert_that(type_checking_table.contains(n.hash()), error_code::nyi, "type checking NYI for \"{}\" nodes (hash: {})", n.name(), n.hash()); return type_checking_table.at(n.hash())(&n);}()
	type_system tsys;

	void analyse(const syntax::inode* ast, type_system& tsys)
	{
		bool should_create_scope = ast->hash() == syntax::node::block{}.hash();
		if(should_create_scope)
		{
			push_scope();
		}
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
		if(should_create_scope)
		{
			pop_scope();
		}
	}

	void populate_table()
	{
		TYPECHECK_BEGIN(identifier)
			type_ptr ret = tsys.get_type(node.iden);
			if(ret == nullptr)
			{
				// find a variable declared by this name.
				const syntax::node::variable_decl* decl = find_variable(node.iden);
				if(decl != nullptr)
				{
					return GETTYPE((*decl));
				}
				sem_assert(false, "could not decipher type of identifier \"{}\". wasn't a valid typename, nor was it a name of a variable available in this scope.", node.iden);
				ILL_FORMED;
			}
			return ret;
		TYPECHECK_END

		TYPECHECK_BEGIN(root)
			return nullptr;
		TYPECHECK_END

		TYPECHECK_BEGIN(block)
			for(const auto& child : node.children)
			{
				GETTYPE((*child));
			}
			return nullptr;
		TYPECHECK_END

		TYPECHECK_BEGIN(variable_decl)
			add_var(node);
			if(node.type_name.iden != syntax::node::inferred_typename)
			{
				return tsys.get_type(node.type_name.iden);
			}
			sem_assert(!node.expr.is_null(), "variable declaration {} does not explicitly specify its type but also doesn't have an initialiser. if you want to infer the type, you must give it a valid initialiser at the point of declaration.", node.var_name.iden);
			return GETTYPE(node.expr);
		TYPECHECK_END

		TYPECHECK_BEGIN(struct_decl)
			sem_assert_ice(node.children.size() == 1 && node.children.front()->hash() == syntax::node::block{}.hash(), "Struct Declaration AST node must have a single child, and that child must be a block. Instead it has {} child{}{}", node.children.size(), node.children.size() == 1 ? "" : "ren", node.children.size() > 1 ? std::format("(first child is a \"{}\")", node.children.front()->name()) : "");
			type_system::struct_builder builder = tsys.make_struct(node.struct_name.iden);
			// get the children of the block. this is where methods and data member variable declarations will be.
			// however, possible TODO: you will almost certainly want to extract the method declarations here too, so it's easy to lookup by method name.
			for(const syntax::node_ptr& ptr : node.children.front()->children)
			{
				// the data members of a struct are encoded in the type. methods however are *not*
				if(ptr->hash() == syntax::node::variable_decl{}.hash())
				{
					const auto& data_member = static_cast<const syntax::node::variable_decl&>(*ptr);
					data_member.impl_should_add_to_current_scope = false;
					type_ptr data_member_type = GETTYPE(data_member);
					builder.add_member(data_member.var_name.iden, data_member_type->get_name());
				}
			}
			return builder.build();
		TYPECHECK_END

		TYPECHECK_BEGIN(function_decl)
			if(node.children.empty()){}
			else if(node.children.size() != 1 || node.children.front()->hash() != syntax::node::block{}.hash())
			{
				sem_assert(false, "function declaration AST node should have either 0 or 1 children (that child being a block). Instead, it has {} children, first one being a \"{}\"", node.children.size(), node.children.front()->name());
			}
			for(const auto& param : node.params.decls)
			{
				if(node.children.empty())
				{
					param.impl_should_add_to_current_scope = false;
				}
				param.impl_is_defined_before_parent_block = true;
				GETTYPE(param);
			}
			return tsys.get_type(node.return_type_name.iden);
		TYPECHECK_END

		TYPECHECK_BEGIN(function_call)
			return tsys.get_type(node.return_type_name.iden);
		TYPECHECK_END

		TYPECHECK_BEGIN(namespace_access)
			std::string full_namespace_name;
			auto parts = node.lhs_parts;
			while(parts.size())
			{
				full_namespace_name += parts.front() + "::";
				parts.erase(parts.begin());
			}
			// todo: actually use the namespace name?
			return GETTYPE((*node.rhs.expr));
		TYPECHECK_END

		TYPECHECK_BEGIN(if_statement)
			type_ptr cond_ty = GETTYPE(node.cond);
			sem_assert(*cond_ty == primitive_type{primitive::boolean}, "evaluated type of if-statement condition must be a bool, but you provided a {}", cond_ty->get_name());
			return nullptr;
		TYPECHECK_END

		TYPECHECK_BEGIN(else_statement)
			if(!node.cond.is_null())
			{
				GETTYPE(node.cond);
			}
			return nullptr;
		TYPECHECK_END

		TYPECHECK_BEGIN(meta_region)
			return nullptr;
		TYPECHECK_END

		TYPECHECK_BEGIN(alias)
			tsys.make_alias(node.alias_name.iden, GETTYPE(node.type_value_expr)->get_name());
			return nullptr;
		TYPECHECK_END
		
		TYPECHECK_BEGIN(expression)
			using type = syntax::node::expression::type;
			switch(node.t)
			{
				case type::integer_literal:
					return tsys.get_primitive_type(primitive::i64);
				break;
				case type::decimal_literal:
					return tsys.get_primitive_type(primitive::f64);
				break;
				case type::string_literal:
					// todo: const?
					return tsys.get_primitive_type(primitive::u8)->ref();
				break;
				case type::char_literal:
					return tsys.get_primitive_type(primitive::u8);
				break;
				case type::bool_literal:
					return tsys.get_primitive_type(primitive::boolean);
				break;
				case type::null_literal:
					// todo: weak?
					return tsys.get_primitive_type(primitive::i8)->ref();
				break;
				case type::parenthesised_expression:
					return GETTYPE((*node.expr.get()));
				break;
				case type::function_call:
					return tsys.get_type(static_cast<syntax::node::function_call*>(node.expr.get())->return_type_name.iden);
				break;
				case type::return_statement:
					if(node.expr == nullptr)
					{
						return nullptr;
					}
					return GETTYPE((*static_cast<syntax::node::expression*>(node.expr.get())));
				break;
			case type::cast:
			{
				// get the extra expression's type.
				// node.extra is either an identifier or yet another expression.
				sem_assert_ice(node.extra != nullptr, "in a cast expression, the `extra` (rhs) must be either an expression or an identifier. it's a nullptr. parse reduction has gone awry");
				auto hash = node.extra->hash();
				sem_assert(hash == syntax::node::identifier{}.hash(), "rhs of cast expression x@y should be an identifier. what you gave me was a \"{}\"", node.extra->name());
				return GETTYPE((*node.extra));
			}
			break;
			case type::deref:
				return GETTYPE((*node.expr))->deref();
			break;
			case type::ref:
				return GETTYPE((*node.expr))->ref();
			break;
			case type::eqcompare:
				return tsys.get_primitive_type(primitive::boolean);
			break;
			case type::neqcompare:
				return tsys.get_primitive_type(primitive::boolean);
			break;
			case type::struct_initialiser:
				if(node.expr->hash() == syntax::node::identifier{}.hash())
				{
					return tsys.get_type(static_cast<syntax::node::identifier*>(node.expr.get())->iden);
				}
				else if(node.expr->hash() == syntax::node::namespace_access{}.hash())
				{
					return GETTYPE((*node.expr));
				}
				else
				{
					sem_assert(false, "struct initialiser lhs should always be either an identifier or a namespace access, instead you've provided a {}", node.expr->name());
					ILL_FORMED;
				}
			break;
			case type::typeinfo:
				return tsys.get_type("typeinfo");
			break;
			case type::namespace_access:
			[[fallthrough]];
			case type::addition:
			[[fallthrough]];
			case type::subtraction:
			[[fallthrough]];
			case type::multiplication:
			[[fallthrough]];
			case type::division:
			[[fallthrough]];
			case type::identifier:
			[[fallthrough]];
			case type::defer:
			{
				return GETTYPE((*node.expr));
			}
			break;
			case type::dot_access:
			{
				type_ptr lhs_ty = GETTYPE((*node.expr));
				sem_assert(lhs_ty->is_struct(), "lhs of dot-access expression should be a struct type, instead you passed {} {}", lhs_ty->hint_name(), lhs_ty->get_name());
				const auto& struct_ty = static_cast<const struct_type&>(*lhs_ty);

				auto rhs_expr = static_cast<const syntax::node::expression*>(node.extra.get());
				sem_assert(node.extra->hash() == syntax::node::expression{}.hash(), "rhs of dot-access expression must be an expression, instead you have provided a {}", node.extra->name());
				auto rhs_hash = rhs_expr->expr->hash();
				if(rhs_hash == syntax::node::identifier{}.hash())
				{
					std::string_view member_name = static_cast<const syntax::node::identifier*>(rhs_expr->expr.get())->iden;
					// should be a data member
					for(const auto& member : struct_ty.members)
					{
						if(member.name == member_name)
						{
							return member.ty->unique_clone();
						}
					}
					sem_assert(false, "struct {} has no such member named {}", struct_ty.get_name(), member_name);
				}
				else if(rhs_hash == syntax::node::function_call{}.hash())
				{
					const auto* call = static_cast<const syntax::node::function_call*>(rhs_expr->expr.get());
					return GETTYPE((*call));
				}
				else
				{
					sem_assert(false, "rhs of dot-access is of expression-type \"{}\", which is unexpected.", syntax::node::expression::type_names[static_cast<int>(rhs_expr->t)]);
				}
				ILL_FORMED;
			}
			break;
			case type::assign:
				// lhs = rhs
				// todo: make sure rhs is convertible to rhs and lhs is not const.
				return GETTYPE((*node.extra));
			break;
				default:
					sem_assert(false, "semantic analysis for expression type {} is NYI", syntax::node::expression::type_names[static_cast<int>(node.t)]);
					ILL_FORMED;
				break;
			}
		TYPECHECK_END
	}
}