#include "semal.hpp"
#include "diag.hpp"

namespace semal
{
	#define sem_assert_ice(expr, inode_ptr, fmt, ...) diag::assert_that(expr, error_code::ice, "at {}: {}", inode_ptr->loc.to_string(), std::format(fmt, ##__VA_ARGS__))
	#define sem_assert(expr, inode_ptr, fmt, ...) diag::assert_that(expr, error_code::semal, "at {}: {}", inode_ptr->loc.to_string(), std::format(fmt, ##__VA_ARGS__))

	identifier::identifier(const type_system& tsys, const syntax::node::identifier& node):
	ty(nullptr),
	node(&node)
	{
		// first try just treating it as a typename (this is perfectly valid e.g myvar@i64 (rhs is i64 identifier))
		this->ty = tsys.get_type(node.iden);
		if(this->ty == nullptr)
		{
			// its a variable name or a parameter.
			diag::error(error_code::nyi, "semantic analysis for identifiers that aren't directly typenames (iden: {}) is NYI", node.iden);
		}
	}

	const itype& identifier::get_type() const
	{
		return *this->ty;
	}

	expression::expression(const type_system& tsys, const syntax::node::expression& node):
	ty(nullptr),
	node(&node)
	{
		// figure out the type evaluated by any expression. ALOT of code incoming.
		using type = syntax::node::expression::type;
		switch(node.t)
		{
			case type::integer_literal:
				this->ty = tsys.get_type("i64");
			break;
			case type::decimal_literal:
				this->ty = tsys.get_type("f64");
			break;
			case type::string_literal:
				this->ty = tsys.get_type("u8")->ref();
			break;
			case type::char_literal:
				this->ty = tsys.get_type("u8");
			break;
			case type::bool_literal:
				this->ty = tsys.get_type("bool");
			break;
			case type::null_literal:
				// todo: weak qualifier
				this->ty = tsys.get_type("i8")->ref();
			break;
			case type::parenthesised_expression:
				this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.expr.get())).ty;
			break;
			case type::function_call:
				// get the return type of the function.
				this->ty = tsys.get_type(static_cast<syntax::node::function_call*>(node.expr.get())->return_type_name.iden);
			break;
			case type::return_statement:
				// just get the type of the returned expression (or u0 if its just `return`)
				if(node.expr == nullptr)
				{
					this->ty = tsys.get_type("u0");
				}
				else
				{
					this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.expr.get())).ty;
				}
			break;
			case type::cast:
			{
				// get the extra expression's type.
				// node.extra is either an identifier or yet another expression.
				sem_assert_ice(node.extra != nullptr, (&node), "in a cast expression, the `extra` (rhs) must be either an expression or an identifier. it's a nullptr. parse reduction has gone awry");
				auto hash = node.extra->hash();
				if(hash == syntax::node::identifier{}.hash())
				{
					this->ty = identifier(tsys, *static_cast<syntax::node::identifier*>(node.extra.get())).ty;
				}
				else if(hash == syntax::node::expression{}.hash())
				{
					this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.extra.get())).ty;
				}
				else
				{
					sem_assert_ice(false, (&node), "in a cast expression, the `extra` (rhs) must be either an expression or an identifier. it is infact a {}. parse reduction has gone awry. hash: {} (iden: {}, expr: {})", node.name(), hash, syntax::node::identifier{}.hash(), syntax::node::expression{}.hash());
				}
			}
			break;
			case type::deref:
				this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.expr.get())).ty->deref();
			break;
			case type::ref:
				this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.expr.get())).ty->ref();
			break;
			case type::defer:
				this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.expr.get())).ty;
			break;
			case type::eqcompare:
				this->ty = tsys.get_type("bool");
			break;
			case type::neqcompare:
				this->ty = tsys.get_type("bool");
			break;
			case type::struct_initialiser:
				sem_assert(node.expr->hash() == syntax::node::identifier{}.hash(), (&node), "struct initialiser expression lhs must be an identifier, but instead it is a {}", node.expr->name());
				this->ty = tsys.get_type(static_cast<syntax::node::identifier*>(node.expr.get())->iden);
			break;
			case type::typeinfo:
				this->ty = tsys.get_type("typeinfo");
			break;
			case type::namespace_access:
			{
				sem_assert(node.expr->hash() == syntax::node::namespace_access{}.hash(), (&node), "namespace access expression lhs must be a namespace access, but instead it is a {}", node.expr->name());
				auto access = static_cast<syntax::node::namespace_access*>(node.expr.get());
				this->ty = namespace_access(tsys, *access).evaluated_ty;
			}
			break;
			case type::assign:
				// lhs = rhs
				// todo: make sure rhs is convertible to rhs and lhs is not const.
				this->ty = expression(tsys, *static_cast<syntax::node::expression*>(node.extra.get())).ty;
			break;
			default:
				sem_assert(false, this->node, "semantic analysis for expression type {} is NYI", syntax::node::expression::type_names[static_cast<int>(node.t)]);
			break;
		}
	}

	const itype& expression::get_type() const
	{
		return *this->ty;
	}

	variable_decl::variable_decl(const type_system& tsys, const syntax::node::variable_decl& node):
	ty(),
	node(&node)
	{
		// variable declaration. getting its type is simple, *unless* its an inferred typename e.g `mytype ::= get_callback_count();`
		if(node.type_name.iden != syntax::node::inferred_typename)
		{
			// easy one: typename explicitly specified
			this->ty = tsys.get_type(node.type_name.iden);
		}
		else
		{
			// inferred typename. the type of our variable decl is the type evaluated by its initialiser expression.
			// obviously, it better have an impression.
			sem_assert(!this->node->expr.is_null(), this->node, "variable declaration {} does not explicitly specify its type but also doesn't have an initialiser. if you want to infer the type, you must give it a valid initialiser at the point of declaration.", this->get_name());
			this->ty = expression(tsys, this->node->expr).get_type().unique_clone();
		}
	}

	std::string_view variable_decl::get_name() const
	{
		return this->node->var_name.iden;
	}

	const itype& variable_decl::get_type() const
	{
		sem_assert_ice(this->ty != nullptr, this->node, "semal variable {}'s ast node was nullptr", this->get_name());
		return *this->ty;
	}

	function_decl::function_decl(const type_system& tsys, const syntax::node::function_decl& node):
	return_ty(tsys.get_type(node.return_type_name.iden)),
	node(&node),
	param_types()
	{
		// populate param types.
		for(const syntax::node::variable_decl& param : node.params.decls)
		{
			this->param_types.push_back(variable_decl(tsys, param).get_type().unique_clone());
		}
	}

	std::string_view function_decl::get_name() const
	{
		return this->node->func_name.iden;
	}

	const itype& function_decl::get_return_type() const
	{
		return *this->return_ty;
	}

	std::span<const type_ptr> function_decl::get_param_types() const
	{
		return this->param_types;
	}

	struct_decl::struct_decl(type_system& tsys, const syntax::node::struct_decl& node):
	ty(nullptr),
	node(&node)
	{
		// note: struct types don't exist in the type system by default because they are of course written by the programmer.
		// so we need: a mutable reference to the type system
		// and now we go ahead and construct the type and insert it into the type system.
		sem_assert_ice(this->node->children.size() == 1 && this->node->children.front()->hash() == syntax::node::block{}.hash(), this->node, "Struct declaration AST node must have a single child: a block. instead, it has {} children{}", this->node->children.size(), this->node->children.size() ? std::format(" (the first of which is a {})", this->node->children.front()->name()) : "");

		type_system::struct_builder builder = tsys.make_struct(node.struct_name.iden);

		// get the children of the block. this is where methods and data member variable declarations will be.
		// however, possible TODO: you will almost certainly want to extract the method declarations here too, so it's easy to lookup by method name.
		for(const syntax::node_ptr& ptr : this->node->children.front()->children)
		{
			// the data members of a struct are encoded in the type. methods however are *not*
			if(ptr->hash() == syntax::node::variable_decl{}.hash())
			{
				const auto& data_member = static_cast<const syntax::node::variable_decl&>(*ptr);
				builder.add_member(data_member.var_name.iden, std::string{variable_decl(tsys, data_member).get_type().get_name()});
			}
		}
		builder.build();
	}

	const itype& struct_decl::get_type() const
	{
		sem_assert_ice(this->ty != nullptr, this->node, "semal struct's {}'s ast node was nullptr", this->node->struct_name.iden);
		return *this->ty;
	}

	namespace_access::namespace_access(const type_system& tsys, const syntax::node::namespace_access& access):
	node(&access),
	evaluated_ty(nullptr)
	{
		// rhs should either be:
		// - an identifier (e.g bar in foo::bar) representing a struct or global variable
		// - a function call (e.g mynamespace::function(...)) calling a function in a certain namespace.
		auto parts = access.lhs_parts;
		std::string full_namespace_name = "";
		while(parts.size())
		{
			full_namespace_name += parts.front() + "::";
			parts.erase(parts.begin());
		}
		// todo: actually use the full namespace name. none of that context is used currently.
		if(access.rhs.expr->hash() == syntax::node::identifier{}.hash())
		{
			std::string iden = static_cast<const syntax::node::identifier*>(access.rhs.expr.get())->iden;
			diag::warning("at {}: type checking for {}{} is NYI", access.loc.to_string(), full_namespace_name, iden);
		}
		else if(access.rhs.expr->hash() == syntax::node::function_call{}.hash())
		{
			const auto& call = static_cast<const syntax::node::function_call*>(access.rhs.expr.get());
			this->evaluated_ty = tsys.get_type(call->return_type_name.iden);
		}
		else
		{
			sem_assert(false, (&access), "right-hand-side of namespace access should be either an identifier or a function call. instead, you've provided a {}", access.rhs.expr->name());
		}
	}

	block::block(type_system& tsys, const syntax::node::block& node):
	node(&node)
	{
		for(const auto& child : this->node->children)
		{
			analyse(this->node, tsys);
		}
	}

	void analyse(const syntax::inode* ast, type_system& tsys)
	{
		#define SEMAL_CASE(NODETY) if(ast->hash() == syntax::node::NODETY{}.hash()){NODETY{tsys, *static_cast<const syntax::node::NODETY*>(ast)};}
		SEMAL_CASE(identifier)
		else SEMAL_CASE(expression)
		else SEMAL_CASE(variable_decl)
		else SEMAL_CASE(function_decl)
		else SEMAL_CASE(struct_decl);
		for(const auto& child : ast->children)
		{
			if(child != nullptr)
			{
				analyse(child.get(), tsys);
			}	
		}
	}
}