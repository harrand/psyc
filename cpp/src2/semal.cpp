#include "semal.hpp"
#include "diag.hpp"

namespace semal
{
	#define sem_assert_ice(expr, inode_ptr, fmt, ...) diag::assert_that(expr, error_code::ice, "at {}: {}", inode_ptr->loc.to_string(), std::format(fmt, ##__VA_ARGS__))
	#define sem_assert(expr, inode_ptr, fmt, ...) diag::assert_that(expr, error_code::semal, "at {}: {}", inode_ptr->loc.to_string(), std::format(fmt, ##__VA_ARGS__))

	identifier::identifier(const type_system& tsys, syntax::node::identifier& node):
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

	expression::expression(const type_system& tsys, syntax::node::expression& node):
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
					sem_assert_ice(false, (&node), "in a cast expression, the `extra` (rhs) must be either an expression or an identifier. it's not null but not an expr or iden either. parse reduction has gone awry. hash: {} (iden: {}, expr: {})", hash, syntax::node::identifier{}.hash(), syntax::node::expression{}.hash());
				}
			}
			break;
			default:
				diag::error(error_code::nyi, "semantic analysis for expression type {} is NYI", syntax::node::expression::type_names[static_cast<int>(node.t)]);
			break;
		}
	}

	variable::variable(const type_system& tsys, syntax::node::variable_decl& node):
	ty(),
	node(&node)
	{
		if(node.type_name.iden != syntax::node::inferred_typename)
		{
			this->ty = tsys.get_type(node.type_name.iden);
		}
		else
		{
			sem_assert(!this->node->expr.is_null(), this->node, "variable declaration {} does not explicitly specify its type but also doesn't have an initialiser. if you want to infer the type, you must give it a valid initialiser at the point of declaration.", this->get_name());
			this->ty = expression(tsys, this->node->expr).get_type().unique_clone();
		}
	}

	std::string_view variable::get_name() const
	{
		return this->node->var_name.iden;
	}

	const itype& variable::get_type() const
	{
		sem_assert_ice(this->ty != nullptr, this->node, "semal variable {}'s ast node was nullptr", this->get_name());
		return *this->ty;
	}
}