#ifndef PSYC_AST_HPP
#define PSYC_AST_HPP
#include "util.hpp"
#include "lex.hpp"
#include "srcloc.hpp"
#include <string>
#include <format>
#include <variant>
#include <vector>
#include <span>
#include <optional>

struct ast
{
	struct expression;
	using boxed_expression = util::box<expression>;
	struct integer_literal
	{
		std::int64_t val;
		std::string to_string() const
		{
			return std::format("integer-literal({})", val);
		}
		bool operator==(const integer_literal& rhs) const = default;
	};
	struct decimal_literal
	{
		double val;
		std::string to_string() const
		{
			return std::format("decimal-literal({})", val);
		}
		bool operator==(const decimal_literal& rhs) const = default;
	};
	struct bool_literal
	{
		bool val;
		std::string to_string() const
		{
			return std::format("bool-literal({})", val);
		}
		bool operator==(const bool_literal& rhs) const = default;
	};
	struct identifier
	{
		std::string iden;
		std::string to_string() const
		{
			return std::format("identifier({})", iden);
		}
		bool operator==(const identifier& rhs) const = default;
	};

	struct unary_operator
	{
		lex::token op;
		boxed_expression expr;
		std::string to_string() const
		{
			return std::format("unop({} {})", op.lexeme, expr->to_string());
		}
		bool operator==(const unary_operator& rhs) const = default;
	};

	struct binary_operator
	{
		boxed_expression lhs_expr;
		lex::token op;
		boxed_expression rhs_expr;
		std::string to_string() const
		{
			return std::format("biop({} {} {})", lhs_expr->to_string(), op.lexeme, rhs_expr->to_string());
		}
		bool operator==(const binary_operator& rhs) const = default;
	};

	struct variable_declaration
	{
		std::string var_name;	
		std::string type_name;
		std::optional<util::box<expression>> initialiser = std::nullopt;
		std::string to_string() const
		{
			return std::format("variable_declaration({} : {}{})", var_name, type_name, initialiser.has_value() ? std::format(" = {}", initialiser.value()->to_string()) : "");
		}
		bool operator==(const variable_declaration& rhs) const = default;
	};
	struct function_call
	{
		std::string function_name;
		std::vector<boxed_expression> params;

		std::string to_string() const
		{
			std::string params_str = "(";
			for(const auto& param : this->params)
			{
				params_str += param->to_string();
			}
			params_str += ")";
			return std::format("function_call({}({}))", function_name, params_str);
		}
		bool operator==(const function_call& rhs) const = default;
	};

	struct return_statement
	{
		std::optional<boxed_expression> expr;
		std::string to_string() const
		{
			return std::format("return({})", expr.has_value() ? expr.value()->to_string() : "");
		}
		bool operator==(const return_statement& rhs) const = default;
	};

	struct if_statement
	{
		boxed_expression if_expr;
		std::string to_string() const
		{
			return std::format("if({})", if_expr->to_string());
		}
		bool operator==(const if_statement& rhs) const = default;
	};

	struct for_statement
	{
		boxed_expression init_expr;
		boxed_expression cond_expr;
		boxed_expression iter_expr;
		std::string to_string() const
		{
			return std::format("for({}, {}, {})", init_expr->to_string(), cond_expr->to_string(), iter_expr->to_string());
		}
		bool operator==(const for_statement& rhs) const = default;
	};

	struct expression
	{
		std::variant
		<
			ast::unary_operator,
			ast::binary_operator,
			ast::integer_literal,
			ast::decimal_literal,
			ast::bool_literal,
			ast::identifier,
			ast::variable_declaration,
			ast::function_call,
			ast::return_statement,
			ast::if_statement,
			ast::for_statement
		> expr;
		bool capped = false;
		std::string to_string() const
		{
			std::string ret;
			std::visit([&ret](auto&& arg)
			{
				ret = arg.to_string();
			}, expr);
			return std::format("expr({})", ret);
		}
		bool operator==(const expression& rhs) const = default;
	};

	struct block
	{
		std::string to_string() const
		{
			return "block()";
		}
		bool operator==(const block& rhs) const = default;
	};

	struct function_definition
	{
		std::string func_name;
		std::vector<variable_declaration> params = {};
		std::string ret_type;
		bool is_extern = false;
		bool is_builtin = false;
		std::string to_string() const
		{
			std::string params_str = "(";
			for(const auto& param : this->params)
			{
				params_str += param.to_string();
			}
			params_str += ")";
			return std::format("function_definition({} :: ({}) -> {}{})", func_name, params_str, ret_type, is_extern ? " := extern" : "");
		}
		bool operator==(const function_definition& rhs) const = default;
	};

	struct struct_definition
	{
		std::string name;
		std::string to_string() const
		{
			return std::format("struct({})", name);
		}
		bool operator==(const struct_definition& rhs) const = default;
	};

	struct meta_region
	{
		std::string name;
		std::string type;
		std::string to_string() const
		{
			return std::format("meta_region({} : {})", name, type);
		}
		bool operator==(const meta_region& rhs) const = default;
	};

	struct node
	{
		using payload_t = std::variant<std::monostate, integer_literal, decimal_literal, bool_literal, identifier, function_call, if_statement, for_statement, expression, return_statement, variable_declaration, function_definition, struct_definition, block, meta_region>;
		payload_t payload = std::monostate{};
		srcloc meta = {};
		std::vector<node> children = {};

		std::string to_string() const
		{
			std::string ret;
			std::visit([&ret](auto&& arg)
			{
				if constexpr(std::is_same_v<std::decay_t<decltype(arg)>, std::monostate>)
				{
					ret = "<empty>";
				}
				else
				{
					ret = arg.to_string();
				}
			}, this->payload);
			return ret;
		}
		bool operator<(const node& rhs) const
		{
			return this->meta < rhs.meta;
		}
		bool operator==(const node& rhs) const = default;
	};

	using path_t = std::vector<std::size_t>;
	using path_const_view_t = std::span<const std::size_t>;
	using path_view_t = std::span<std::size_t>;
	
	const node& get(path_const_view_t path) const;
	node& get(path_const_view_t path);

	// given a path, retrieve a path pointing to the previous node (i.e the node before this path with the same parent). if it can't be done, return nullopt.
	std::optional<path_t> try_get_previous(path_t path) const;
	// given a path, retrieve a path pointing to the next node (i.e the node after this path with the same parent). if it can't be done, return nullopt.
	std::optional<path_t> try_get_next(path_t path) const;

	const node& current() const;
	node& current();

	void attach_to(ast& parent, const path_t& path) const;
	void pretty_print() const;

	bool operator==(const ast& rhs) const
	{
		return this->root == rhs.root;
	}

	node root;
	path_t current_path = {};
};

namespace std
{
	template<>
	struct hash<ast::path_t>
	{
		std::size_t operator()(const ast::path_t& val) const
		{
			std::size_t seed = val.size();
			for(auto x : val)
			{
				x = ((x >> 16) ^ x) * 0x45d9f3b;
				x = ((x >> 16) ^ x) * 0x45d9f3b;
				x = (x >> 16) ^ x;
				seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
			}
			return seed;
		}
	};
}

#endif // PSYC_AST_HPP