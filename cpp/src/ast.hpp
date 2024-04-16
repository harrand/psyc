#ifndef PSYC_AST_HPP
#define PSYC_AST_HPP
#include "lex.hpp"
#include "util.hpp"
#include "diag.hpp"
#include <variant>
#include <limits>
#include <optional>
#include <format>

struct ast
{
	struct unary_operator
	{
		lexer::token::type type;
		std::string to_string() const
		{
			diag::assert_that(
				type == lexer::token::type::minus ||
				type == lexer::token::type::bitwise_complement ||
				type == lexer::token::type::logical_negation,
				"internal compiler error: parsed a unary_operator via lexer token type that doesn't represent a unary operator."
			);
			return std::format("unary-operator \"{}\"", lexer::token_type_names[static_cast<int>(this->type)]);
		};
	};

	struct binary_operator
	{
		lexer::token::type type;
		std::string to_string() const
		{
			diag::assert_that(
				type == lexer::token::type::minus ||
				type == lexer::token::type::plus ||
				type == lexer::token::type::double_equals ||
				type == lexer::token::type::equals ||
				type == lexer::token::type::not_equals,
				"internal compiler error: parsed a binary via lexer token type that doesn't represent a binary operator."
			);
			return std::format("binary-operator \"{}\"", lexer::token_type_names[static_cast<int>(this->type)]);
		};
	};

	struct identifier
	{
		std::string name;
		std::string to_string() const
		{
			return std::format("identifier: {}", this->name);
		}
	};

	struct integer_literal
	{
		int val;
		std::string to_string() const
		{
			return std::format("integer-literal: {}", val);
		};
	};

	struct decimal_literal
	{
		double val;	
		std::string to_string() const
		{
			return std::format("decimal-literal: {}", val);
		};
	};

	struct char_literal
	{
		char val;
		std::string to_string() const
		{
			return std::format("char-literal: {}", val);
		}
	};

	struct string_literal
	{
		std::string val;
		std::string to_string() const
		{
			return std::format("string-literal: {}", val);
		};
	};

	struct bool_literal
	{
		bool val;
		std::string to_string() const
		{
			return std::format("boolean-literal: {}", val ? "true" : "false");
		}
	};

	struct expression;

	struct function_call
	{
		std::string function_name;
		std::vector<expression> params = {};

		std::string to_string() const
		{
			std::string params_stringified = "";
			for(std::size_t i = 0; i < this->params.size(); i++)
			{
				const expression& expr = this->params[i];
				params_stringified += expr.to_string();
				if(i < (this->params.size() - 1))
				{
					params_stringified += ", ";
				}
			}
			return std::format("function-call: {}({})", this->function_name, params_stringified);
		};
	};
	struct expression
	{
		std::variant
		<
			// boxed because expression at this point is an incomplete type. the heap alloc is entirely unavoidable im afraid.
			std::pair<unary_operator, util::box<expression>>,
			std::tuple<binary_operator, util::box<expression>, util::box<expression>>,
			integer_literal,
			decimal_literal,
			char_literal,
			string_literal,
			bool_literal,
			function_call,
			identifier
		> expr;
		
		std::string to_string() const
		{
			std::string ret = "expression: ";
			std::visit([&ret](auto&& arg)
			{
				using T = std::decay_t<decltype(arg)>;
				if constexpr(std::is_same_v<T, std::pair<unary_operator, util::box<expression>>>)
				{
					const auto&[op, boxed_expr] = arg;
					ret += std::format("{{{}, {}}}", op.to_string(), boxed_expr->to_string());
				}
				else if constexpr(std::is_same_v<T, std::tuple<binary_operator, util::box<expression>, util::box<expression>>>)
				{
					const auto&[op, lhs, rhs] = arg;
					ret += std::format("{{{}, {}, {}}}", op.to_string(), lhs->to_string(), rhs->to_string());
				}
				else
				{
					ret += arg.to_string();
				}
			}, this->expr);
			return ret;
		}
	};

	struct if_statement
	{
		expression condition;	
		std::string to_string() const
		{
			return std::format("if-statement: {}", this->condition.to_string());
		}
	};

	struct for_statement
	{
		ast::expression start, end, loop;
		std::string to_string() const
		{
			return std::format("for-statement: for({}, {}, {})", start.to_string(), end.to_string(), loop.to_string());
		}
	};

	struct return_statement
	{
		std::optional<expression> value;
		std::string to_string() const
		{
			return std::format("return-statement: {}", this->value.has_value() ? this->value->to_string() : "");
		}
	};
	struct variable_declaration
	{
		std::string var_name;
		std::string type_name;
		std::optional<expression> initialiser = std::nullopt;

		std::string to_string() const
		{
			std::string initialiser_string = "";
			if(this->initialiser.has_value())
			{
				initialiser_string = " = " + this->initialiser->to_string();
			}
			return std::format("variable-declaration: {} : {}{}", this->var_name, this->type_name, initialiser_string);
		}
	};

	struct function_definition
	{
		std::string function_name;
		std::vector<variable_declaration> params = {};
		std::string return_type;
		bool is_extern = false;

		std::string to_string() const
		{
			std::string params_string = "";
			for(std::size_t i = 0; i < this->params.size(); i++)
			{
				const variable_declaration& param = this->params[i];
				params_string += param.to_string();
				if(i < (this->params.size() - 1))
				{
					params_string += ", ";
				}
			}
			return std::format("function-declaration: {} : ({}) -> {}", this->function_name, params_string, this->return_type);
		}
	};

	struct struct_definition
	{
		std::string struct_name;

		std::string to_string() const
		{
			return std::format("struct: {}", struct_name);
		}
	};

	struct meta_region
	{
		std::string region_name;
		std::string to_string() const
		{
			return std::format("region: {}", region_name);
		}
	};

	struct metadata
	{
		std::size_t line_number = std::numeric_limits<std::size_t>::max();
	};
	struct node
	{
		using payload_t = std::variant<std::monostate, integer_literal, decimal_literal, char_literal, string_literal, bool_literal, function_call, expression, if_statement, for_statement, return_statement, variable_declaration, function_definition, struct_definition, meta_region>;
		payload_t payload;
		metadata meta;
		std::vector<node> children;
	};

	using path_t = std::vector<std::size_t>;
	using path_view_t = std::span<const std::size_t>;

	std::optional<ast::node> try_find_variable_from(path_t path, std::string_view variable_name) const;
	std::optional<ast::node> try_find_variable(std::string_view variable_name) const;

	const node& current() const;
	node& current();
	path_t current_path() const;
	void push(node n);
	const node& get(path_view_t path) const;
	node& get(path_view_t path);
	void pop();
	void pretty_print();

	node program;
	path_t path = {};
};


#endif // PSYC_AST_HPP