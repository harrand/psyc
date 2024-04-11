#ifndef PSYC_PARSE_HPP
#define PSYC_PARSE_HPP
#include "lex.hpp"
#include "util.hpp"
#include <variant>
#include <limits>
#include <optional>
#include <format>

namespace parser
{
	struct ast
	{
		struct unary_operator
		{
			std::string to_string() const
			{
				return "";
			};
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

		struct string_literal
		{
			std::string val;
			std::string to_string() const
			{
				return std::format("string-literal: {}", val);
			};
		};

		struct expression;

		struct function_call
		{
			std::string function_name;
			std::vector<expression> params = {};

			std::string to_string() const
			{
				std::string params_stringified = "";
				for(const expression& expr : this->params)
				{
					params_stringified += expr.to_string();
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
				integer_literal,
				decimal_literal,
				string_literal,
				function_call
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
						ret += std::format("{}{}", op.to_string(), boxed_expr->to_string());
					}
					else
					{
						ret += arg.to_string();
					}
				}, this->expr);
				return ret;
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

			std::string to_string() const
			{
				std::string params_string = "";
				for(const auto& param : this->params)
				{
					params_string += param.to_string();
				}
				return std::format("function-declaration: {} : ({}) -> {}", this->function_name, params_string, this->return_type);
			}
		};

		struct metadata
		{
			std::size_t line_number = std::numeric_limits<std::size_t>::max();
		};
		struct node
		{
			using payload_t = std::variant<std::monostate, variable_declaration, expression, function_definition>;
			payload_t payload;
			metadata meta;
			std::vector<node> children;
		};
		using path_t = std::vector<std::size_t>;
		using path_view_t = std::span<const std::size_t>;

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

	ast parse(lexer::const_token_view tokens);
}
#endif // PSYC_PARSE_HPP