#ifndef PSYC_PARSE_HPP
#define PSYC_PARSE_HPP
#include "lex.hpp"
#include <variant>
#include <iostream>
#include <ios>
#include <limits>

namespace parser
{
	struct ast
	{
		struct identifier
		{
			std::string name;
			inline void pretty_print() const
			{
				std::cout << "identifier: " << this->name;
			}
		};

		struct integer_literal
		{
			std::int64_t value;
			inline void pretty_print() const
			{
				std::cout << "integer-literal: " << this->value;
			}
		};

		struct decimal_literal
		{
			double value;
			inline void pretty_print() const
			{
				// note: cout on double likes to round it if its got no fractional part.
				// this is poopoo. use fixed as a workaround
				std::cout << "decimal-literal: " << std::fixed << this->value;
			}
		};

		struct string_literal
		{
			std::string value;
			inline void pretty_print() const
			{
				std::cout << "string-literal: " << this->value;
			}
		};

		struct function_call;

		using expression = std::variant<integer_literal, decimal_literal, function_call, identifier>;

		struct variable_declaration
		{
			identifier name;
			identifier type;
			inline void pretty_print() const
			{
				std::cout << this->name.name << " : " << this->type.name;
			}
		};
		struct function_call
		{
			identifier function_name;
			std::vector<expression> parameters;
			inline void pretty_print() const
			{
				std::cout << "function-call: " << this->function_name.name << "(";
				for(const auto& param : this->parameters)
				{
					std::visit([](auto&& arg)
					{
						arg.pretty_print();
					}, param);
				}
				std::cout << ")";
			}
		};

		struct return_statement
		{
			expression value;
			inline void pretty_print() const
			{
				std::cout << "return-statement: ";
				std::visit([](auto&& arg)
				{
					arg.pretty_print();
				}, this->value);
			}
		};

		struct function_definition
		{
			identifier function_name;
			std::vector<variable_declaration> parameters = {};
			identifier return_type;
			inline void pretty_print() const
			{
				std::cout << "function-definition: " << this->function_name.name;
				std::cout << "(";
				for(std::size_t i = 0; i < this->parameters.size(); i++)
				{
					this->parameters[i].pretty_print();
					if(i < (this->parameters.size() - 1))
					{
						std::cout << ", ";
					}
				}
				std::cout << ") -> " << this->return_type.name;
			}
		};

		struct metadata
		{
			std::size_t line_number = std::numeric_limits<std::size_t>::max();
		};
		struct node
		{
			using payload_t = std::variant<std::monostate, variable_declaration, function_call, return_statement, integer_literal, decimal_literal, string_literal, function_definition>;
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