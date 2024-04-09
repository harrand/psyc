#ifndef PSYC_PARSE_HPP
#define PSYC_PARSE_HPP
#include "lex.hpp"
#include <variant>
#include <iostream>
#include <limits>

namespace parser
{
	struct ast
	{
		struct identifier{std::string name;};
		struct function_call
		{
			identifier function_name;
			std::vector<identifier> parameters;
			inline void pretty_print() const
			{
				std::cout << "function-call: " << this->function_name.name << "(";
				for(const auto& param : this->parameters)
				{
					std::cout << param.name << " ";
				}
				std::cout << ")";
			}
		};


		struct return_statement
		{
			std::string value;
			inline void pretty_print() const
			{
				std::cout << "return-statement: " << this->value;
			}
		};

		struct function_definition
		{
			identifier function_name;
			identifier return_type;
			inline void pretty_print() const
			{
				std::cout << "function-definition: " << this->function_name.name << " -> " << this->return_type.name;
			}
		};

		struct metadata
		{
			std::size_t line_number = std::numeric_limits<std::size_t>::max();
		};
		struct node
		{
			using payload_t = std::variant<std::monostate, function_call, return_statement, function_definition>;
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