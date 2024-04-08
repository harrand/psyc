#include "lex.hpp"
#include <variant>
#include <iostream>

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

		struct node
		{
			using payload_t = std::variant<std::monostate, function_call, return_statement, function_definition>;
			payload_t payload;
			std::vector<node> children;
		};

		const node& current() const;
		node& current();
		void push(node n);
		void push(node::payload_t payload);
		void pop();
		void pretty_print();

		node program;
		std::vector<std::size_t> path = {};
	};

	ast parse(lexer::const_token_view tokens);
}