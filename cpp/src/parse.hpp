#include "lex.hpp"
#include <variant>

namespace parser
{
	struct ast
	{
		struct identifier{std::string name;};
		struct function_call
		{
			identifier function_name;
			std::vector<identifier> parameters;
		};

		struct compound_statement
		{

		};

		struct return_statement
		{
			std::string value;
		};

		struct function_definition
		{
			identifier function_name;
			identifier return_type;
			compound_statement impl;
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

		node program;
		std::vector<std::size_t> path = {};
	};

	ast parse(lexer::const_token_view tokens);
}