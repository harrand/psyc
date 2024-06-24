#include "ast.hpp"

namespace syntax
{
	node_ptr make_node(const lex::token& t)
	{
		switch(t.t)
		{
			case lex::type::identifier:
				return std::make_unique<node::identifier>(t.lexeme);
			break;
			case lex::type::integer_literal:
				return std::make_unique<node::integer_literal>(std::stoi(t.lexeme));
			break;
			case lex::type::decimal_literal:
				return std::make_unique<node::decimal_literal>(std::stod(t.lexeme));
			break;
			default: break;
		}
		auto ret = std::make_unique<node::unparsed_token>(t);
		return ret;
	}
}