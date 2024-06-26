#include "ast.hpp"

namespace syntax
{
	node_ptr make_node(const lex::token& t)
	{
		node_ptr ret = nullptr;
		switch(t.t)
		{
			case lex::type::identifier:
				ret = std::make_unique<node::identifier>(t.lexeme);
			break;
			case lex::type::integer_literal:
				ret = std::make_unique<node::integer_literal>(std::stoi(t.lexeme));
			break;
			case lex::type::decimal_literal:
				ret = std::make_unique<node::decimal_literal>(std::stod(t.lexeme));
			break;
			default:
				ret = std::make_unique<node::unparsed_token>(t);
			break;
		}
		ret->loc = t.meta_srcloc;
		return ret;
	}
}