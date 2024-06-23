#include "ast.hpp"

namespace syntax
{
	node_ptr make_node(const lex::token& t)
	{
		auto ret = std::make_unique<node::unparsed_token>(t);
		return ret;
	}
}