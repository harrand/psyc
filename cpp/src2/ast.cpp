#include "ast.hpp"
#include <stack>
#include <iostream>

namespace syntax
{
	void inode::pretty_print() const
	{
		std::stack<const inode*> node_list;
		std::stack<std::size_t> indents;
		node_list.push(this);
		indents.push(0);
		while(node_list.size())
		{
			const inode* cur = node_list.top();
			node_list.pop();
			std::size_t indent = indents.top();
			indents.pop();
			for(std::size_t i = 0; i < indent; i++)
			{
				std::cout << "  ";
			}
			if(cur != nullptr)
			{
				std::cout << cur->to_string() << "\n";
			}
			for(std::size_t i = 0; i < cur->children.size(); i++)
			{
				const inode* child = cur->children[cur->children.size() - 1 - i].get();
				node_list.push(child);
				indents.push(indent + 1);
			}
		}
	}

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
			case lex::type::null_literal:
				ret = std::make_unique<node::null_literal>();
			break;
			default:
				ret = std::make_unique<node::unparsed_token>(t);
			break;
		}
		ret->loc = t.meta_srcloc;
		return ret;
	}
}