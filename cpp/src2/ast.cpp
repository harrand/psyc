#include "ast.hpp"
#include "diag.hpp"
#include <stack>
#include <iostream>
#include <set>

namespace syntax
{
	namespace node
	{
		expression::expression():
		expr(syntax::nodenew{}),
		extra(syntax::nodenew{})
		{

		}

		bool expression::is_null() const
		{
			return !this->expr->has_value() || this->t == type::_unknown;
		}

		std::string expression::to_string() const
		{
			return std::format("expr-{}({}{})", expression::type_names[static_cast<int>(this->t)], this->expr->to_string(), this->extra->has_value() ? std::format(", {}", this->extra->to_string()) : "");
		}

		if_statement::if_statement(expression cond, block blk, bool is_static):
		cond(cond),
		is_static(is_static)
		{
			this->children.push_back(syntax::nodenew{.payload = blk});
		}

		else_statement::else_statement(expression cond, block blk):
		cond(cond)
		{
			this->children.push_back(syntax::nodenew{.payload = blk});
		}
	}

	bool nodenew::has_value() const
	{
		return !std::holds_alternative<std::monostate>(this->payload);
	}

	std::string nodenew::to_string() const
	{
		std::string ret;
		std::visit(util::overload
		{
			[&ret](std::monostate)
			{
				ret = "<empty>";
			},
			[&ret](auto arg)
			{
				ret = arg.to_string();
			}
		}, this->payload);
		return ret;
	}

	const char* nodenew::name() const
	{
		PROFZONE("node name");
		return payload_names[this->payload.index()];
	}

	std::size_t nodenew::hash() const
	{
		PROFZONE("node hash");
		std::size_t ret = std::hash<std::size_t>{}(this->payload.index());
		if(std::holds_alternative<syntax::node::unparsed_token>(this->payload))
		{
			ret ^= std::get<syntax::node::unparsed_token>(this->payload).hash();
		}
		return ret;
	}

	std::vector<boxed_node>& nodenew::children()
	{
		std::vector<boxed_node>* ret = nullptr;
		std::visit(util::overload
		{
			[&ret](std::monostate)
			{
				__builtin_unreachable();	
			},
			[&ret](auto& arg)
			{
				ret = &arg.children;
			}
		}, this->payload);
		return *ret;
	}

	const std::vector<boxed_node>& nodenew::children() const
	{
		const std::vector<boxed_node>* ret = nullptr;
		std::visit(util::overload
		{
			[&ret](std::monostate)
			{
				diag::ice("attempt to get location (read-only) of null node");
			},
			[&ret](auto& arg)
			{
				ret = &arg.children;
			}
		}, this->payload);
		return *ret;
	}

	srcloc& nodenew::loc()
	{
		srcloc* ret = nullptr;
		std::visit(util::overload
		{
			[&ret](std::monostate)
			{
				diag::ice("attempt to get location (writable) of null node");
			},
			[&ret](auto& arg)
			{
				ret = &arg.loc;
			}
		}, this->payload);
		return *ret;
	}

	const srcloc& nodenew::loc() const
	{
		const srcloc* ret = nullptr;
		std::visit(util::overload
		{
			[&ret](std::monostate)
			{
				__builtin_unreachable();	
			},
			[&ret](auto& arg)
			{
				ret = &arg.loc;
			}
		}, this->payload);
		return *ret;
	}

	void nodenew::pretty_print() const
	{
		std::stack<const nodenew*> node_list;
		std::stack<std::size_t> indents;
		std::set<std::size_t> parents = {};
		node_list.push(this);
		indents.push(0);
		while(node_list.size())
		{
			const nodenew* cur = node_list.top();
			node_list.pop();
			std::size_t indent = indents.top();
			indents.pop();
			if(indent > 0)
			{
				for(std::size_t i = 0; i < indent; i++)
				{
					if(std::find(parents.begin(), parents.end(), i - 1) != parents.end())
					{
						std::cout << "│ ";
					}
					else
					{
						std::cout << "  ";
					}
				}
				if(indent > 1 && cur->children().empty() && (indents.size() && indents.top() != indent))
				{
					std::cout << "└─";
				}
				else
				{
					std::cout << "├─";
				}
			}
			if(cur != nullptr)
			{
				std::cout << cur->to_string() << "\n";
			}
			for(std::size_t i = 0; i < cur->children().size(); i++)
			{
				const nodenew& child = *cur->children()[cur->children().size() - 1 - i];
				node_list.push(&child);
				indents.push(indent + 1);
			}
			while(parents.size() && indent < *parents.rbegin())
			{
				parents.erase(std::prev(parents.end()));
			}
			if(this->children().size())
			{
				parents.insert(indent);
			}
		}
	}

	std::string escape(std::string_view literal)
	{
		std::string ret;
		static const std::unordered_map<std::string_view, char> escape_map = {
			{"\\0", '\0'}, // Null terminator
			{"\\a", '\a'}, // Bell (alert)
			{"\\b", '\b'}, // Backspace
			{"\\f", '\f'}, // Formfeed
			{"\\n", '\n'}, // Newline (line feed)
			{"\\r", '\r'}, // Carriage return
			{"\\t", '\t'}, // Horizontal tab
			{"\\v", '\v'}, // Vertical tab
			{"\\\\", '\\'}, // Backslash
			{"\\'", '\''}, // Single quote
			{"\\\"", '\"'}, // Double quote
			{"\\?", '\?'}  // Question mark
		};
		if(literal.size() == 1)
		{
			return std::string{literal};
		}
		for(std::size_t i = 0; i < literal.size(); i++)
		{
			std::string_view substr{literal.data() + i, 2};
			auto iter = escape_map.find(substr);
			if(iter != escape_map.end())
			{
				ret += iter->second;
				i++;
			}
			else
			{
				ret += literal[i];
			}
		}
		return ret;
	}

	nodenew make_node(const lex::token& t)
	{
		nodenew ret;
		switch(t.t)
		{
			case lex::type::identifier:
				ret.payload = node::identifier(t.lexeme);
			break;
			case lex::type::integer_literal:
				ret.payload = node::integer_literal(std::stoi(t.lexeme));
			break;
			case lex::type::decimal_literal:
				ret.payload = node::decimal_literal(std::stod(t.lexeme));
			break;
			case lex::type::null_literal:
				ret.payload = node::null_literal();
			break;
			case lex::type::char_literal:
			{
				std::string charlit = t.lexeme;
				diag::assert_that(!charlit.empty(), error_code::lex, "empty char-literal detected at {}. char literals must contain a single character.", t.meta_srcloc.to_string());
				charlit = escape(charlit);
				diag::assert_that(charlit.size() == 1, error_code::lex, "char-literal must consist of 1 character, but \'{}\' contains {}", t.lexeme, charlit.size());
				ret.payload = node::char_literal(charlit.front());
			}
			break;
			case lex::type::bool_literal:
			{
				bool val = false;
				if(t.lexeme == "true")
				{
					val = true;
				}
				else if(t.lexeme == "false")
				{
					val = false;
				}
				else
				{
					diag::assert_that(false, error_code::type, "bool literal had lexeme \"{}\", but it should only be \"true\" or \"false\"", t.lexeme);
				}
				ret.payload = node::bool_literal(val);
			}
			break;
			case lex::type::string_literal:
			{
				std::string stringlit = t.lexeme;
				ret.payload = node::string_literal(escape(stringlit));
			}
			break;
			default:
				ret.payload = node::unparsed_token{.tok = t};
			break;
		}
		if(ret.has_value())
		{
			ret.loc() = t.meta_srcloc;
		}
		return ret;
	}
}