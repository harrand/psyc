#include "ast.hpp"
#include "diag.hpp"
#include <stack>
#include <set>
#include <sstream>
#include <cstdio>

namespace syntax
{
	expression::expression():
	expr(syntax::node{}),
	extra(syntax::node{})
	{

	}

	bool expression::is_null() const
	{
		return !this->expr->has_value() || this->t == type::_unknown;
	}

	std::string expression::to_string() const
	{
		return this->expr->to_string();
	}

	if_statement::if_statement(expression cond, block blk, bool is_static):
	cond(cond),
	is_static(is_static)
	{
		this->children.push_back(syntax::node{.payload = blk});
	}

	else_statement::else_statement(expression cond, block blk):
	cond(cond)
	{
		this->children.push_back(syntax::node{.payload = blk});
	}

	bool node::has_value() const
	{
		return !std::holds_alternative<std::monostate>(this->payload);
	}

	std::string node::to_string() const
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

	const char* node::name() const
	{
		PROFZONE("node name");
		return payload_names[this->payload.index()];
	}

	std::size_t node::hash() const
	{
		PROFZONE("node hash");
		std::size_t ret = std::hash<std::size_t>{}(this->payload.index());
		if(std::holds_alternative<syntax::unparsed_token>(this->payload))
		{
			ret ^= std::get<syntax::unparsed_token>(this->payload).hash();
		}
		return ret;
	}

	std::vector<boxed_node>& node::children()
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

	const std::vector<boxed_node>& node::children() const
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

	node& node::evaluate_path(node::path_view_t path)
	{
		if(path.empty())
		{
			return *this;
		}
		auto& children = this->children();
		diag::assert_that(children.size() > path.front(), error_code::ice, "invalid child index {} when evaluating path (only have {} children)", path.front(), children.size());
		return children[path.front()]->evaluate_path(path.subspan(1));
	}

	const node& node::evaluate_path(node::path_view_t path) const
	{
		if(path.empty())
		{
			return *this;
		}
		const auto& children = this->children();
		diag::assert_that(children.size() > path.front(), error_code::ice, "invalid child index {} when evaluating path (only have {} children)", path.front(), children.size());
		return children[path.front()]->evaluate_path(path.subspan(1));
	}

	node& node::evaluate_parent(node::path_view_t path)
	{
		diag::assert_that(path.size(), error_code::ice, "attempt to get parent-path using an empty path.");
		return this->evaluate_path(path.last(path.size() - 1));
	}

	const node& node::evaluate_parent(node::path_view_t path) const
	{
		diag::assert_that(path.size(), error_code::ice, "attempt to get parent-path using an empty path.");
		return this->evaluate_path(path.last(path.size() - 1));
	}

	void node::iterate(std::function<void(path_view_t, node&)> callback, bool recursive, path_t impl_path_dont_touch)
	{
		auto& children = this->children();
		for(std::size_t i = 0; i < children.size(); i++)
		{
			auto path_cpy = impl_path_dont_touch;
			path_cpy.push_back(i);
			callback(path_cpy, *children[i]);
			if(recursive)
			{
				children[i]->iterate(callback, recursive, path_cpy);
			}
		}
	}

	void node::iterate(std::function<void(path_view_t, const node&)> callback, bool recursive, path_t impl_path_dont_touch) const
	{
		const auto& children = this->children();
		for(std::size_t i = 0; i < children.size(); i++)
		{
			auto path_cpy = impl_path_dont_touch;
			path_cpy.push_back(i);
			callback(path_cpy, *children[i]);
			if(recursive)
			{
				children[i]->iterate(callback, recursive, path_cpy);
			}
		}
	}

	srcloc& node::loc()
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

	const srcloc& node::loc() const
	{
		const srcloc* ret = nullptr;
		std::visit(util::overload
		{
			[&ret](std::monostate)
			{
				diag::ice("attempt to get location (read-only) of null node");
			},
			[&ret](auto& arg)
			{
				ret = &arg.loc;
			}
		}, this->payload);
		return *ret;
	}

	void node::pretty_print() const
	{
		PROFZONE("pretty print");
		std::stack<const node*> node_list;
		std::stack<std::size_t> indents;
		std::set<std::size_t> parents = {};
		std::string streamdata;
		streamdata.reserve(4096);
		std::ostringstream stream(std::move(streamdata));
		node_list.push(this);
		indents.push(0);
		while(node_list.size())
		{
			const node* cur = node_list.top();
			node_list.pop();
			std::size_t indent = indents.top();
			indents.pop();
			if(indent > 0)
			{
				for(std::size_t i = 0; i < indent; i++)
				{
					if(std::find(parents.begin(), parents.end(), i - 1) != parents.end())
					{
						stream << "│ ";
					}
					else
					{
						stream << "  ";
					}
				}
				if(indent > 1 && cur->children().empty() && (indents.size() && indents.top() != indent))
				{
					stream << "└─";
				}
				else
				{
					stream << "├─";
				}
			}
			if(cur != nullptr)
			{
				stream << cur->to_string() << "\n";
			}
			for(std::size_t i = 0; i < cur->children().size(); i++)
			{
				const node& child = *cur->children()[cur->children().size() - 1 - i];
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
		std::puts(stream.str().c_str());
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

	node make_node(const lex::token& t)
	{
		node ret;
		switch(t.t)
		{
			case lex::type::identifier:
				ret.payload = identifier(t.lexeme);
			break;
			case lex::type::integer_literal:
				ret.payload = integer_literal(std::stoi(t.lexeme));
			break;
			case lex::type::decimal_literal:
				ret.payload = decimal_literal(std::stod(t.lexeme));
			break;
			case lex::type::null_literal:
				ret.payload = null_literal();
			break;
			case lex::type::char_literal:
			{
				std::string charlit = t.lexeme;
				diag::assert_that(!charlit.empty(), error_code::lex, "empty char-literal detected at {}. char literals must contain a single character.", t.meta_srcloc.to_string());
				charlit = escape(charlit);
				diag::assert_that(charlit.size() == 1, error_code::lex, "char-literal must consist of 1 character, but \'{}\' contains {}", t.lexeme, charlit.size());
				ret.payload = char_literal(charlit.front());
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
				ret.payload = bool_literal(val);
			}
			break;
			case lex::type::string_literal:
			{
				std::string stringlit = t.lexeme;
				ret.payload = string_literal(escape(stringlit));
			}
			break;
			default:
				ret.payload = unparsed_token{.tok = t};
			break;
		}
		if(ret.has_value())
		{
			ret.loc() = t.meta_srcloc;
		}
		return ret;
	}
}