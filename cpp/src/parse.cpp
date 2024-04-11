#include "parse.hpp"
#include "diag.hpp"
#include "lex.hpp"
#include <stack>
#include <string>
#include <unordered_map>
#include <format>
#include <optional>

namespace parser
{
	// ast implementation begin
	const ast::node& ast::current() const
	{
		const node* cur = &this->program;
		for(std::size_t id : this->path)
		{
			cur = &cur->children[id];
		}
		return *cur;
	}

	ast::node& ast::current()
	{
		return this->get(this->path);
	}

	ast::path_t ast::current_path() const
	{
		return this->path;
	}

	void ast::push(ast::node n)
	{
		auto& cur = this->current();
		auto id = cur.children.size();
		cur.children.push_back(n);
		this->path.push_back(id);
	}

	const ast::node& ast::get(std::span<const std::size_t> path) const
	{
		const node* n = &this->program;
		for(std::size_t i : path)
		{
			n = &n->children[i];
		}
		return *n;
	}

	ast::node& ast::get(std::span<const std::size_t> path)
	{
		node* n = &this->program;
		for(std::size_t i : path)
		{
			n = &n->children[i];
		}
		return *n;
	}

	void ast::pop()
	{
		diag::assert_that(this->path.size(), "internal compiler error: popped too many times while parsing.");
		this->path.pop_back();
	}

	void ast::pretty_print()
	{
		std::stack<const ast::node*> node_list;
		std::stack<std::size_t> indents;
		node_list.push(&this->program);
		indents.push(0);
		std::cout << "program:\n";
		while(node_list.size())
		{
			const ast::node* cur = node_list.top();
			node_list.pop();
			std::size_t indent = indents.top();
			indents.pop();
			for(std::size_t i = 0; i < indent; i++)
			{
				std::cout << "  ";
			}
			std::visit([](auto&& arg)
			{
				using T = std::decay_t<decltype(arg)>;
				if constexpr(!std::is_same_v<T, std::monostate>)
				{
					std::cout << arg.to_string() << "\n";
				}
			}, cur->payload);
			for(std::size_t i = 0; i < cur->children.size(); i++)
			{
				const auto& child = cur->children[cur->children.size() - 1 - i];
				node_list.push(&child);
				indents.push(indent + 1);
			}
		}
	}

	// ast implementation end
	struct parser_impl
	{
		void parse()
		{

		}

		ast get_ast() const
		{
			return this->tree;
		}

		lexer::const_token_view tokens;
		ast tree = {};
	};

	ast parse(lexer::const_token_view tokens)
	{
		parser_impl parser{.tokens = tokens};
		parser.parse();
		return parser.get_ast();
	}
}