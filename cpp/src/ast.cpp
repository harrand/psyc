#include "ast.hpp"
#include <stack>

std::optional<ast::node> ast::try_find_variable_from(path_t p, std::string_view variable_name) const
{
	auto is_variable_decl = [](const ast::node& n, std::string_view variable_name) -> bool
	{
		// first obvious choice: the node is a variable declaration of the name matching
		if(std::holds_alternative<ast::variable_declaration>(n.payload) && std::get<ast::variable_declaration>(n.payload).var_name == variable_name)
		{
			return true;
		}
		// second less-obvious choice: the node is a function-definition whose params have defined the function instead.
		if(std::holds_alternative<ast::function_definition>(n.payload))
		{
			const auto& func = std::get<ast::function_definition>(n.payload);
			for(const auto& param : func.params)
			{
				if(param.var_name == variable_name)
				{
					return true;
				}
			}
		}
		return false;
	};

	if(p.empty())
	{
		return std::nullopt;
	}
	std::size_t child_idx = p.back();
	p.pop_back();
	const auto& parent = this->get(p);
	if(is_variable_decl(parent, variable_name))
	{
		return parent;
	}
	for(std::size_t i = 0; i < child_idx; i++)
	{
		auto child = parent.children[i];
		if(is_variable_decl(child, variable_name))
		{
			return child;
		}
	}
	return this->try_find_variable_from(p, variable_name);
}

std::optional<ast::node> ast::try_find_variable(std::string_view variable_name) const
{
	return this->try_find_variable_from(this->current_path(), variable_name);
}

// which node are we on again?
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

// how do i navigate from the root node to the current node im looking at in the tree?
ast::path_t ast::current_path() const
{
	return this->path;
}

// add a new node as a child to whatever the current node is.
void ast::push(ast::node n)
{
	auto& cur = this->current();
	auto id = cur.children.size();
	cur.children.push_back(n);
	this->path.push_back(id);
}

// i have a path, if i follow it which node do i end up at?
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

std::optional<ast::path_t> ast::try_get_next(path_view_t path) const
{
	if(path.empty())
	{
		return std::nullopt;
	}
	path_t parent_path{path.begin(), path.end() - 1};
	std::size_t our_id = path.back();
	const node& parent = this->get(parent_path);
	std::size_t next_id = our_id + 1;
	if(parent.children.size() <= next_id)
	{
		// no
		return std::nullopt;
	}
	parent_path.push_back(next_id);
	return parent_path;
}

// i promise im not at the root node. i want to navigate to the parent of my current node. (i.e jump up a level)
void ast::pop()
{
	diag::assert_that(this->path.size(), "internal compiler error: popped too many times while parsing.");
	this->path.pop_back();
}

// print out the entire program AST. i hope you're a compiler developer or you aint finna like reading this.
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