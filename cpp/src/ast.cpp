#include "ast.hpp"
#include "diag.hpp"
#include <stack>

const ast::node& ast::get(ast::path_const_view_t path) const
{
	const ast::node* nodeptr = &this->root;
	for(std::size_t i : path)
	{
		diag::assert_that(nodeptr != nullptr, error_code::ice, "attempt to retrieve child {} of a null node", i);
		diag::assert_that(nodeptr->children.size() > i, error_code::ice, "attempt to retrieve child {} of a node with only {} children", i, nodeptr->children.size());
		nodeptr = &nodeptr->children[i];
	}
	return *nodeptr;
}

ast::node& ast::get(ast::path_const_view_t path)
{
	return const_cast<ast::node&>(const_cast<const ast*>(this)->get(path));
}

std::optional<ast::path_t> ast::try_get_previous(ast::path_t path) const
{
	if(path.empty())
	{
		return std::nullopt;
	}
	std::size_t back = path.back();
	if(back == 0)
	{
		return std::nullopt;
	}
	path.pop_back();
	const node& parent = this->get(path);
	if(parent.children.size() > (back - 1))
	{
		path.push_back(back - 1);
		return path;
	}
	return std::nullopt;
}

std::optional<ast::path_t> ast::try_get_next(ast::path_t path) const
{
	if(path.empty())
	{
		return std::nullopt;
	}
	std::size_t back = path.back();
	path.pop_back();
	const node& parent = this->get(path);
	if(parent.children.size() > (back + 1))
	{
		path.push_back(back + 1);
		return path;
	}
	return std::nullopt;
}

const ast::node& ast::current() const
{
	return this->get(this->current_path);
}

ast::node& ast::current()
{
	return this->get(this->current_path);
}

void ast::attach_to(ast& parent, const path_t& path) const
{
	diag::assert_that(!std::holds_alternative<std::monostate>(this->root.payload), error_code::ice, "attempt to attach AST subtree to a parent tree, but the subtree's root node has no payload. this means it is not a valid subtree. the root node should be a smaller (child) node of the parent you're trying to attach it to (hence bottom-up)");
	path_t parent_path = path;
	std::size_t back = path.back();
	parent_path.pop_back();

	ast::node& parent_node = parent.get(parent_path);
	diag::assert_that(parent_node.children.size() > back, error_code::ice, "attempt to attach AST subtree to a parent tree at a dodgy path.");
	diag::assert_that(parent_node.children[back] == ast::node{}, error_code::ice, "while attaching an AST subtree to a parent tree, noticed that the node at the provided path was not empty - meaning you're going to overwrite something. when calling attach_to, the child at the path should exist already as an empty node");

	parent_node.children[back] = this->root;
}

void ast::pretty_print() const
{
	std::stack<const ast::node*> node_list;
	std::stack<std::size_t> indents;
	node_list.push(&this->root);
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