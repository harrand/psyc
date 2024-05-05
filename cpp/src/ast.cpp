#include "ast.hpp"
#include "diag.hpp"

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