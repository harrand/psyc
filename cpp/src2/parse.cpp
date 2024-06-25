#include "parse.hpp"
#include "diag.hpp"
#include "ast.hpp"
#include <unordered_map>
#include <tuple>

#define HASH(x) syntax::node::x{}.hash()
#define TOKEN_HASH(toktype) syntax::node::unparsed_token{{.t = lex::type::toktype}}.hash()

namespace parse
{
	std::span<const syntax::node_ptr> reducer::view() const
	{
		return std::span<const syntax::node_ptr>{this->subtrees}.subspan(this->idx);
	}

	struct parse_entry
	{
		reduce_function_t reduce_fn = nullptr;
		std::unordered_map<std::size_t, parse_entry> children = {};
	};
	std::unordered_map<std::size_t, parse_entry> parse_table;

	reduction find_reduction_impl(subtree_state_view state, parse_entry* prev)
	{
		diag::assert_that(prev != nullptr, error_code::ice, "previous parse entry was nullptr.");

		// we reached the end, return the current resolve function.
		if(state.empty())
		{
			return {.reduce_fn = prev->reduce_fn, .offset = 0};
		}
		auto iter = prev->children.find(state.front().idx);
		if(iter == prev->children.end())
		{
			return reduction::null();
		}
		return find_reduction_impl(state.subspan(1), &iter->second);
	}

	reduction find_reduction(subtree_state_view state)
	{
		diag::assert_that(state.size(), error_code::ice, "requested to find reduction but state is empty.");
		const subtree_index& front = state.front();
		auto iter = parse_table.find(front.idx);
		if(iter == parse_table.end())
		{
			return reduction::null();
		}

		return find_reduction_impl(state.subspan(1), &iter->second);
	}

	void add_new_reduction_impl(subtree_state_view hashes, reduce_function_t reduce_fn)
	{
		diag::assert_that(hashes.size(), error_code::nyi, "attempt to add reduction for the empty subtree state.");
		const subtree_index& front = hashes.front();
		parse_entry* entry = &parse_table[front.idx];
		for(const subtree_index& idx : hashes.subspan(1))
		{
			entry = &entry->children[idx.idx];
		}

		diag::assert_that(entry->reduce_fn == nullptr, error_code::ice, "duplicate definition of reduce function");
		entry->reduce_fn = reduce_fn;
	}

	#define INFUNC
	#include "parse_macros.hpp"

	void populate_parse_table()
	{
		#include "parse_token.cpp"
		#include "parse_identifier.cpp"
		#include "parse_primary_expression.cpp"
	}
}