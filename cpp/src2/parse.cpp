#include "parse.hpp"
#include "diag.hpp"
#include "ast.hpp"
#include "profile.hpp"
#include <unordered_map>
#include <tuple>

#define HASH(x) syntax::node::x{}.hash()
#define TOKEN_HASH(toktype) syntax::node::unparsed_token{{.t = lex::type::toktype}}.hash()

namespace parse
{
	std::span<const syntax::nodenew> reducer::view() const
	{
		return std::span<const syntax::nodenew>{this->subtrees}.subspan(this->idx);
	}

	bool reducer::no_prefix() const
	{
		return this->idx == 0;
	}

	struct parse_entry
	{
		reduce_function_t reduce_fn = nullptr;
		std::unordered_map<std::size_t, parse_entry> children = {};
	};
	std::unordered_map<std::size_t, parse_entry> parse_table;

	reduction find_reduction_impl(subtree_view state, parse_entry* prev, std::optional<reduction> last_good = std::nullopt)
	{
		diag::assert_that(prev != nullptr, error_code::ice, "previous parse entry was nullptr.");

		// we reached the end, return the current resolve function.
		if(state.empty())
		{
			return {.reduce_fn = prev->reduce_fn, .offset = 0};
		}
		auto iter = prev->children.find(state.front().hash());
		if(iter == prev->children.end())
		{
			if(last_good.has_value())
			{
				return last_good.value();
			}
			return reduction::null();
		}
		std::optional<reduction> this_good = std::nullopt;
		if(iter->second.reduce_fn != nullptr)
		{
			this_good = {.reduce_fn = iter->second.reduce_fn, .offset = 0};
		}
		return find_reduction_impl(state.subspan(1), &iter->second, this_good);
	}

	reduction find_reduction(subtree_view state)
	{
		PROFZONE("find reduction");
		diag::assert_that(state.size(), error_code::ice, "requested to find reduction but state is empty.");
		subtree_index front{.idx = state.front().hash(), .name_hint = state.front().name()};
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

	#include "parse_macros.hpp"

	void populate_parse_table()
	{
		PROFZONE("parse table populate");
		#include "parse_chords/parse_token.inl"
		#include "parse_chords/parse_literals.inl"
		#include "parse_chords/parse_identifier.inl"
		#include "parse_chords/parse_function_call.inl"
		#include "parse_chords/parse_expression.inl"
		#include "parse_chords/parse_expression_list.inl"
		#include "parse_chords/parse_function_decl.inl"
		#include "parse_chords/parse_variable_decl.inl"
		#include "parse_chords/parse_variable_decl_list.inl"
		#include "parse_chords/parse_designated_initialiser.inl"
		#include "parse_chords/parse_designated_initialiser_list.inl"
		#include "parse_chords/parse_meta_region.inl"
		#include "parse_chords/parse_namespace_access.inl"
		#include "parse_chords/parse_unfinished_block.inl"
		#include "parse_chords/parse_if_statement.inl"
	}
}