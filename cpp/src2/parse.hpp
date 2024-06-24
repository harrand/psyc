#ifndef PSYC_PARSE2_HPP
#define PSYC_PARSE2_HPP
#include "ast.hpp"
#include <vector>
#include <cstddef>
#include <span>

namespace parse
{
	using subtree_view = std::span<const syntax::node_ptr>;
	struct reducer
	{
		std::vector<syntax::node_ptr>& subtrees;
		std::size_t idx = 0;
	};
	struct result
	{
		enum class type
		{
			reduce_success,
			shift,
			shift_but_clear_lookahead,
			error
		} t;
		std::string errmsg = "";
	};
	using reduce_function_t = result(*)(reducer);

	struct subtree_index
	{
		std::size_t idx;
		const char* name_hint;
	};
	using subtree_state = std::vector<subtree_index>;
	using subtree_state_view = std::span<const subtree_index>;

	struct reduction
	{
		// the function that does the necessary reduction.
		reduce_function_t reduce_fn;
		// the offset into the subtree state view that the reductio n begins. currently, this is always 0.
		std::size_t offset = 0;

		bool is_null() const{return this->reduce_fn == nullptr;}
		static reduction null(){return {};}
	};

	reduction find_reduction(subtree_state_view state);
	template<std::size_t N>
	void add_new_reduction(std::array<subtree_index, N> hashes, reduce_function_t reduce_fn)
	{
		add_new_reduction_impl(hashes, reduce_fn);
	}

	void add_new_reduction_impl(subtree_state_view hashes, reduce_function_t reduce_fn);
	void populate_parse_table();

}

#endif // PSYC_PARSE2_HPP