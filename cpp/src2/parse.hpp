#ifndef PSYC_PARSE_HPP
#define PSYC_PARSE_HPP
#include "ast.hpp"
#include <vector>
#include <cstddef>
#include <span>

namespace parse
{
	using subtree_view = std::span<const syntax::node_ptr>;
	using reduce_function_t = bool(*)(subtree_view);

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
	void add_new_reduction(subtree_state_view hashes, reduce_function_t reduce_fn);
}

#endif // PSYC_PARSE_HPP