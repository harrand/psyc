#include "parser.hpp"
#include "parse.hpp"
#include "diag.hpp"

namespace parse
{
	parser::parser(lex::const_token_view tokens):
	tokens(tokens),
	unscanned_tokens(tokens)
	{

	}

	void parser::step()
	{
		auto state = this->get_parsed_state();
		if(state.empty())
		{
			// we're done.
			diag::error(error_code::nyi, "done!");
		}
		reduction reduction = find_reduction(state);
		if(reduction.is_null())
		{
			shift();
		}
		else
		{
			reduction.reduce_fn(this->subtrees);
		}
		// reduce_fn == null means no reductions available. we must shift.
	}

	bool parser::shift()
	{
		if(this->unscanned_tokens.empty())
		{
			return false;
		}
		lex::token t = this->unscanned_tokens.front();
		this->unscanned_tokens = this->unscanned_tokens.subspan(1);
		if(lex::get_trait(t.t).parse_skip)
		{
			return shift();
		}

		this->subtrees.push_back(syntax::make_node(t));
		return true;
	}

	void parser::reduce()
	{
		// using each subtree
	}

	subtree_state parser::get_parsed_state() const
	{
		subtree_state ret;
		ret.reserve(this->subtrees.size());
		for(const auto& subtree : this->subtrees)
		{
			ret.push_back(subtree_index{.idx = subtree->hash(), .name_hint = subtree->name()});
		}
		return ret;
	}
}