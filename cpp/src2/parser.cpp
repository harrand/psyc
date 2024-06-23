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

	void parser::parse()
	{
		while(this->step()){}
	}

	bool parser::step()
	{
		auto state = this->get_parsed_state();
		if(state.empty())
		{
			// we're done.
			diag::error(error_code::nyi, "done!");
		}
		reduction reduction = find_reduction(state);
		// reduce_fn == null means no reductions available. we must shift.
		if(reduction.is_null())
		{
			return shift();
		}
		else
		{
			reduction.reduce_fn(this->make_reducer());
			return true;
		}
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

	reducer parser::make_reducer()
	{
		return {.subtrees = this->subtrees};
	}

	syntax::node_ptr parser::get_output()
	{
		diag::assert_that(this->subtrees.size() == 1, error_code::parse, "did not parse to a single AST. {} subtrees remaining", this->subtrees.size());
		return std::move(this->subtrees.front());
	}

	syntax::node_ptr tokens(lex::const_token_view toks)
	{
		parser state{toks};
		state.parse();
		return state.get_output();
	}
}