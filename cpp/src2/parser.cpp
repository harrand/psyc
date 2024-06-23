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
		constexpr std::size_t max_lookahead = 8;
		for(std::size_t i = 0; std::cmp_less(i, std::min(max_lookahead, this->subtrees.size())); i++)
		{
			auto state = this->get_parsed_state(i);
			if(!state.empty())
			{
				reduction reduc = find_reduction(state);
				if(!reduc.is_null())
				{
					reduc.reduce_fn(this->make_reducer(i));
					return true;
				}
			}
		}
		return shift();
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

	subtree_state parser::get_parsed_state(std::size_t lookahead) const
	{
		subtree_state ret;
		ret.reserve(this->subtrees.size() - lookahead);
		for(auto iter = this->subtrees.begin() + lookahead; iter != this->subtrees.end(); iter++)
		{
			ret.push_back(subtree_index{.idx = iter->get()->hash(), .name_hint = iter->get()->name()});
		}
		return ret;
	}

	reducer parser::make_reducer(std::size_t offset)
	{
		return {.subtrees = this->subtrees, .idx = offset};
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