#include "parser.hpp"

namespace parse
{
	parser::parser(lex::const_token_view tokens):
	tokens(tokens),
	unscanned_tokens(tokens)
	{

	}

	void parser::step()
	{

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

		// we have a token t
		// append it as a new subtree.
		//this->subtrees.push_back(make_subtree(t));
	}

	void parser::reduce()
	{

	}
}