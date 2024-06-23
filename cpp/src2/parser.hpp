#ifndef PSYC_PARSER_HPP
#define PSYC_PARSER_HPP
#include "lex.hpp"
#include "parse.hpp"
#include "ast.hpp"

namespace parse
{
	class parser
	{
	public:
		parser(lex::const_token_view tokens);

		void step();

		bool shift();
		void reduce();
		subtree_state get_parsed_state() const;
	private:
		lex::const_token_view tokens;
		lex::const_token_view unscanned_tokens;
		std::vector<syntax::node_ptr> subtrees = {};
	};
}

#endif // PSYC_PARSER_HPP