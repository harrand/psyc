#ifndef PSYC_PARSER_HPP
#define PSYC_PARSER_HPP
#include "lex.hpp"

namespace parse
{
	class parser
	{
	public:
		parser(lex::const_token_view tokens);

		void step();

		bool shift();
		void reduce();
	private:
		lex::const_token_view tokens;
		lex::const_token_view unscanned_tokens;
		//std::vector<ast> subtrees = {};
	};
}

#endif // PSYC_PARSER_HPP