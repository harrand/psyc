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
		parser(lex::output tokens);

		void parse();
		bool step();

		bool shift();
		subtree_view get_parsed_state(std::size_t offset) const;

		reducer make_reducer(std::size_t offset);
		syntax::nodenew get_output();
	private:
		lex::tokens_list tokens;
		lex::const_token_view unscanned_tokens;
		std::string source;
		std::vector<syntax::nodenew> subtrees = {};
		syntax::nodenew output = {};
		std::size_t total_reduction_count = 0;
		std::size_t silent_rejection_count = 0;
	};

	syntax::nodenew tokens(lex::output tokens);

	struct state
	{
		std::unordered_map<std::filesystem::path, syntax::nodenew> parsed_input_files = {};
	};
}

#endif // PSYC_PARSER_HPP