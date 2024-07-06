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
		subtree_state get_parsed_state(std::size_t offset) const;

		reducer make_reducer(std::size_t offset);
		syntax::node_ptr get_output();
	private:
		lex::tokens_list tokens;
		lex::const_token_view unscanned_tokens;
		std::string source;
		std::vector<syntax::node_ptr> subtrees = {};
		syntax::node_ptr output = nullptr;
	};

	syntax::node_ptr tokens(lex::output tokens);

	struct state
	{
		std::unordered_map<std::filesystem::path, syntax::node_ptr> parsed_input_files = {};
	};
}

#endif // PSYC_PARSER_HPP