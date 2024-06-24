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

		void parse();
		bool step();

		bool shift();
		subtree_state get_parsed_state(std::size_t offset) const;

		reducer make_reducer(std::size_t offset);
		syntax::node_ptr get_output();
	private:
		lex::const_token_view tokens;
		lex::const_token_view unscanned_tokens;
		std::optional<lex::token> lookahead = std::nullopt;
		std::vector<syntax::node_ptr> subtrees = {};
		syntax::node_ptr output = nullptr;
	};

	syntax::node_ptr tokens(lex::const_token_view toks);

	struct state
	{
		std::unordered_map<std::filesystem::path, syntax::node_ptr> parsed_input_files = {};
	};
}

#endif // PSYC_PARSER_HPP