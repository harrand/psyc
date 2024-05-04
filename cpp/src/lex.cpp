#include "lex.hpp"
#include "diag.hpp"
#include <fstream>

namespace lex
{
	constexpr std::size_t npos = std::numeric_limits<std::size_t>::max();
	struct tokenise_state
	{
		unsigned int line = 1;
		unsigned int col = 1;
		std::size_t cursor = 0;
		std::size_t current_word_begin = npos;
	};

	std::optional<token> tokenise_once(tokenise_state& state, std::string_view data);

	output tokenise(std::filesystem::path psy_file)
	{
		tokens_list tokens = {};
		tokenise_state state;

		std::ifstream fstr(psy_file);
		// slurp the whole file into a string.
		std::stringstream buffer;
		buffer << fstr.rdbuf();
		std::string str = buffer.str();
		if(str.empty())
		{
			diag::warning("empty file: {}", psy_file.filename().string());
		}

		while(state.cursor < str.size())
		{
			std::string_view data = str.data() + state.cursor;
			const srcloc curloc
			{
				.file = psy_file,
				.line = state.line,
				.column = state.col
			};

			// try to match the current token(s)
			if(data.starts_with("\n"))
			{
				state.col = 0;	
				state.line++;
			}
			else
			{
				auto maybe_token = tokenise_once(state, data);
				if(maybe_token.has_value())
				{
					maybe_token->meta_srcloc = curloc;
					tokens.push_back(maybe_token.value());
				}
			}
			state.col++;
			state.cursor++;
		}
		return
		{
			.tokens = tokens,
			.psy_source = str
		};
	}

	std::optional<token> tokenise_once(tokenise_state& state, std::string_view data)
	{
		return std::nullopt;
	}
}