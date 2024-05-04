#include "lex.hpp"
#include "diag.hpp"
#include <fstream>

namespace lex
{
	constexpr std::size_t npos = std::numeric_limits<std::size_t>::max();
	struct tokenise_state
	{
		std::filesystem::path filename;
		std::string_view source;
		unsigned int line = 1;
		unsigned int col = 1;
		std::size_t cursor = 0;
		std::size_t current_word_begin = npos;

		void advance(std::size_t amt = 1)
		{
			this->col += amt;
			this->cursor += amt;
		}

		bool in_word() const
		{
			return this->current_word_begin != npos;
		}

		std::string pop_word()
		{
			diag::assert_that(this->in_word(), error_code::ice, "lexer has performed a bad call to pop_word");
			std::string ret{source.substr(this->current_word_begin, cursor - this->current_word_begin)};
			this->current_word_begin = npos;
			return ret;
		}

		void error_generic(std::string_view dodgy_part, std::string msg)
		{
			const srcloc curloc
			{
				.file = this->filename,
				.line = this->line,
				.column = this->col
			};
			constexpr std::size_t snippet_width = 4;
			std::size_t snippet_begin = this->cursor > snippet_width ? (this->cursor - snippet_width) : 0;
			std::size_t snippet_end = (this->cursor + snippet_width) >= this->source.size() ? this->source.size() : (this->cursor + snippet_width);
			std::string_view snippet = this->source.substr(snippet_begin, snippet_end - snippet_begin);
			diag::error(error_code::syntax, "at {}, {}", curloc.to_string(), std::vformat(msg, std::make_format_args(dodgy_part.substr(0, std::min(dodgy_part.size(), static_cast<std::size_t>(2u))), snippet)));
		}

		void unrecognised_tokens(std::string_view dodgy_part)
		{
			this->error_generic(dodgy_part, "unrecognised token(s) \"{}\" within: \"...{}...\"");
		}
	};

	std::optional<token> tokenise_once(tokenise_state& state, std::string_view data);
	bool breaks_word(std::string_view str);

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
		state.filename = psy_file;
		state.source = str;

		while(state.cursor < str.size())
		{
			std::string_view data = str.data() + state.cursor;
			const srcloc curloc
			{
				.file = psy_file,
				.line = state.line,
				.column = state.col
			};
			if(state.in_word() && breaks_word(data))
			{
				std::string word = state.pop_word();
				tokens.push_back
				({
					.t = type::identifier,
					.lexeme = word,
					.meta_srcloc = srcloc
					{
						.file = curloc.file,
						.line = curloc.line,
						.column = curloc.column - static_cast<unsigned int>(word.size())
					}
				});
			}
			auto maybe_token = tokenise_once(state, data);
			if(maybe_token.has_value())
			{
				maybe_token->meta_srcloc = curloc;
				tokens.push_back(maybe_token.value());
			}
			state.advance();
		}
		return
		{
			.tokens = tokens,
			.psy_source = str
		};
	}

	std::optional<token> tokenise_once(tokenise_state& state, std::string_view data)
	{
		if(data.starts_with("\n"))
		{
			state.col = 0;	
			state.line++;
		}
		if(data.starts_with(";"))
		{
			return token
			{
				.t = type::semicolon
			};
		}
		else if(data.starts_with(":"))
		{
			return token
			{
				.t = type::colon
			};
		}
		else if(data.starts_with("->"))
		{
			state.advance();
			return token
			{
				.t = type::arrow_forward
			};
		}
		else if(data.starts_with("<-"))
		{
			state.advance();
			return token
			{
				.t = type::arrow_backward
			};
		}
		else if(data.starts_with("//"))
		{
			std::size_t comment_begin = state.cursor;
			while(++state.cursor < (state.source.size() - 1) && state.source[state.cursor] != '\n')
			{}
			std::size_t comment_end = state.cursor;
			state.col = 0;	
			state.line++;
			return token
			{
				.t = type::line_comment,
				.lexeme = std::string(state.source.data() + comment_begin, comment_end - comment_begin)
			};
		}
		else if(data.starts_with("("))
		{
			return token{.t = type::open_paren};
		}
		else if(data.starts_with(")"))
		{
			return token{.t = type::close_paren};
		}
		else if(data.starts_with("{"))
		{
			return token{.t = type::open_brace};
		}
		else if(data.starts_with("}"))
		{
			return token{.t = type::close_brace};
		}
		else if(data.starts_with("["))
		{
			return token{.t = type::open_brack};
		}
		else if(data.starts_with("]"))
		{
			return token{.t = type::close_brack};
		}
		else if(data.starts_with("=="))
		{
			state.advance();
			return token{.t = type::operator_double_equals};
		}
		else if(data.starts_with("="))
		{
			return token{.t = type::operator_equals};
		}
		else if(data.starts_with("+"))
		{
			return token{.t = type::operator_plus};
		}
		else if(data.starts_with("-"))
		{
			return token{.t = type::operator_minus};
		}
		else if(data.starts_with("/"))
		{
			return token{.t = type::operator_slash};
		}
		else if(data.starts_with("*"))
		{
			return token{.t = type::operator_asterisk};
		}
		else if(
				// substrings that aren't syntax errors but don't form any tokens.
				data.starts_with(" ") ||
				data.starts_with("\n") ||
				data.starts_with("\t")
			)
		{}
		else if(!breaks_word(data))
		{
			if(!state.in_word())
			{
				state.current_word_begin = state.cursor;
			}
		}
		else
		{
			state.unrecognised_tokens(data);
		}
		return std::nullopt;
	}

	bool breaks_word(std::string_view str)
	{
		return !std::isalnum(str.front())
			|| std::isspace(str.front())
		;
	}
}