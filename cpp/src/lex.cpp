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
		std::size_t current_integer_literal_begin = npos;
		std::size_t current_decimal_literal_begin = npos;

		void advance(std::size_t amt = 1)
		{
			this->col += amt;
			this->cursor += amt;
		}

		bool in_word() const
		{
			return this->current_word_begin != npos;
		}

		bool in_integer_literal() const
		{
			return this->current_integer_literal_begin != npos;
		}

		bool in_decimal_literal() const
		{
			return this->current_decimal_literal_begin != npos;
		}

		bool in_anything() const
		{
			return this->in_word() || this->in_integer_literal() || this->in_decimal_literal();
		}

		std::string pop_word(type& t)
		{
			std::string ret;
			if(this->in_integer_literal())
			{
				ret = std::string{source.substr(this->current_integer_literal_begin, cursor - this->current_integer_literal_begin)};
				this->current_integer_literal_begin = npos;
				t = type::integer_literal;
			}
			else if(this->in_decimal_literal())
			{
				ret = std::string{source.substr(this->current_decimal_literal_begin, cursor - this->current_decimal_literal_begin)};
				if(ret.back() == '.')
				{
					this->error_generic(ret, "malformed decimal literal \"{}{}\" - no fractional part detected.");
				}
				this->current_decimal_literal_begin = npos;
				t = type::decimal_literal;
			}
			else if(this->in_word())
			{
				ret = std::string{source.substr(this->current_word_begin, cursor - this->current_word_begin)};
				this->current_word_begin = npos;
				t = type::identifier;
			}
			else
			{
				error_generic(this->source.data() + this->cursor, "call to `pop_word`");
			}
			return ret;
		}

		void error_generic(std::string_view dodgy_part, std::string msg, error_code errcode = error_code::lex)
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
			std::string_view dodgy_snippet = dodgy_part.substr(0, std::min(dodgy_part.size(), static_cast<std::size_t>(2u)));
			diag::error(errcode, "at {}, {}", curloc.to_string(), std::vformat(msg, std::make_format_args(dodgy_snippet, snippet)));
		}

		void unrecognised_tokens(std::string_view dodgy_part)
		{
			this->error_generic(dodgy_part, "unrecognised token(s) \"{}\" within: \"...{}...\"");
		}

		void invalid_numeric_literal(std::string_view dodgy_part)
		{
			this->error_generic(dodgy_part, "unexpected non-digit character(s) within numeric literal: \"{}\" within: \"...{}...\"");
		}
	};

	std::optional<token> tokenise_once(tokenise_state& state, std::string_view data);
	bool breaks_word(const tokenise_state& state, std::string_view str);

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
			if(state.in_anything() && breaks_word(state, data))
			{
				type t;
				std::string word = state.pop_word(t);
				tokens.push_back
				({
					.t = t,
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
				if(maybe_token->lexeme.empty())
				{
					maybe_token->lexeme = data.front();
				}
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
			return token{.t = type::semicolon};
		}
		else if(data.starts_with(":="))
		{
			state.advance();
			return token{.t = type::initialiser, .lexeme = ":="};
		}
		else if(data.starts_with(":"))
		{
			return token{.t = type::colon};
		}
		else if(data.starts_with("..."))
		{
			state.advance(2);
			return token{.t = type::ellipsis, .lexeme = "..."};
		}
		else if(data.starts_with("."))
		{
			if(state.in_integer_literal())
			{
				// if we're in an integer literal, we transform it into a decimal literal.
				state.current_decimal_literal_begin = state.current_integer_literal_begin;
				state.current_integer_literal_begin = npos;
			}
			else
			{
				return token{.t = type::dot};
			}
		}
		else if(data.starts_with(","))
		{
			return token{.t = type::comma};
		}
		else if(data.starts_with("->"))
		{
			state.advance();
			return token
			{
				.t = type::arrow_forward,
				.lexeme = "->"
			};
		}
		else if(data.starts_with("<-"))
		{
			state.advance();
			return token
			{
				.t = type::arrow_backward,
				.lexeme = "<-"
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
		else if(data.starts_with("struct"))
		{
			state.advance(5);
			return token{.t = type::keyword_struct, .lexeme = "struct"};
		}
		else if(data.starts_with("if"))
		{
			state.advance();
			return token{.t = type::keyword_if, .lexeme = "if"};
		}
		else if(data.starts_with("else"))
		{
			state.advance(3);
			return token{.t = type::keyword_else, .lexeme = "else"};
		}
		else if(data.starts_with("for"))
		{
			state.advance(2);
			return token{.t = type::keyword_for, .lexeme = "for"};
		}
		else if(data.starts_with("true"))
		{
			state.advance(3);
			return token{.t = type::bool_literal, .lexeme = "true"};
		}
		else if(data.starts_with("false"))
		{
			state.advance(4);
			return token{.t = type::bool_literal, .lexeme = "false"};
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
			return token{.t = type::operator_double_equals, .lexeme = "=="};
		}
		else if(data.starts_with("!="))
		{
			state.advance();
			return token{.t = type::operator_notequals, .lexeme = "!="};
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
		else if(data.starts_with("deref"))
		{
			state.advance(4);
			return token{.t = type::operator_deref, .lexeme = "deref"};
		}
		else if(data.starts_with("ref"))
		{
			state.advance(2);
			return token{.t = type::operator_ref, .lexeme = "ref"};
		}
		else if(data.starts_with("defer"))
		{
			state.advance(4);
			return token{.t = type::operator_defer, .lexeme = "defer"};
		}
		else if(data.starts_with("@"))
		{
			return token{.t = type::operator_cast};
		}
		else if(data.starts_with("?"))
		{
			return token{.t = type::question_mark};
		}
		else if(data.starts_with("return"))
		{
			state.advance(5);
			return token{.t = type::return_statement, .lexeme = "return"};
		}
		else if(data.starts_with("$"))
		{
			return token{.t = type::dollar_sign};
		}
		else if(
				// substrings that aren't syntax errors but don't form any tokens.
				data.starts_with(" ") ||
				data.starts_with("\n") ||
				data.starts_with("\t")
			)
		{}
		else if(!breaks_word(state, data))
		{
			if(!state.in_anything())
			{
				// we're typing something new.
				// if the new char is a number, its a numeric literal of some kind.
				if(std::isdigit(data.front()))
				{
					state.current_integer_literal_begin = state.cursor;
				}
				else // probably the start of a word (identifier/keyword)
				{
					state.current_word_begin = state.cursor;
				}
			}
			else
			{
				if((state.in_integer_literal() || state.in_decimal_literal()) && !std::isdigit(data.front()))
				{
					// you typed something thats not a digit
					state.invalid_numeric_literal(data);
				}
			}
		}
		else
		{
			state.unrecognised_tokens(data);
		}
		return std::nullopt;
	}

	bool breaks_word(const tokenise_state& state, std::string_view str)
	{
		if(str.starts_with(" const") || str.starts_with(" weak"))
		{
			// don't break on a space if its immediately followed by const.
			return false;
		}
		if(state.in_integer_literal() && str.starts_with("."))
		{
			return false;
			// a dot doesn't break an integer literal.
		}
		return (!std::isalnum(str.front())
				&& str.front() != '_'
				&& str.front() != '&')
			|| std::isspace(str.front())
		;
	}
}