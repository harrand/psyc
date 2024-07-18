#include "lex.hpp"
#include "diag.hpp"
#include <cstddef>
#include <limits>
#include <filesystem>
#include <string_view>
#include <fstream>

namespace lex
{
	constexpr std::size_t npos = std::numeric_limits<std::size_t>::max();

	struct internal_state
	{
		std::filesystem::path filename;
		std::string_view source;

		unsigned int line = 1;
		unsigned int col = 1;
		std::size_t cursor = 0;

		void advance(std::size_t count = 1)
		{
			this->col += count;
			this->cursor += count;
		}

		void advance(const char* cstr)
		{
			this->advance(std::strlen(cstr));
		}

		bool finished() const
		{
			return this->cursor >= source.size();
		}

		template<typename Pred>
		std::size_t advance_until(Pred p)
		{
			auto start = this->cursor;
			auto cur = start;
			std::string_view str;
			do
			{
				str = this->source.substr(cur++);
			}while(str.size() && !p(str));
			std::size_t dst = cur - start - 1;
			this->advance(dst);
			return dst;
		}

		template<typename... Ts>
		void error(std::format_string<Ts...> fmt, Ts&&... ts)
		{
			srcloc curloc{.file = this->filename, .line = this->line, .column = this->col};
			std::string msg = std::format(fmt, std::forward<Ts>(ts)...);

			diag::error(error_code::lex, "at {}, {}", curloc.to_string(), msg);
		}
	};

	token tokenise_once(internal_state& state, std::string_view data);

	output tokenise(std::filesystem::path psy_file)
	{
		tokens_list tokens = {};
		tokens.push_back(token{.t = type::source_begin, .lexeme = "begin token", .meta_srcloc = {.file = psy_file, .line = 0, .column = 0}});
		internal_state state;

		std::ifstream fstr(psy_file);
		diag::assert_that(fstr.good(), error_code::badargs, "could not open input file \"{}\"", psy_file.string());
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
			token tok = tokenise_once(state, data);
			if(tok.t == type::_undefined && state.cursor < state.source.size())
			{
				state.error("ill-defined token(s) detected");
			}

			tok.meta_srcloc = srcloc
			{
				.file = psy_file,
				.line = state.line,
				.column = state.col
			};

			if(tok.t != type::_undefined)
			{
				tokens.push_back(tok);
			}
		}
		return
		{
			.tokens = tokens,
			.psy_source = str
		};
	}

	bool word_should_break(std::string_view str);

	token tokenise_once(internal_state& state, std::string_view data)
	{
		while(data.starts_with("\n") || std::isspace(data.front()))
		{
			if(data.starts_with("\n"))
			{
				state.col = 0;
				state.line++;
			}
			state.cursor++;
			data = data.substr(1);
			if(data.empty())
			{
				return token{.t = type::_undefined};
			}
		}
		for(int i = 0; i < static_cast<int>(type::_count); i++)
		{
			auto t = static_cast<type>(i);
			std::string_view name = get_trait(t).name;
			if(!name.empty())
			{
				// just check for "name" at the start
				if(data.starts_with(name))
				{
					// advance the state. for nearly everything, advance by the lexeme size (and the lexeme is equal to the lex type name). for some things (e.g line comment) we have a different behaviour.
					std::string lexeme{name};
					switch(t)
					{
						case type::comment:
						[[fallthrough]];
						case type::doc_comment:
						{
							// advance till end of line
							std::size_t dst = state.advance_until([](std::string_view str)->bool
							{
								return str.starts_with("\n");
							});
							lexeme = data.substr(0, dst);
						}
						break;
						case type::mlcomment:
						[[fallthrough]];
						case type::mldoc_comment:
						{
							// advance till "*/" is found
							std::size_t dst = state.advance_until([](std::string_view str)->bool
							{
								return str.starts_with("*/");
							});
							if(state.finished())
							{
								state.error("multi-line comment is never terminated by */");
							}
							lexeme = data.substr(0, dst);
						}
						break;
						default:
							state.advance(name.size());
						break;
					}
					return token{t, lexeme};
				}
			}
			if(name.empty())
			{
				// its not a trivial check. need to check specifics.
				if(data.starts_with("true"))
				{
					state.advance("true");
					return token{.t = type::bool_literal, .lexeme = "true"};
				}
				else if(data.starts_with("false"))
				{
					state.advance("false");
					return token{.t = type::bool_literal, .lexeme = "false"};
				}
				else if(data.starts_with("null"))
				{
					state.advance("null");
					return token{.t = type::null_literal, .lexeme = "null"};
				}
				else if(data.starts_with("'"))
				{
					state.advance();
					std::size_t dst = state.advance_until([](std::string_view sv){return sv.starts_with("'");});
					state.advance();
					return token{.t = type::char_literal, .lexeme = std::string{data.substr(1, dst)}};
				}
				else if(data.starts_with("\""))
				{
					state.advance();
					std::size_t dst = state.advance_until([](std::string_view sv){return sv.starts_with("\"");});
					state.advance();
					return token{.t = type::string_literal, .lexeme = std::string{data.substr(1, dst)}};
				}
				else if(std::isdigit(data.front()))
				{
					// if it starts with a digit, it's an integer/decimal literal.
					std::size_t counter = 0;
					std::size_t dot_count = 0;
					std::string_view next;
					do
					{
						next = data.substr(++counter);
						if(next.starts_with("."))
						{
							dot_count++;
						}
					} while (next.front() == '.' || !word_should_break(next));
					std::string_view lexeme = data.substr(0, counter);
					state.advance(counter);
					if(dot_count > 1)
					{
						state.error("malformed decimal literal  \"{}\" contained more than one \".\" character", lexeme);
					}
					type t = dot_count == 0 ? type::integer_literal : type::decimal_literal;
					return token{t, std::string{lexeme}};
				}
				else
				{
					// identifier.
					std::size_t dst = state.advance_until(word_should_break);
					if(dst == 0)
					{
						srcloc curloc{.file = state.filename, .line = state.line, .column = state.col};
						std::size_t after = state.cursor + data.find_first_of('\n');
						std::size_t before = after;
						for(std::size_t i = after - 1; i > 0; i--)
						{
							before = i + 1;
							if(state.source[i] == '\n')
							{
								break;
							}
						}
						std::string_view line = state.source.substr(before, after - before);
						std::string bottom_text = "└─";
						std::string mid_text = "│ ";
						for(std::size_t i = 0; i < state.cursor - before; i++)
						{
							bottom_text += "─";
							mid_text += "═";
						}
						mid_text += "│";
						for(std::size_t i = 0; i < after - state.cursor; i++)
						{
							mid_text += "═";
						}
						bottom_text += "┘";
						diag::error(error_code::lex, "illegal token \"{}\" (ascii {}) detected:\n┌──[{}]\n│\n│ {}\n{}\n{}", data.front(), static_cast<int>(data.front()), curloc.to_string(), line, mid_text, bottom_text);
					}
					return token{.t = type::identifier, .lexeme =std::string{data.substr(0, dst)}};
				}
			}
		}
		return token{.t = type::_undefined};
	}

	bool word_should_break(std::string_view str)
	{
		if(str.starts_with(" const") || str.starts_with(" weak") || str.starts_with(" static"))
		{
			// don't break on a space if its immediately followed by const.
			return false;
		}
		return (!std::isalnum(str.front())
				&& str.front() != '_'
				&& str.front() != '&')
			|| std::isspace(str.front())
		;
	}
}