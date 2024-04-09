#include "lex.hpp"
#include "diag.hpp"
#include <format>

namespace lexer
{
	tokens lex(std::string_view psy)
	{
		tokens ret = {};
		std::size_t cursor = 0;
		if(psy.empty())
		{
			return ret;
		}
		constexpr auto npos = std::numeric_limits<std::size_t>::max();
		std::size_t current_word_begin = npos;

		constexpr const char* keywords[] =
		{
			"return", // return from current function.
		};

		auto current_is_keyword = [](std::string_view str) constexpr -> bool
		{
			constexpr auto keyword_count = sizeof(keywords) / sizeof(const char*);
			for(std::size_t i = 0; i < keyword_count; i++)
			{
				if(str == keywords[i])
				{
					return true;
				}
			}
			return false;
		};

		auto emit_word = [&]()
		{
			if(current_word_begin != npos)
			{
				std::string value = std::string(psy.data() + current_word_begin, cursor - current_word_begin);
				ret.push_back({.id = current_is_keyword(value) ? token::type::keyword : token::type::identifier, .value = value});
				current_word_begin = npos;
			}
		};

		std::size_t line_counter = 1;

		while(cursor < psy.size())
		{
			std::string_view data = psy.substr(cursor);
			if(data.starts_with("\n"))
			{
				emit_word();
				ret.push_back({.id = token::type::newline});
				line_counter++;
			}
			else if(data.starts_with("\t"))
			{
				// ignore the following tokens completely.
			}
			else if(data.starts_with(";"))
			{
				emit_word();
				ret.push_back({.id = token::type::semicolon});
			}
			else if(data.starts_with(" "))
			{
				emit_word();
			}
			else if(data.starts_with("("))
			{
				emit_word();
				ret.push_back({.id = token::type::open_paren});
			}
			else if(data.starts_with(")"))
			{
				emit_word();
				ret.push_back({.id = token::type::close_paren});
			}
			else if(data.starts_with("{"))
			{
				emit_word();
				ret.push_back({.id = token::type::open_brace});
			}
			else if(data.starts_with("}"))
			{
				emit_word();
				ret.push_back({.id = token::type::close_brace});
			}
			else if(data.starts_with(":"))
			{
				emit_word();
				ret.push_back({.id = token::type::colon});
			}
			else if(data.starts_with("->"))
			{
				emit_word();
				ret.push_back({.id = token::type::arrow});
				// -> is 2 chars unlike the others. advance an additional time now.
				cursor++;
			}
			else
			{
				if(current_word_begin == npos)
				{
					current_word_begin = cursor;
				}
			}
			cursor++;
		}

		if(current_word_begin != npos)
		{
			diag::error(std::format("lexer: unterminated word by eof. line {}. unrecognised token(s): \"{}\"", line_counter, psy.substr(current_word_begin, cursor - current_word_begin)));
		}

		return ret;
	}
}
