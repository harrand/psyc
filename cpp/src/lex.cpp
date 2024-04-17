#include "lex.hpp"
#include "diag.hpp"
#include <format>
#include <string>

namespace lexer
{
	std::string token::to_string() const
	{
		std::string type_name = {lexer::token_type_names[static_cast<std::size_t>(this->id)]};
		return type_name + this->value;
	}

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
		std::size_t current_string_literal_begin = npos;
		std::size_t current_char_literal_begin = npos;

		constexpr const char* keywords[] =
		{
			"return", // return from current function.
			"extern",
			"if", // if-statement
			"for", // for-statement
			"struct" // struct specifier
		};

		auto current_is_keyword = [&keywords](std::string_view str) constexpr -> bool
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
			if(current_char_literal_begin != npos || current_string_literal_begin != npos)
			{
				return;
			}
			if(current_word_begin != npos)
			{
				std::string value = std::string(psy.data() + current_word_begin, cursor - current_word_begin);
				token::type t = token::type::identifier;
				// if its a keyword, then its a keyword
				if(current_is_keyword(value))
				{
					t = token::type::keyword;
				}
				else
				{
					// check if its a bool literal (true/false)
					if(value == "true" || value == "false")
					{
						t = token::type::bool_literal;
					}
					else
					{
						// otherwise check if its a decimal literal (swallow exception and move on if not)
						try
						{
							double val = std::stod(value);
							// `5` also converts well to a double, so check if the string contains a `.`
							if(value.find('.') == std::string::npos)
							{
								t = token::type::integer_literal;
							}
							else
							{
								t = token::type::decimal_literal;
							}
						}catch(...){}
					}
				}
				// if we failed all of that, it keeps its original value as an identifier.
				ret.push_back({.id = t, .value = value});
				current_word_begin = npos;
			}
		};

		auto emit_string_literal = [&]()
		{
			if(current_string_literal_begin != npos)
			{
				std::string value = std::string(psy.data() + current_string_literal_begin + 1, cursor - current_string_literal_begin - 1);
				// if we failed all of that, it keeps its original value as an identifier.
				ret.push_back({.id = token::type::string_literal, .value = value});
				current_string_literal_begin = npos;
			}
		};

		auto emit_char_literal = [&]()
		{
			if(current_char_literal_begin != npos)
			{
				char value = psy.data()[current_char_literal_begin + 1];
				ret.push_back({.id = token::type::char_literal, .value = std::string{value}});
				current_char_literal_begin = npos;
			}
		};

		std::size_t line_counter = 1;

		while(cursor < psy.size())
		{
			std::string_view data = psy.substr(cursor);
			if(data.starts_with("'"))
			{
				if(current_char_literal_begin == npos)
				{
					current_char_literal_begin = cursor;
				}
				else
				{
					emit_char_literal();
					cursor++;
					continue;
				}
			}
			else if(current_char_literal_begin != npos)
			{
				cursor++;
				continue;
			}
			if(data.starts_with("\""))
			{
				if(current_string_literal_begin == npos)
				{
					current_string_literal_begin = cursor;
				}
				else
				{
					emit_string_literal();
					cursor++;
					continue;
				}
			}
			else if(current_string_literal_begin != npos)
			{
				cursor++;
				continue;
			}
			else if(data.starts_with("//"))
			{
				emit_word();
				if(current_string_literal_begin == npos)
				{
					std::size_t comment_begin = cursor;
					while(++cursor < (psy.size() - 1) && psy[cursor] != '\n'){}
					std::size_t comment_end = cursor;
					ret.push_back({.id = token::type::line_comment, .value = std::string{psy.data() + comment_begin, psy.data() + comment_end + 1}});
				}
			}
			else if(data.starts_with("\n"))
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
			else if(data.starts_with("["))
			{
				emit_word();
				ret.push_back({.id = token::type::open_brack});
			}
			else if(data.starts_with("]"))
			{
				emit_word();
				ret.push_back({.id = token::type::close_brack});
			}
			else if(data.starts_with(","))
			{
				emit_word();
				ret.push_back({.id = token::type::commar});
			}
			else if(data.starts_with("=="))
			{
				emit_word();
				ret.push_back({.id = token::type::double_equals});
				// == is 2 chars unlike the others. advance an additional time now.
				cursor++;
			}
			else if(data.starts_with("!="))
			{
				emit_word();
				ret.push_back({.id = token::type::not_equals});
				// == is 2 chars unlike the others. advance an additional time now.
				cursor++;
			}
			else if(data.starts_with("="))
			{
				emit_word();
				ret.push_back({.id = token::type::equals});
			}
			else if(data.starts_with(":"))
			{
				emit_word();
				ret.push_back({.id = token::type::colon});
			}
			else if(data.starts_with("..."))
			{
				emit_word();
				ret.push_back({.id = token::type::ellipsis});
				cursor += 2;
			}
			else if(data.starts_with("."))
			{
				emit_word();
				ret.push_back({.id = token::type::dot});
			}
			else if(data.starts_with("->"))
			{
				emit_word();
				ret.push_back({.id = token::type::arrow});
				// -> is 2 chars unlike the others. advance an additional time now.
				cursor++;
			}
			else if(data.starts_with("+"))
			{
				ret.push_back({.id = token::type::plus});
			}
			else if(data.starts_with("-"))
			{
				ret.push_back({.id = token::type::minus});
			}
			else if(data.starts_with("~"))
			{
				ret.push_back({.id = token::type::bitwise_complement});
			}
			else if(data.starts_with("!"))
			{
				ret.push_back({.id = token::type::logical_negation});
			}
			else
			{
				if(current_word_begin == npos && current_char_literal_begin == npos && current_string_literal_begin == npos)
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

		if(current_string_literal_begin != npos)
		{
			diag::error(std::format("lexer: unterminated string literal by eof. line {}. unrecognised token(s): \"{}\"", line_counter, psy.substr(current_word_begin, cursor - current_string_literal_begin)));
		}

		return ret;
	}
}
