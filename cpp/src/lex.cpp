#include "lex.hpp"

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

		while(cursor < psy.size())
		{
			std::string_view data = psy.substr(cursor);
			if(data.starts_with("\n"))
			{
				ret.push_back({.id = token::type::newline});
			}
			cursor++;
		}

		return ret;
	}
}
