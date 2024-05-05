#ifndef PSYC_AST_HPP
#define PSYC_AST_HPP
#include "srcloc.hpp"
#include <variant>

struct ast
{
	struct node
	{
		using payload_t = std::variant<int>;
		payload_t payload;
		srcloc meta;
	};
};

#endif // PSYC_AST_HPP