#ifndef PSYC_AST_HPP
#define PSYC_AST_HPP
#include "srcloc.hpp"
#include <variant>

struct ast
{
	struct integer_literal
	{
		std::int64_t val;
		constexpr std::string to_string() const
		{
			return std::format("integer-literal({})", val);
		}
	};
	struct decimal_literal
	{
		double val;
		constexpr std::string to_string() const
		{
			return std::format("decimal-literal({})", val);
		}
	};

	struct node
	{
		using payload_t = std::variant<integer_literal, decimal_literal>;
		payload_t payload;
		srcloc meta;

		constexpr std::string to_string() const
		{
			std::string ret;
			std::visit([&ret](auto&& arg)
			{
				ret = arg.to_string();
			}, this->payload);
			return ret;
		}
	};
};

#endif // PSYC_AST_HPP