#ifndef PSYC_AST_HPP
#define PSYC_AST_HPP
#include "srcloc.hpp"
#include <variant>
#include <span>

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
	struct identifier
	{
		std::string iden;
		constexpr std::string to_string() const
		{
			return std::format("identifier({})", iden);
		}
	};

	struct node
	{
		using payload_t = std::variant<std::monostate, integer_literal, decimal_literal, identifier>;
		payload_t payload;
		srcloc meta;
		std::vector<node> children = {};

		constexpr std::string to_string() const
		{
			std::string ret;
			std::visit([&ret](auto&& arg)
			{
				if constexpr(std::is_same_v<std::decay_t<decltype(arg)>, std::monostate>)
				{
					ret = "<empty>";
				}
				else
				{
					ret = arg.to_string();
				}
			}, this->payload);
			return ret;
		}
	};

	using path_t = std::vector<std::size_t>;
	using path_const_view_t = std::span<const std::size_t>;
	using path_view_t = std::span<std::size_t>;
	
	const node& get(path_const_view_t path) const;
	node& get(path_const_view_t path);

	// given a path, retrieve a path pointing to the previous node (i.e the node before this path with the same parent). if it can't be done, return nullopt.
	std::optional<path_t> try_get_previous(path_t path) const;
	// given a path, retrieve a path pointing to the next node (i.e the node after this path with the same parent). if it can't be done, return nullopt.
	std::optional<path_t> try_get_next(path_t path) const;

	const node& current() const;
	node& current();

	node root;
	path_t current_path = {};
};

#endif // PSYC_AST_HPP