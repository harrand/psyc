#ifndef PSYC_AST_HPP
#define PSYC_AST_HPP
#include "srcloc.hpp"
#include "util.hpp"
#include "lex.hpp"
#include <string>
#include <memory>
#include <vector>

namespace syntax
{
	class inode;
	using node_ptr = std::unique_ptr<inode>;

	class inode
	{
	public:
		virtual ~inode() = default;
		// imeplemented asap below.
		virtual std::string to_string() const;
		// each node type implements this in their own file, coz it can get really really long.
		virtual node_ptr reduce(void* ctx);
		virtual std::size_t hash() const;

		std::vector<node_ptr> children = {};
		srcloc loc = srcloc::undefined();
	};

	namespace node
	{
		struct expression;
		using boxed_expression = util::box<expression>;

		struct unparsed_token : public inode
		{
			lex::token tok;
			virtual std::string to_string() const final
			{
				return std::format("token({})", tok.lexeme);
			}
			virtual std::size_t hash() const final
			{
				// 2 unparsed tokens should hash to the same value iff their type and lexeme are the same.
				return std::hash<int>{}(static_cast<int>(tok.t)) ^ std::hash<std::string>{}(tok.lexeme);
			}
		};

		#define DEFAULT_HASH virtual std::size_t hash() const final{return typeid(*this).hash_code();}

		struct integer_literal : public inode
		{
			std::int64_t val;
			virtual std::string to_string() const final
			{
				return std::format("integer-literal({})", this->val);
			}
			DEFAULT_HASH
		};

		struct decimal_literal : public inode
		{
			double val;
			virtual std::string to_string() const final
			{
				return std::format("decimal-literal({})", this->val);
			}
			DEFAULT_HASH
		};
	}

	node_ptr make_node(const lex::token& t);
}

#endif // PSYC_AST_HPP