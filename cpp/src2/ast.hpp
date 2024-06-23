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
		virtual std::string to_string() const;
		virtual node_ptr reduce(void* ctx);

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
		};

		struct integer_literal : public inode
		{
			std::int64_t val;
			virtual std::string to_string() const final
			{
				return std::format("integer-literal({})", this->val);
			}
		};
	}

	node_ptr make_node(const lex::token& t);
}

#endif // PSYC_AST_HPP