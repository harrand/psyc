#ifndef PSYC_AST2_HPP
#define PSYC_AST2_HPP
#include "srcloc.hpp"
#include "util.hpp"
#include "lex.hpp"
#include <string>
#include <memory>
#include <variant>
#include <vector>

namespace syntax
{
	class inode;
	using node_ptr = std::unique_ptr<inode>;

	class inode
	{
	public:
		inode() = default;
		virtual ~inode() = default;
		// imeplemented asap below.
		virtual std::string to_string() const = 0;
		virtual const char* name() const = 0;
		// each node type implements this in their own file, coz it can get really really long.
		virtual std::size_t hash() const
		{
			return typeid(*this).hash_code();
		}
		virtual bool is_lookahead_token() const
		{
			return false;
		}

		std::vector<node_ptr> children = {};
		srcloc loc = srcloc::undefined();
	};

	namespace node
	{
		struct expression;
		using boxed_expression = util::box<expression>;

		// these are very noisy now sadly. because they are subclasses of inode you cant use designated initialisers, so the constructor noise cant be removed.
		struct unparsed_token : public inode
		{
			unparsed_token(lex::token tok, bool is_lookahead = false): tok(tok), is_lookahead(is_lookahead){}
			lex::token tok;
			bool is_lookahead;
			virtual std::string to_string() const final
			{
				return std::format("token({})", tok.lexeme);
			}
			virtual const char* name() const final
			{
				return this->tok.lexeme.c_str();
			}
			virtual std::size_t hash() const final
			{
				// when should two unparsed tokens yield the same hash code?
				// well, we never care about the lexeme itself, but we do care about the type.
				// if im checking for reductions and i know i have a token, i will need to know if the token is a semicolon for example. however, i wont need to know what exactly the comment is, nor what the identifier value is.
				return inode::hash() ^ std::hash<int>{}(static_cast<int>(tok.t));
			}
			virtual bool is_lookahead_token() const override
			{
				return this->is_lookahead;
			}
		};


		struct integer_literal : public inode
		{
			integer_literal(std::int64_t val = 0): val(val){}
			std::int64_t val;
			virtual std::string to_string() const final
			{
				return std::format("integer-literal({})", this->val);
			}
			virtual const char* name() const final
			{
				return "integer literal";
			}
		};

		struct decimal_literal : public inode
		{
			decimal_literal(double val = 0.0): val(val){}
			double val;
			virtual std::string to_string() const final
			{
				return std::format("decimal-literal({})", this->val);
			}
			virtual const char* name() const final
			{
				return "decimal literal";
			}
		};

		struct identifier : public inode
		{
			identifier(std::string iden = ""): iden(iden){}
			std::string iden;

			virtual std::string to_string() const final
			{
				return std::format("identifier({})", this->iden);
			}
			virtual const char* name() const final
			{
				return "identifier";
			}
		};

		struct primary_expression : public inode
		{
			enum class type
			{
				identifier,
				integer_literal,
				decimal_literal,
				string_literal,
				char_literal,
				bool_literal,
				null_literal,
				parenthesised_expression,
				_count
			};
			constexpr static std::array<const char*, int(type::_count)> type_names
			{
				"iden",
				"intlit",
				"declit",
				"strlit",
				"charlit",
				"boollit",
				"nulllit",
				"(expr)"
			};

			primary_expression(type t, const inode* expr): t(t), expr(expr){}

			type t;
			const inode* expr;

			virtual std::string to_string() const final
			{
				return std::format("prim-expr_{}({})", primary_expression::type_names[static_cast<int>(this->t)], this->expr->to_string());
			}

			virtual const char* name() const final
			{
				return "primary expression";
			}
		};
	}

	node_ptr make_node(const lex::token& t);
}

#endif // PSYC_AST2_HPP