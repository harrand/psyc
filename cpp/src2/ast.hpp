#ifndef PSYC_AST2_HPP
#define PSYC_AST2_HPP
#include "srcloc.hpp"
#include "util.hpp"
#include "lex.hpp"
#include "util.hpp"
#include <string>
#include <memory>
#include <vector>

namespace syntax
{
	class inode;
	using node_ptr = std::unique_ptr<inode>;

	class inode : public util::unique_cloneable<inode>
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

		void pretty_print() const;

		std::vector<node_ptr> children = {};
		srcloc loc = srcloc::undefined();
	};

	namespace node
	{
		struct root : public inode
		{
			root(std::filesystem::path source_file = {}): source_file(source_file){}

			root(const root& cpy): source_file(cpy.source_file){}

			std::filesystem::path source_file;

			virtual std::string to_string() const final
			{
				return this->source_file.string();
			}
			virtual const char* name() const final
			{
				return "root node";
			}
			COPY_UNIQUE_CLONEABLE(inode)
		};

		// these are very noisy now sadly. because they are subclasses of inode you cant use designated initialisers, so the constructor noise cant be removed.
		struct unparsed_token : public inode
		{
			unparsed_token(lex::token tok, bool is_lookahead = false): tok(tok), is_lookahead(is_lookahead){}
			unparsed_token(const unparsed_token& cpy): tok(cpy.tok), is_lookahead(cpy.is_lookahead){}
			//unparsed_token(const unparsed_token& cpy) = default;
			lex::token tok;
			bool is_lookahead;
			virtual std::string to_string() const final
			{
				return std::format("token({})", tok.lexeme);
			}
			COPY_UNIQUE_CLONEABLE(inode)
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
			integer_literal(const integer_literal& cpy): val(cpy.val){}

			std::int64_t val;
			COPY_UNIQUE_CLONEABLE(inode)
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
			decimal_literal(const decimal_literal& cpy): val(cpy.val){}

			double val;
			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("decimal-literal({})", this->val);
			}
			virtual const char* name() const final
			{
				return "decimal literal";
			}
		};

		struct null_literal : public inode
		{
			null_literal(){}
			null_literal(const null_literal& cpy): null_literal{}{}

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return "null-literal()";
			}
			virtual const char* name() const final
			{
				return "null literal";
			}
		};

		constexpr const char* inferred_typename = "<AUTOTYPE>";

		struct identifier : public inode
		{
			identifier(std::string iden = ""): iden(iden){}
			identifier(const identifier& cpy): iden(cpy.iden){}
			std::string iden;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("identifier({})", this->iden);
			}
			virtual const char* name() const final
			{
				return "identifier";
			}
		};

		struct expression : public inode
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
				_unknown,
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

			expression(type t = type::_unknown, node_ptr expr = nullptr): t(t), expr(std::move(expr)){}
			expression(const expression& cpy):
			t(cpy.t), expr(cpy.expr == nullptr ? nullptr : cpy.expr->unique_clone()){}
			expression& operator=(expression rhs)
			{
				std::swap(this->t, rhs.t);
				std::swap(this->expr, rhs.expr);
				return *this;
			}

			type t;
			node_ptr expr;

			bool is_null() const{return this->expr == nullptr || this->t == type::_unknown;}

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("prim-expr_{}({})", expression::type_names[static_cast<int>(this->t)], this->expr->to_string());
			}

			virtual const char* name() const final
			{
				return "primary expression";
			}
		};

		struct expression_list : public inode
		{
			expression_list(std::vector<expression> exprs = {}): exprs(exprs){}
			expression_list(const expression_list& rhs): exprs(rhs.exprs){}

			std::vector<expression> exprs;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				std::string contents = "";
				for(std::size_t i = 0; i < this->exprs.size(); i++)
				{
					contents += this->exprs[i].to_string();
					if(i < (this->exprs.size() - 1))
					{
						contents += ", ";
					}
				}
				return std::format("expr-list({})", contents);
			}

			virtual const char* name() const final
			{
				return "expression list";
			}
		};

		struct variable_decl : public inode
		{
			variable_decl(identifier var_name = {}, identifier type_name = {}, expression expr = {}): var_name(var_name), type_name(type_name), expr(expr){}
			variable_decl(const variable_decl& cpy): var_name(cpy.var_name), type_name(cpy.type_name), expr(cpy.expr){}

			identifier var_name;
			identifier type_name;
			expression expr;

			COPY_UNIQUE_CLONEABLE(inode)

			virtual std::string to_string() const final
			{
				return std::format("variable-decl({} : {}{})", this->var_name.to_string(), this->type_name.to_string(), expr.is_null() ? "" : std::format(" := {}", expr.to_string()));
			}

			virtual const char* name() const final
			{
				return "variable declaration";
			}
		};

		struct variable_decl_list : public inode
		{
			variable_decl_list(std::vector<variable_decl> decls = {}): decls(decls){}
			variable_decl_list(const variable_decl_list& rhs): decls(rhs.decls){}

			std::vector<variable_decl> decls;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				std::string contents = "";
				for(std::size_t i = 0; i < this->decls.size(); i++)
				{
					contents += this->decls[i].to_string();
					if(i < (this->decls.size() - 1))
					{
						contents += ", ";
					}
				}
				return std::format("variable-decl-list({})", contents);
			}

			virtual const char* name() const final
			{
				return "variable list";
			}
		};
		
		struct function_decl : public inode
		{
			function_decl(identifier func_name = {}, variable_decl_list params = {}, identifier return_type_name = {}): func_name(func_name), params(params), return_type_name(return_type_name){}

			function_decl(const function_decl& cpy): func_name(cpy.func_name), params(cpy.params), return_type_name(cpy.return_type_name), is_extern(cpy.is_extern){}

			identifier func_name;
			variable_decl_list params;
			identifier return_type_name;
			bool is_extern = false;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("function-decl({} :: {} -> {}{})", this->func_name.to_string(), this->params.to_string(), this->return_type_name.to_string(), this->is_extern ? ":= extern" : "");
			}

			virtual const char* name() const final
			{
				return "function declaration";
			}
		};

		struct function_call : public inode
		{
			function_call(identifier func_name = {}, expression_list params = {}): func_name(func_name), params(params){}

			function_call(const function_call& cpy): func_name(cpy.func_name), params(cpy.params){}

			identifier func_name;
			expression_list params;
			identifier return_type_name;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("function-call({}({}))", this->func_name.to_string(), this->params.to_string());
			}

			virtual const char* name() const final
			{
				return "function call";
			}
		};
	}

	node_ptr make_node(const lex::token& t);
}

#endif // PSYC_AST2_HPP