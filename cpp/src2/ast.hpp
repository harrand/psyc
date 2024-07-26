#ifndef PSYC_AST2_HPP
#define PSYC_AST2_HPP
#include "srcloc.hpp"
#include "util.hpp"
#include "lex.hpp"
#include "util.hpp"
#include "static.hpp"
#include <string>
#include <memory>
#include <vector>
#include <variant>

namespace syntax
{
	#define NODE_IS(some_node, node_type) (some_node)->is<syntax::node::node_type>()
	#define NODE_AS(some_node, node_type) static_cast<syntax::node::node_type*>(some_node)

	namespace node
	{
		struct root
		{
			std::filesystem::path source_file;

			std::string to_string() const
			{
				return this->source_file.string();
			}
			const char* name() const
			{
				return "root node";
			}
			
		};

		struct unfinished_block
		{
			srcloc start;
			
			std::string to_string() const
			{
				return std::format("unfinished block starting at {}", this->start.to_string());
			}
			const char* name() const
			{
				return "unfinished block";
			}
		};

		struct block
		{
			srcloc start;
			srcloc finish;
			
			std::string to_string() const
			{
				return std::format("block from {} to {}", this->start.to_string(), this->finish.to_string());
			}
			const char* name() const
			{
				return "block";
			}
		};

		// these are very noisy now sadly. because they are subclasses of inode you cant use designated initialisers, so the constructor noise cant be removed.
		struct unparsed_token
		{
			lex::token tok;

			std::string to_string() const
			{
				return std::format("token({})", tok.lexeme);
			}
			
			const char* name() const
			{
				return this->tok.lexeme.c_str();
			}
			std::size_t hash() const
			{
				// when should two unparsed tokens yield the same hash code?
				// well, we never care about the lexeme itself, but we do care about the type.
				// if im checking for reductions and i know i have a token, i will need to know if the token is a semicolon for example. however, i wont need to know what exactly the comment is, nor what the identifier value is.
				return std::hash<int>{}(static_cast<int>(tok.t));
			}
		};


		struct integer_literal
		{
			integer_literal(std::int64_t val = 0): val(val){}

			std::int64_t val;
			
			std::string to_string() const
			{
				return std::format("integer-literal({})", this->val);
			}
			const char* name() const
			{
				return "integer literal";
			}
		};

		struct decimal_literal
		{
			decimal_literal(double val = 0.0): val(val){}

			double val;
			
			std::string to_string() const
			{
				return std::format("decimal-literal({})", this->val);
			}
			const char* name() const
			{
				return "decimal literal";
			}
		};

		struct char_literal
		{
			char_literal(char val = ' '): val(val){}

			char val;
			
			std::string to_string() const
			{
				return std::format("char-literal('{}' (dec {})", this->val, static_cast<int>(this->val));
			}
			const char* name() const
			{
				return "char literal";
			}
		};

		struct bool_literal
		{
			bool_literal(bool val = false): val(val){}

			bool val;
			
			std::string to_string() const
			{
				return std::format("bool-literal({})", this->val ? "true" : "false");
			}
			const char* name() const
			{
				return "bool literal";
			}
		};

		struct string_literal
		{
			string_literal(std::string val = ""): val(val){}

			std::string val;
			
			std::string to_string() const
			{
				return std::format("string-literal(\"{}\")", this->val);
			}
			const char* name() const
			{
				return "string literal";
			}
		};

		struct null_literal
		{
			null_literal(){}

			
			std::string to_string() const
			{
				return "null-literal()";
			}
			const char* name() const
			{
				return "null literal";
			}
		};

		constexpr const char* inferred_typename = "<AUTOTYPE>";

		struct identifier
		{
			identifier(std::string iden = ""): iden(iden){}
			std::string iden;

			
			std::string to_string() const
			{
				return std::format("identifier({})", this->iden);
			}
			const char* name() const
			{
				return "identifier";
			}
		};

		struct expression
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
				function_call,
				return_statement,
				cast,
				deref,
				ref,
				typeinfo,
				defer,
				addition,
				subtraction,
				multiplication,
				division,
				assign,
				eqcompare,
				neqcompare,
				dot_access,
				namespace_access,
				struct_initialiser,
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
				"parenthesised",
				"call",
				"return",
				"cast",
				"deref",
				"ref",
				"typeinfo",
				"defer",
				"\"+\"",
				"\"-\"",
				"\"*\"",
				"\"/\"",
				"\"=\"",
				"\"==\"",
				"\"!=\"",
				"\".\"",
				"\"::\"",
				"structinit"
			};

			expression(type t = type::_unknown, node_ptr expr = nullptr, node_ptr extra = nullptr, bool capped = false): t(t), expr(std::move(expr)), extra(std::move(extra)), capped(capped){}
			expression(const expression& cpy): inode(cpy),
			t(cpy.t), expr(cpy.expr == nullptr ? nullptr : cpy.expr->unique_clone()), extra(cpy.extra == nullptr ? nullptr : cpy.extra->unique_clone()), capped(cpy.capped){}
			expression& operator=(expression rhs)
			{
				std::swap(this->t, rhs.t);
				std::swap(this->expr, rhs.expr);
				std::swap(this->extra, rhs.extra);
				std::swap(this->capped, rhs.capped);
				return *this;
			}

			type t;
			node_ptr expr;
			node_ptr extra = nullptr;
			bool capped;

			bool is_null() const{return this->expr == nullptr || this->t == type::_unknown;}

			
			std::string to_string() const
			{
				return std::format("expr-{}({}{})", expression::type_names[static_cast<int>(this->t)], this->expr->to_string(), this->extra != nullptr ? std::format(", {}", this->extra->to_string()) : "");
			}

			const char* name() const
			{
				return "expression";
			}
		};

		struct expression_list
		{
			expression_list(std::vector<expression> exprs = {}): exprs(exprs){}

			std::vector<expression> exprs;

			
			std::string to_string() const
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

			const char* name() const
			{
				return "expression list";
			}
		};


		struct namespace_access
		{
			namespace_access(identifier lhs = {}, expression rhs = {}):
			inode(lhs),
			lhs_parts({lhs.iden}),
			rhs(rhs){}
			namespace_access(identifier lhs, const namespace_access& rhs):
			inode(lhs),
			lhs_parts({lhs.iden}),
			rhs(rhs.rhs)
			{
				// a::b::foo() = iden(a)::namespace_access(iden(b), expr(foo()))
				for(std::string part : rhs.lhs_parts)
				{
					this->lhs_parts.push_back(part);
				}
			}
			
			std::vector<std::string> lhs_parts;
			expression rhs;

			
			std::string to_string() const
			{
				std::string lhs_total;
				for(auto part : this->lhs_parts)
				{
					lhs_total += part;
				}
				return std::format("namespace-access({}::{})", lhs_total, this->rhs.to_string());
			}

			const char* name() const
			{
				return "namespace access";
			}
		};

		struct variable_decl
		{
			variable_decl(identifier var_name = {}, identifier type_name = {}, expression expr = {}, bool capped = false): var_name(var_name), type_name(type_name), expr(expr), capped(capped){}

			identifier var_name;
			identifier type_name;
			expression expr;
			bool capped;
			mutable bool impl_should_add_to_current_scope = true;
			mutable bool impl_is_defined_before_parent_block = false;

			

			std::string to_string() const
			{
				return std::format("variable-decl({} : {}{})", this->var_name.to_string(), this->type_name.to_string(), expr.is_null() ? "" : std::format(" := {}", expr.to_string()));
			}

			const char* name() const
			{
				return "variable declaration";
			}
		};

		struct variable_decl_list
		{
			variable_decl_list(std::vector<variable_decl> decls = {}): decls(decls){}

			std::vector<variable_decl> decls;

			
			std::string to_string() const
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

			const char* name() const
			{
				return "variable list";
			}
		};
		
		struct function_decl
		{
			function_decl(identifier func_name = {}, variable_decl_list params = {}, identifier return_type_name = {}): func_name(func_name), params(params), return_type_name(return_type_name){}

			identifier func_name;
			variable_decl_list params;
			identifier return_type_name;
			std::string struct_owner = "";
			bool is_extern = false;
			bool capped = false;

			
			std::string to_string() const
			{
				return std::format("function-decl({} :: {} -> {}{})", this->func_name.to_string(), this->params.to_string(), this->return_type_name.to_string(), this->is_extern ? ":= extern" : "");
			}

			const char* name() const
			{
				return "function declaration";
			}
		};

		struct function_call
		{
			function_call(identifier func_name = {}, expression_list params = {}): func_name(func_name), params(params){}

			identifier func_name;
			expression_list params;

			
			std::string to_string() const
			{
				return std::format("function-call({}({}))", this->func_name.to_string(), this->params.to_string());
			}

			const char* name() const
			{
				return "function call";
			}
		};

		struct meta_region
		{
			enum class type
			{
				name_space,
				static_if,
				build,
				_unknown,
				_count
			};
			constexpr static std::array<const char*, int(type::_count)> type_names
			{
				"namespace",
				"static_if",
				"build",
			};

			meta_region(identifier metaname = {}, type t = type::_unknown): metaname(metaname), t(t){}

			identifier metaname;
			type t;
			bool capped = false;

			
			std::string to_string() const
			{
				return std::format("meta-region({} : {})", this->metaname.to_string(), type_names[static_cast<int>(this->t)]);
			}

			const char* name() const
			{
				return "meta region";
			}
		};

		struct alias
		{
			alias(identifier alias_name = {}, expression type_value_expr = {}): alias_name(alias_name), type_value_expr(type_value_expr){}

			identifier alias_name;
			expression type_value_expr;

			
			std::string to_string() const
			{
				return std::format("alias({} ::= {})", this->alias_name.iden, type_value_expr.to_string());
			}

			const char* name() const
			{
				return "alias specifier";
			}
		};

		struct struct_decl
		{
			struct_decl(identifier struct_name = {}, bool capped = false): struct_name(struct_name), capped(capped){}

			identifier struct_name;
			bool capped = false;

			
			std::string to_string() const
			{
				return std::format("struct({})", this->struct_name.iden);
			}

			const char* name() const
			{
				return "struct";
			}
		};

		struct designated_initialiser
		{
			designated_initialiser(identifier member = {}, expression initialiser = {}): member(member), initialiser(initialiser){}

			identifier member;
			expression initialiser;

			
			std::string to_string() const
			{
				return std::format("member-initialiser({} := {})", this->member.iden, this->initialiser.to_string());
			}

			const char* name() const
			{
				return "designated initialiser";
			}
		};

		struct designated_initialiser_list
		{
			designated_initialiser_list(std::vector<designated_initialiser> inits = {}): inits(inits){}

			std::vector<designated_initialiser> inits;

			
			std::string to_string() const
			{
				std::string contents = "";
				for(std::size_t i = 0; i < this->inits.size(); i++)
				{
					contents += this->inits[i].to_string();
					if(i < (this->inits.size() - 1))
					{
						contents += ", ";
					}
				}
				return std::format("desig-init-list({})", contents);
			}

			const char* name() const
			{
				return "designated initialiser list";
			}
		};

		struct if_statement
		{
			if_statement(expression cond = {}, block blk = {}, bool is_static = false): cond(cond), is_static(is_static)
			{
				this->children.push_back(blk.unique_clone());
			}
			expression cond;
			bool is_static;

			std::string to_string() const
			{
				return std::format("if-statement({})", this->cond.to_string());
			}

			const char* name() const
			{
				return "if-statement";
			}
		};

		struct else_statement
		{
			else_statement(expression cond = {}, block blk = {}): cond(cond)
			{
				this->children.push_back(blk.unique_clone());
			}

			expression cond;

			
			std::string to_string() const
			{
				if(this->cond.is_null())
				{
					return "else()";
				}
				return std::format("else-if({})", this->cond.to_string());
			}

			const char* name() const
			{
				return "else-statement";
			}
		};
	}

	struct nodenew
	{
		using payload_t = std::variant<std::monostate,
			node::root,
			node::unfinished_block,
			node::block,
			node::unparsed_token,
			node::integer_literal,
			node::decimal_literal,
			node::char_literal,
			node::bool_literal,
			node::string_literal,
			node::null_literal,
			node::identifier,
			node::expression,
			node::expression_list,
			node::namespace_access,
			node::variable_decl,
			node::variable_decl_list,
			node::function_decl,
			node::function_call,
			node::meta_region,
			node::alias,
			node::struct_decl,
			node::designated_initialiser,
			node::designated_initialiser_list,
			node::if_statement,
			node::else_statement
			>;
		payload_t payload = std::monostate{};
		srcloc loc;
		std::vector<nodenew> children = {};

		std::string to_string() const;
		const char* name() const;
		std::size_t hash() const;

		void pretty_print() const;
	};

	node_ptr make_node(const lex::token& t);
}

#endif // PSYC_AST2_HPP