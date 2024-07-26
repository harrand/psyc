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
	class inode;
	using node_ptr = std::unique_ptr<inode>;

	class inode : public util::unique_cloneable<inode>
	{
	public:
		inode() = default;
		inode(const inode& cpy): children(), loc(cpy.loc), semal(cpy.semal.clone())
		{
			for(const auto& child_ptr : cpy.children)
			{
				this->children.push_back(child_ptr->unique_clone());
			}
		}
		inode& operator=(const inode& rhs)
		{
			this->children.clear();
			for(const auto& child_ptr : rhs.children)
			{
				this->children.push_back(child_ptr->unique_clone());
			}
			this->loc = rhs.loc;
			return *this;
		}

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

		template<typename T> requires std::is_base_of_v<inode, T>
		bool is() const
		{
			return this->hash() == T{}.hash();
		}

		void pretty_print() const;

		std::vector<node_ptr> children = {};
		srcloc loc = srcloc::undefined();
		mutable static_value semal;
	};

	#define NODE_IS(some_node, node_type) (some_node)->is<syntax::node::node_type>()
	#define NODE_AS(some_node, node_type) static_cast<syntax::node::node_type*>(some_node)

	namespace node
	{
		struct root : public inode
		{
			root(std::filesystem::path source_file = {}): source_file(source_file){}

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

		struct unfinished_block : public inode
		{
			unfinished_block(): start(srcloc::undefined()){}
			unfinished_block(node_ptr node):
			start(node->loc)
			{
				this->children.push_back(std::move(node));
			}

			srcloc start;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("unfinished block starting at {}", this->start.to_string());
			}
			virtual const char* name() const final
			{
				return "unfinished block";
			}

			void extend(node_ptr node)
			{
				this->children.push_back(std::move(node));
			}
		};

		struct block : public inode
		{
			block(): start(srcloc::undefined()), finish(srcloc::undefined()){}

			block(unfinished_block blk, srcloc finish):
			start(blk.start),
			finish(finish)
			{
				this->children = std::move(blk.children);
			}

			srcloc start;
			srcloc finish;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("block from {} to {}", this->start.to_string(), this->finish.to_string());
			}
			virtual const char* name() const final
			{
				return "block";
			}

			void extend(node_ptr node)
			{
				this->children.push_back(std::move(node));
			}
		};

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

		struct char_literal : public inode
		{
			char_literal(char val = ' '): val(val){}

			char val;
			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("char-literal('{}' (dec {})", this->val, static_cast<int>(this->val));
			}
			virtual const char* name() const final
			{
				return "char literal";
			}
		};

		struct bool_literal : public inode
		{
			bool_literal(bool val = false): val(val){}

			bool val;
			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("bool-literal({})", this->val ? "true" : "false");
			}
			virtual const char* name() const final
			{
				return "bool literal";
			}
		};

		struct string_literal : public inode
		{
			string_literal(std::string val = ""): val(val){}

			std::string val;
			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("string-literal(\"{}\")", this->val);
			}
			virtual const char* name() const final
			{
				return "string literal";
			}
		};

		struct null_literal : public inode
		{
			null_literal(){}

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

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("expr-{}({}{})", expression::type_names[static_cast<int>(this->t)], this->expr->to_string(), this->extra != nullptr ? std::format(", {}", this->extra->to_string()) : "");
			}

			virtual const char* name() const final
			{
				return "expression";
			}
		};

		struct expression_list : public inode
		{
			expression_list(std::vector<expression> exprs = {}): exprs(exprs){}

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


		struct namespace_access : public inode
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

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				std::string lhs_total;
				for(auto part : this->lhs_parts)
				{
					lhs_total += part;
				}
				return std::format("namespace-access({}::{})", lhs_total, this->rhs.to_string());
			}

			virtual const char* name() const final
			{
				return "namespace access";
			}
		};

		struct variable_decl : public inode
		{
			variable_decl(identifier var_name = {}, identifier type_name = {}, expression expr = {}, bool capped = false): var_name(var_name), type_name(type_name), expr(expr), capped(capped){}

			identifier var_name;
			identifier type_name;
			expression expr;
			bool capped;
			mutable bool impl_should_add_to_current_scope = true;
			mutable bool impl_is_defined_before_parent_block = false;

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

			identifier func_name;
			variable_decl_list params;
			identifier return_type_name;
			std::string struct_owner = "";
			bool is_extern = false;
			bool capped = false;

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

			identifier func_name;
			expression_list params;

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

		struct meta_region : public inode
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

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("meta-region({} : {})", this->metaname.to_string(), type_names[static_cast<int>(this->t)]);
			}

			virtual const char* name() const final
			{
				return "meta region";
			}
		};

		struct alias : public inode
		{
			alias(identifier alias_name = {}, expression type_value_expr = {}): alias_name(alias_name), type_value_expr(type_value_expr){}

			identifier alias_name;
			expression type_value_expr;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("alias({} ::= {})", this->alias_name.iden, type_value_expr.to_string());
			}

			virtual const char* name() const final
			{
				return "alias specifier";
			}
		};

		struct struct_decl : public inode
		{
			struct_decl(identifier struct_name = {}, bool capped = false): struct_name(struct_name), capped(capped){}

			identifier struct_name;
			bool capped = false;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("struct({})", this->struct_name.iden);
			}

			virtual const char* name() const final
			{
				return "struct";
			}
		};

		struct designated_initialiser : public inode
		{
			designated_initialiser(identifier member = {}, expression initialiser = {}): member(member), initialiser(initialiser){}

			identifier member;
			expression initialiser;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("member-initialiser({} := {})", this->member.iden, this->initialiser.to_string());
			}

			virtual const char* name() const final
			{
				return "designated initialiser";
			}
		};

		struct designated_initialiser_list : public inode
		{
			designated_initialiser_list(std::vector<designated_initialiser> inits = {}): inits(inits){}

			std::vector<designated_initialiser> inits;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
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

			virtual const char* name() const final
			{
				return "designated initialiser list";
			}
		};

		struct if_statement : public inode
		{
			if_statement(expression cond = {}, block blk = {}, bool is_static = false): cond(cond), is_static(is_static)
			{
				this->children.push_back(blk.unique_clone());
			}
			expression cond;
			bool is_static;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				return std::format("if-statement({})", this->cond.to_string());
			}

			virtual const char* name() const final
			{
				return "if-statement";
			}
		};

		struct else_statement : public inode
		{
			else_statement(expression cond = {}, block blk = {}): cond(cond)
			{
				this->children.push_back(blk.unique_clone());
			}

			expression cond;

			COPY_UNIQUE_CLONEABLE(inode)
			virtual std::string to_string() const final
			{
				if(this->cond.is_null())
				{
					return "else()";
				}
				return std::format("else-if({})", this->cond.to_string());
			}

			virtual const char* name() const final
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

		std::string to_string() const
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

	node_ptr make_node(const lex::token& t);
}

#endif // PSYC_AST2_HPP