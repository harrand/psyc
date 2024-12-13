#ifndef PSYC_AST2_HPP
#define PSYC_AST2_HPP
#include "srcloc.hpp"
#include "util.hpp"
#include "lex.hpp"
#include "util.hpp"
#include "static.hpp"
#include "profile.hpp"
#include <string>
#include <memory>
#include <vector>
#include <variant>
#include <functional>

namespace syntax
{
	struct node;
	using boxed_node = util::box<node>;

	struct nodecomn
	{
		srcloc loc = srcloc::undefined();
		std::vector<boxed_node> children = {};
	};

	struct root : public nodecomn
	{
		std::filesystem::path source_file;

		std::string to_string() const
		{
			return this->source_file.string();
		}
		
	};

	struct unfinished_block : public nodecomn
	{
		unfinished_block(): start(srcloc::undefined()){}
		template<typename T>
		unfinished_block(T node):
		start(node.loc)
		{
			this->children.reserve(64);
			this->children.push_back(syntax::node{.payload = node});
		}

		srcloc start;
		
		std::string to_string() const
		{
			return std::format("unfinished block starting at {}", this->start.to_string());
		}

		template<typename T>
		void extend(T node)
		{
			this->children.push_back(syntax::node{.payload = node});	
		}
	};

	struct block : public nodecomn
	{
		block() = default;
		block(unfinished_block blk, srcloc finish):
		start(blk.start),
		finish(finish)
		{
			this->children = std::move(blk.children);
		}

		srcloc start = srcloc::undefined();
		srcloc finish = srcloc::undefined();
		
		std::string to_string() const
		{
			return std::format("block from {} to {}", this->start.to_string(), this->finish.to_string());
		}
	};

	// these are very noisy now sadly. because they are subclasses of inode you cant use designated initialisers, so the constructor noise cant be removed.
	struct unparsed_token : public nodecomn
	{
		lex::token tok;

		std::string to_string() const
		{
			return std::format("'{}'", tok.lexeme);
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


	struct integer_literal : public nodecomn
	{
		integer_literal(std::int64_t val = 0): val(val){}

		std::int64_t val;
		
		std::string to_string() const
		{
			return std::format("{}", this->val);
		}
	};

	struct decimal_literal : public nodecomn
	{
		decimal_literal(double val = 0.0): val(val){}

		double val;
		
		std::string to_string() const
		{
			return std::format("{}", this->val);
		}
		const char* name() const
		{
			return "decimal literal";
		}
	};

	struct char_literal : public nodecomn
	{
		char_literal(char val = ' '): val(val){}

		char val;
		
		std::string to_string() const
		{
			return std::format("'{}'", this->val, static_cast<int>(this->val));
		}
	};

	struct bool_literal : public nodecomn
	{
		bool_literal(bool val = false): val(val){}

		bool val;
		
		std::string to_string() const
		{
			return std::format("bool-literal({})", this->val ? "true" : "false");
		}
	};

	struct string_literal : public nodecomn
	{
		string_literal(std::string val = ""): val(val){}

		std::string val;
		
		std::string to_string() const
		{
			return std::format("\"{}\"", this->val);
		}
	};

	struct null_literal : public nodecomn
	{
		null_literal(){}

		
		std::string to_string() const
		{
			return "null";
		}
	};

	constexpr const char* inferred_typename = "<AUTOTYPE>";

	struct identifier : public nodecomn
	{
		identifier(std::string iden = ""): iden(iden){}
		std::string iden;

		
		std::string to_string() const
		{
			return this->iden;
		}
	};

	struct expression : public nodecomn
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
			function_definition,
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
			struct_decl,
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
			"fndef",
			"fncall",
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
			"structdecl",
			"structinit",
			"unknown"
		};

		expression();

		template<typename T, typename U = T>
		expression(type t, T expr, U extra, bool capped = false): t(t), expr(util::box{syntax::node{.payload = expr}}), extra(util::box{syntax::node{.payload = extra}}), capped(capped){}
		template<typename T>
		expression(type t, T expr): expression(t, expr, T{}, false){}

		type t = type::_unknown;
		boxed_node expr;
		boxed_node extra;
		bool capped = false;

		bool is_null() const;
		
		std::string to_string() const;
	};

	struct capped_expression : public expression
	{
		capped_expression(): expression()
		{
			expression::capped = true;
		}
		capped_expression(const expression& expr): expression(expr)
		{
			this->capped = true;
		}
		template<typename T, typename U = T>
		capped_expression(type t, T expr, U extra): expression(t, expr, extra, true){}
		template<typename T>
		capped_expression(type t, T expr): expression(t, expr, T{}, true){}
	};


	struct expression_list : public nodecomn
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
			return std::format("{}", contents);
		}
	};

	struct annotations : public expression_list
	{
		annotations(std::vector<expression> exprs = {}): expression_list(exprs){}
		annotations(const expression_list& elist): expression_list(elist){}

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
			return std::format("{}", contents);
		}
	};

	struct namespace_access : public nodecomn
	{
		namespace_access(identifier lhs = {}, expression rhs = {}):
		lhs_parts({lhs.iden}),
		rhs(rhs){}
		namespace_access(identifier lhs, const namespace_access& rhs):
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
	};

	struct variable_decl : public nodecomn
	{
		variable_decl(identifier var_name = {}, identifier type_name = {}, expression expr = {}, bool capped = false): var_name(var_name), type_name(type_name), expr(expr), capped(capped){}

		identifier var_name;
		identifier type_name;
		expression expr;
		annotations annotations = {};
		bool capped;
		mutable bool impl_should_add_to_current_scope = true;
		mutable bool impl_is_defined_before_parent_block = false;

		std::string to_string() const
		{
			return std::format("{}{} : {}{}", this->annotations.exprs.size() ? std::format("[[{}]] ", this->annotations.to_string()) : "", this->var_name.to_string(), this->type_name.to_string(), expr.is_null() ? "" : std::format(" := {}", expr.to_string()));
		}
	};

	struct capped_variable_decl : public variable_decl
	{
		capped_variable_decl(const variable_decl& cpy): variable_decl(cpy)
		{
			variable_decl::capped = true;
		}
		capped_variable_decl(identifier var_name = {}, identifier type_name = {}, expression expr = {}): variable_decl(var_name, type_name, expr, true){}
	};

	struct variable_decl_list : public nodecomn
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
			return std::format("{}", contents);
		}
	};
	
	struct function_decl : public nodecomn
	{
		function_decl(variable_decl_list constparams = {}, variable_decl_list params = {}, identifier return_type_name = {}): params(params), return_type_name(return_type_name){}

		variable_decl_list constparams;
		variable_decl_list params;
		identifier return_type_name;
		annotations annotations = {};
		std::string struct_owner = "";
		bool is_extern = false;
		bool capped = false;
		
		std::string to_string() const
		{
			return std::format("func{}<{}>({}) -> {}{}", this->annotations.exprs.size() ? std::format(" [{}] ", this->annotations.to_string()) : "", this->constparams.to_string(), this->params.to_string(), this->return_type_name.to_string(), this->is_extern ? ":= extern" : "");
		}
	};

	struct capped_function_decl : public function_decl
	{
		capped_function_decl(const function_decl& cpy): function_decl(cpy)
		{
			function_decl::capped = true;
		}
		capped_function_decl(variable_decl_list constparams = {}, variable_decl_list params = {}, identifier return_type_name = {}): function_decl(constparams, params, return_type_name)
		{
			function_decl::capped = true;
		}
	};

	struct function_call : public nodecomn
	{
		function_call(identifier func_name = {}, expression_list constparams = {}, expression_list params = {}): func_name(func_name), params(params){}

		identifier func_name;
		expression_list constparams;
		expression_list params;
		
		std::string to_string() const
		{
			return std::format("{}<{}>({})", this->func_name.to_string(), this->constparams.to_string(), this->params.to_string());
		}
	};

	struct meta_region : public nodecomn
	{
		enum class type
		{
			build,
			_unknown,
			_count
		};
		constexpr static std::array<const char*, int(type::_count)> type_names
		{
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
	};

	struct capped_meta_region : public meta_region
	{
		capped_meta_region(identifier metaname = {}, type t = type::_unknown): meta_region(metaname, t)
		{
			meta_region::capped = true;
		}
		capped_meta_region(const meta_region& cpy): meta_region(cpy)
		{
			meta_region::capped = true;
		}
	};

	struct struct_decl : public nodecomn
	{
		struct_decl(variable_decl_list constparams = {}, bool capped = false): capped(capped){}

		variable_decl_list constparams;
		annotations annotations = {};
		bool capped = false;
		
		std::string to_string() const
		{
			return std::format("struct(<{}>)", constparams.to_string());
		}

		const char* name() const
		{
			return "struct";
		}
	};

	struct capped_struct_decl : public struct_decl
	{
		capped_struct_decl(): struct_decl(variable_decl_list{}, true){}
		capped_struct_decl(const struct_decl& cpy): struct_decl(cpy)
		{
			struct_decl::capped = true;
		}

	};

	struct designated_initialiser : public nodecomn
	{
		designated_initialiser(identifier member = {}, expression initialiser = {}): member(member), initialiser(initialiser){}

		identifier member;
		expression initialiser;

		
		std::string to_string() const
		{
			return std::format("member-initialiser({} := {})", this->member.iden, this->initialiser.to_string());
		}
	};

	struct designated_initialiser_list : public nodecomn
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
	};

	struct struct_initialiser : public nodecomn
	{
		struct_initialiser(identifier struct_name = {}, expression_list constinits = {}, designated_initialiser_list inits = {}): 
		struct_name(struct_name), constinits(constinits), inits(inits){}

		identifier struct_name;
		expression_list constinits;
		designated_initialiser_list inits;

		std::string to_string() const
		{
			return std::format("struct-init({}<{}> {{{}}})", struct_name.iden, constinits.to_string(), inits.to_string());
		}
	};

	struct if_statement : public nodecomn
	{
		if_statement(expression cond = {}, block blk = {}, bool is_static = false);

		expression cond;
		bool is_static;

		std::string to_string() const
		{
			return std::format("if-statement({})", this->cond.to_string());
		}
	};

	struct else_statement : public nodecomn
	{
		else_statement(expression cond = {}, block blk = {});

		expression cond;

		std::string to_string() const
		{
			if(this->cond.is_null())
			{
				return "else()";
			}
			return std::format("else-if({})", this->cond.to_string());
		}
	};

	struct node
	{
		using payload_t = std::variant<std::monostate,
			root,
			block,
			unfinished_block,
			unparsed_token,
			integer_literal,
			decimal_literal,
			char_literal,
			bool_literal,
			string_literal,
			null_literal,
			identifier,
			expression,
			capped_expression,
			expression_list,
			namespace_access,
			variable_decl,
			capped_variable_decl,
			variable_decl_list,
			function_decl,
			capped_function_decl,
			function_call,
			meta_region,
			capped_meta_region,
			struct_decl,
			capped_struct_decl,
			designated_initialiser,
			designated_initialiser_list,
			struct_initialiser,
			if_statement,
			else_statement
			>;
		using path_t = std::vector<std::size_t>;
		using path_view_t = std::span<const std::size_t>;
		static constexpr const char* payload_names[] =
		{
			"unknown",
			"root",
			"block",
			"unfinished block",
			"token",
			"integer literal",
			"decimal literal",
			"char literal",
			"bool literal",
			"string literal",
			"null literal",
			"identifier",
			"expression",
			"capped expression",
			"expression list",
			"namespace access",
			"variable declaration",
			"capped variable declaration",
			"variable declaration list",
			"function declaration",
			"capped function declaration",
			"function call",
			"meta region",
			"capped meta region",
			"struct declaration",
			"capped struct declaration",
			"designated initialiser",
			"designated initialiser list",
			"struct initialiser",
			"if statement",
			"else statement"
		};
		payload_t payload = std::monostate{};

		bool has_value() const;

		std::string to_string() const;
		const char* name() const;
		std::size_t hash() const;
		std::vector<boxed_node>& children();
		const std::vector<boxed_node>& children() const;
		node& evaluate_path(path_view_t path);
		const node& evaluate_path(path_view_t path) const;
		node& evaluate_parent(path_view_t path);
		const node& evaluate_parent(path_view_t path) const;
		void iterate(std::function<void(path_view_t, node&)> callback, bool recursive = false, path_t impl_path_dont_touch = {});
		void iterate(std::function<void(path_view_t, const node&)> callback, bool recursive = false, path_t impl_path_dont_touch = {}) const;
		srcloc& loc();
		const srcloc& loc() const;

		void pretty_print() const;
	};

	#define NODE_IS(some_node, node_type) (static_cast<syntax::node>(some_node).hash() == syntax::node{.payload = syntax::node_type{}}.hash())
	#define NODE_AS(some_node, node_type) std::get<syntax::node_type>(static_cast<syntax::node>(some_node).payload)

	node make_node(const lex::token& t);
}

#endif // PSYC_AST2_HPP