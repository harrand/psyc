#include "parse.hpp"
#include "lex.hpp"
#include "diag.hpp"
#include <deque>
#include <vector>

namespace parse
{
	struct subtree
	{
		ast tree = {};
		lex::token tok = {};
	};
	struct parser_state
	{
		std::size_t tokidx = 0;
		lex::const_token_view tokens;

		ast try_parse_integer_literal();
		ast try_parse_decimal_literal();
		ast try_parse_identifier();

		bool try_reduce_variable_declaration();
		bool try_reduce_function_definition();

		ast parse();

		void node_assert(const ast::node& node, bool expr, std::string msg)
		{
			diag::assert_that(expr, error_code::syntax, "at {}: {}", node.meta.to_string(), msg);
		}
		void node_internal_assert(const ast::node& node, bool expr, std::string msg)
		{
			diag::assert_that(expr, error_code::ice, "at {}: {}", node.meta.to_string(), msg);
		}

		void shift();
		bool reduce();

		std::deque<subtree> subtrees = {};
	};

	ast parser_state::try_parse_integer_literal()
	{
		const auto& tok = this->tokens[this->tokidx];
		if(tok.t == lex::type::integer_literal)
		{
			return ast
			{
				.root =
				{
					.payload = ast::integer_literal{.val = std::stoi(tok.lexeme)},
					.meta = tok.meta_srcloc
				}
			};
		}
		return {};
	}

	ast parser_state::try_parse_decimal_literal()
	{
		const auto& tok = this->tokens[this->tokidx];
		if(tok.t == lex::type::decimal_literal)
		{
			return ast
			{
				.root =
				{
					.payload = ast::decimal_literal{.val = std::stod(tok.lexeme)},
					.meta = tok.meta_srcloc
				}
			};
		}
		return {};
	}

	ast parser_state::try_parse_identifier()
	{
		const auto& tok = this->tokens[this->tokidx];
		if(tok.t == lex::type::identifier)
		{
			return ast
			{
				.root =
				{
					.payload = ast::identifier{.iden = tok.lexeme},
					.meta = tok.meta_srcloc,
				}
			};
		}
		return {};
	}

	bool parser_state::try_reduce_variable_declaration()
	{
		// varname : ty = initialiser;
		// `= initialiser` is optional.
		ast::variable_declaration decl;

		// varname
		if(this->subtrees.size() < 3)
		{
			return false;
		}
		subtree var_name = this->subtrees[0];
		if(var_name.tree == ast{} || !std::holds_alternative<ast::identifier>(var_name.tree.root.payload))
		{
			return false;
		}
		decl.var_name = std::get<ast::identifier>(var_name.tree.root.payload).iden;

		// :
		subtree colon = this->subtrees[1];
		if(colon.tree != ast{} || colon.tok.t != lex::type::colon)
		{
			return false;
		}

		// typename
		subtree type_name = this->subtrees[2];
		if(type_name.tree == ast{} || !std::holds_alternative<ast::identifier>(type_name.tree.root.payload))
		{
			return false;
		}
		decl.type_name = std::get<ast::identifier>(type_name.tree.root.payload).iden;
		this->subtrees.erase(this->subtrees.begin(), this->subtrees.begin() + 3);
		this->subtrees.push_front({.tree = ast
		{
			.root =
			{
				ast::node
				{
					.payload = decl,
					.meta = var_name.tree.root.meta,
					.children = {}
				}
			}	
		}});
		return true;
	}

	bool parser_state::try_reduce_function_definition()
	{
		/*
		// funcname :: (par1 : par1ty, par2 : par2ty) -> retty
		auto stack = this->subtrees;
		subtree funcname = stack.top(); stack.pop();
		// funcname should be a tree containing an identifier only.
		if(funcname.tree == ast{} || funcname.tree.root.children.size() || !std::holds_alternative<ast::identifier>(funcname.tree.root.payload))
		{
			return false;
		}
		*/
		return false;
	}

	// advance the tokidx and create a new subtree from the token if you can.
	void parser_state::shift()
	{
		const lex::token& tok = this->tokens[this->tokidx];
		// make new subtree from the bottom-most types.
		subtree sub;
		ast subtree_tree{};
		if(subtree_tree = this->try_parse_integer_literal(), subtree_tree != ast{}
		||(subtree_tree = this->try_parse_decimal_literal(), subtree_tree != ast{})
		||(subtree_tree = this->try_parse_identifier(), subtree_tree != ast{}))
		{
			this->subtrees.push_back({.tree = subtree_tree});
		}
		else
		{
			this->subtrees.push_back({.tok = tok});
		}
		this->tokidx++;
	}

	bool parser_state::reduce()
	{
		// try to combine any subtrees if it makes sense to do so.
		return this->try_reduce_variable_declaration()
			|| this->try_reduce_function_definition();
	}

	ast parser_state::parse()
	{
		this->tokidx = 0;
		ast ret;
		ret.root = {};

		while(this->tokidx < this->tokens.size())
		{
			while(this->reduce()){}
			shift();
		}

		for(const auto& subtree : this->subtrees)
		{
			if(subtree.tree == ast{})
			{
				diag::error(error_code::syntax, "unparsed token(s) at {}: \"{}\"", subtree.tok.meta_srcloc.to_string(), subtree.tok.lexeme);
			}
			ast::path_t path = {ret.root.children.size()};
			ret.root.children.push_back({});
			subtree.tree.attach_to(ret, path);
		}

		/*
		for(this->tokidx = 0; this->tokidx < this->tokens.size(); this->tokidx++)
		{
			ast subtree = {};
			if(subtree = this->try_parse_integer_literal(), subtree != ast{})
			{
				subtree = this->bottom_up_integer_literal(subtree);
			}
			else if(subtree = this->try_parse_decimal_literal(), subtree != ast{})
			{
				subtree = this->bottom_up_decimal_literal(subtree);
			}
			else if(subtree = this->try_parse_identifier(), subtree != ast{})
			{
				subtree = this->bottom_up_identifier(subtree);
			}
		}
		*/

		return ret;
	}
	// LR(0) - a bottom-up parser.
	ast tokens(lex::const_token_view toks)
	{
		parser_state state;
		state.tokens = toks;
		return state.parse();
	}
}