#include "parse.hpp"
#include "lex.hpp"
#include "diag.hpp"
#include <deque>
#include <vector>

/*
	what i've tried to build here is a bottom-up parser.
	it's not currently clear whether:
	- this really is a bottom-up parser (i think so) or an obfuscated recursive-descent parser.
	- whether this is a weird shift-reduce parser or more specific (LR(k))
*/

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

		bool try_reduce_variable_declaration(std::size_t offset, int* subtree_size_change = nullptr);
		bool try_reduce_function_definition(std::size_t offset, int* subtree_size_change = nullptr);
		bool try_reduce_expression(std::size_t offset, int* subtree_size_change = nullptr);

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

		void shift_front()
		{
			diag::assert_that(this->subtrees.size(), error_code::ice, "try to shift a subtree from the front when the list of subtrees was empty");
			this->subtrees_next_level.push_back(this->subtrees.front());
			this->subtrees.pop_front();
		}

		void next_level()
		{
			diag::assert_that(this->subtrees.empty(), error_code::ice, "try to go to next level when there are still {} unresolved subtrees", this->subtrees.size());
			this->subtrees = this->subtrees_next_level;
			this->subtrees_next_level.clear();
		}

		std::deque<subtree> subtrees = {};
		std::deque<subtree> subtrees_next_level = {};
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

	bool parser_state::try_reduce_variable_declaration(std::size_t offset, int* subtree_size_change)
	{
		// varname : ty = initialiser;
		// `= initialiser` is optional.
		ast::variable_declaration decl;

		// varname
		if(this->subtrees.size() < 3 + offset)
		{
			return false;
		}
		subtree var_name = this->subtrees[offset + 0];
		if(var_name.tree == ast{} || !std::holds_alternative<ast::identifier>(var_name.tree.root.payload))
		{
			return false;
		}
		decl.var_name = std::get<ast::identifier>(var_name.tree.root.payload).iden;

		// :
		subtree colon = this->subtrees[offset + 1];
		if(colon.tree != ast{} || colon.tok.t != lex::type::colon)
		{
			return false;
		}

		// typename
		subtree type_name = this->subtrees[offset + 2];
		if(type_name.tree == ast{} || !std::holds_alternative<ast::identifier>(type_name.tree.root.payload))
		{
			return false;
		}
		decl.type_name = std::get<ast::identifier>(type_name.tree.root.payload).iden;
		int sz = this->subtrees.size();
		this->subtrees.erase(this->subtrees.begin() + offset, this->subtrees.begin() + offset + 3);
		this->subtrees.insert(this->subtrees.begin() + offset,{.tree = ast
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
		if(subtree_size_change != nullptr)
		{
			*subtree_size_change = sz - this->subtrees.size();
		}
		return true;
	}

	bool parser_state::try_reduce_function_definition(std::size_t offset, int* subtree_size_change)
	{
		// funcname :: (par1 : par1ty, par2 : par2ty) -> retty
		if(this->subtrees.size() <= 10 + offset)
		{
			return false;
		}
		ast::function_definition def;
		subtree funcname = this->subtrees[offset + 0];
		if(funcname.tree == ast{} || !std::holds_alternative<ast::identifier>(funcname.tree.root.payload))
		{
			return false;
		}
		def.func_name = std::get<ast::identifier>(funcname.tree.root.payload).iden;
		subtree colon1 = this->subtrees[offset + 1];
		if(colon1.tok.t != lex::type::colon)
		{
			return false;
		}
		subtree colon2 = this->subtrees[offset + 2];
		if(colon2.tok.t != lex::type::colon)
		{
			return false;
		}
		subtree open_par = this->subtrees[offset + 3];
		if(open_par.tok.t != lex::type::open_paren)
		{
			return false;
		}
		std::vector<ast::variable_declaration> params = {};
		std::size_t id = offset + 4;
		bool terminated = false;
		while(id < this->subtrees.size())
		{
			// we're either looking for a variable declaration or a close paren.
			subtree val = this->subtrees[id];
			if(val.tok.t == lex::type::close_paren)
			{
				terminated = true;
				break;
			}
			else
			{
				// it should be a variable declaration.
				// if it isn't, try to reduce it to one.
				if(val.tree == ast{} || !std::holds_alternative<ast::variable_declaration>(val.tree.root.payload))
				{
					int sz_change = 0;
					if(!this->try_reduce_variable_declaration(id, &sz_change) || sz_change > 0)
					{
						return false;
					}
					// retrieve val as the subtree at this id should've changed.
					val = this->subtrees[id];
				}
				diag::assert_that(val.tree != ast{} && std::holds_alternative<ast::variable_declaration>(val.tree.root.payload), error_code::ice, "parser said it reduced a variable declaration, but token(s) {} (at {}) do not comprise a variable declaration", val.tree.root.meta.to_string(), val.tree.root.to_string());
				params.push_back(std::get<ast::variable_declaration>(val.tree.root.payload));
				//id -= sz_change;
			}
			id++;
		}
		if(!terminated) // never found the close_paren
		{
			return false;
		}
		def.params = params;
		subtree retty_arrow = this->subtrees[id + 1];
		if(retty_arrow.tok.t != lex::type::arrow_forward)
		{
			return false;
		}
		subtree retty = this->subtrees[id + 2];
		if(retty.tree == ast{} || !std::holds_alternative<ast::identifier>(retty.tree.root.payload))
		{
			return false;
		}
		def.ret_type = std::get<ast::identifier>(retty.tree.root.payload).iden;
		// either we now have a block, or an extern
		// for now we just assume extern.
		subtree equals = this->subtrees[id + 3];
		if(equals.tok.t != lex::type::operator_equals)
		{
			return false;
		}
		subtree extern_specifier = this->subtrees[id + 4];
		if(extern_specifier.tree == ast{} || !std::holds_alternative<ast::identifier>(extern_specifier.tree.root.payload))
		{
			return false;
		}
		if(std::get<ast::identifier>(extern_specifier.tree.root.payload).iden != "extern")
		{
			return false;
		}
		def.is_extern = true;
		subtree semicolon = this->subtrees[id + 5];
		if(semicolon.tok.t != lex::type::semicolon)
		{
			return false;
		}
		// todo: block
		int sz = this->subtrees.size();
		this->subtrees.erase(this->subtrees.begin() + offset, this->subtrees.begin() + id + 6);
		this->subtrees.insert(this->subtrees.begin() + offset, {.tree = ast
		{
			.root =
			{
				ast::node
				{
					.payload = def,
					.meta = funcname.tree.root.meta
				}
			}
		}});
		if(subtree_size_change != nullptr)
		{
			*subtree_size_change = sz - this->subtrees.size();
		}
		return true;
	}

	bool parser_state::try_reduce_expression(std::size_t offset, int* subtree_size_change)
	{
		if(this->subtrees.size() < 2 + offset)
		{
			return false;
		}
		subtree p0 = this->subtrees[offset + 0];
		if(std::holds_alternative<ast::integer_literal>(p0.tree.root.payload))
		{
			ast::node node = p0.tree.root;
			node.payload = ast::expression{.expr = std::get<ast::integer_literal>(node.payload)};
			// if it ends with a semicolon, we're done.
			if(this->subtrees[offset + 1].tok.t == lex::type::semicolon)
			{
				int sz = this->subtrees.size();
				this->subtrees.erase(this->subtrees.begin() + offset, this->subtrees.begin() + offset + 2);
				this->subtrees.insert(this->subtrees.begin() + offset, subtree{.tree = ast{.root = node}});
				if(subtree_size_change != nullptr)
				{
					*subtree_size_change = sz - this->subtrees.size();
				}
				return true;
			}
		}

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
		if(this->try_reduce_function_definition(0)
			|| this->try_reduce_variable_declaration(0)
			|| this->try_reduce_expression(0))
		{
			this->shift_front();
			return true;
		}
		return false;
	}

	ast parser_state::parse()
	{
		this->tokidx = 0;
		ast ret;
		ret.root = {};

		while(this->tokidx < this->tokens.size())
		{
			shift();
			while(this->reduce()){}
		}
		bool reduced_more = false;
		do
		{
			this->next_level();
			reduced_more = this->reduce();
		}while(reduced_more);
		// next level and go again (until when???)
		std::size_t error_count = 0;

		for(const auto& subtree : this->subtrees)
		{
			if(subtree.tree == ast{})
			{
				error_count++;
				diag::error_nonblocking(error_code::syntax, "unparsed token(s) at {}: \"{}\"", subtree.tok.meta_srcloc.to_string(), subtree.tok.lexeme);
			}
			else
			{
				ast::path_t path = {ret.root.children.size()};
				ret.root.children.push_back({});
				subtree.tree.attach_to(ret, path);
			}
		}
		if(error_count > 0)
		{
			diag::note("ast so far:\n");
			ret.pretty_print();
		}
		diag::assert_that(error_count == 0, error_code::syntax, "{} unparsed token errors - see above", error_count);

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

	ast tokens(lex::const_token_view toks)
	{
		parser_state state;
		state.tokens = toks;
		return state.parse();
	}
}