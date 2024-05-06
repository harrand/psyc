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

		bool reduce_from_integer_literal();
		bool reduce_from_decimal_literal();
		bool reduce_from_identifier();
		bool reduce_from_token();

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

		void do_reduce(std::size_t subtrees_offset, std::size_t count_to_remove, subtree replacement)
		{
			this->subtrees.erase(this->subtrees.begin() + subtrees_offset, this->subtrees.begin() + subtrees_offset + count_to_remove);
			this->subtrees.insert(this->subtrees.begin() + subtrees_offset, replacement);
		}

		std::deque<subtree> subtrees = {};
	};

	bool parser_state::reduce_from_integer_literal()
	{
		subtree atom = this->subtrees.front();
		diag::assert_that(atom.tree != ast{}, error_code::ice, "{} called but atom is not even an ast payload; its a token: \"{}\"", __FUNCTION__, atom.tok.lexeme);
		const ast::node& node = atom.tree.root;
		diag::assert_that(std::holds_alternative<ast::integer_literal>(node.payload), error_code::ice, "{} called but atom node payload is not correct. variant id: {}", __FUNCTION__, node.payload.index());
		auto value = std::get<ast::integer_literal>(node.payload);

		// easy option: it's directly followed by a semicolon. thats an expression.
		if(this->subtrees.size() > 2 && this->subtrees[1].tok.t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			this->do_reduce(0, 2, subtree
			{.tree ={.root = ast::node{
				.payload = ast::expression{.expr = value},
				.meta = atom.tree.root.meta
			}}});
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_decimal_literal()
	{
		subtree atom = this->subtrees.front();
		diag::assert_that(atom.tree != ast{}, error_code::ice, "{} called but atom is not even an ast payload; its a token: \"{}\"", __FUNCTION__, atom.tok.lexeme);
		const ast::node& node = atom.tree.root;
		diag::assert_that(std::holds_alternative<ast::decimal_literal>(node.payload), error_code::ice, "{} called but atom node payload is not correct. variant id: {}", __FUNCTION__, node.payload.index());
		auto value = std::get<ast::decimal_literal>(node.payload);

		if(this->subtrees.size() > 2 && this->subtrees[1].tok.t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			this->do_reduce(0, 2, subtree
			{.tree ={.root = ast::node{
				.payload = ast::expression{.expr = value},
				.meta = atom.tree.root.meta
			}}});
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_identifier()
	{
		subtree atom = this->subtrees.front();
		diag::assert_that(atom.tree != ast{}, error_code::ice, "{} called but atom is not even an ast payload; its a token: \"{}\"", __FUNCTION__, atom.tok.lexeme);
		const ast::node& node = atom.tree.root;
		diag::assert_that(std::holds_alternative<ast::identifier>(node.payload), error_code::ice, "{} called but atom node payload is not correct. variant id: {}", __FUNCTION__, node.payload.index());
		auto value = std::get<ast::identifier>(node.payload);

		if(this->subtrees.size() > 2 && this->subtrees[1].tok.t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			this->do_reduce(0, 2, subtree
			{.tree ={.root = ast::node{
				.payload = ast::expression{.expr = value},
				.meta = atom.tree.root.meta
			}}});
			return true;
		}
		
		return false;
	}

	bool parser_state::reduce_from_token()
	{
		subtree atom = this->subtrees.front();
		diag::assert_that(atom.tree == ast{}, error_code::ice, "{} called but atom appears to be an ast payload instead of a token. payload variant id: {}", __FUNCTION__, atom.tree.root.payload.index());
		switch(atom.tok.t)
		{
			case lex::type::integer_literal:
			{
				std::int64_t val = std::stoull(atom.tok.lexeme);
				this->do_reduce(0, 1, subtree
				{
					.tree = ast{.root = ast::node
					{
						.payload = ast::integer_literal{.val = val},
						.meta = atom.tok.meta_srcloc
					}}
				});
			}
			break;
			case lex::type::decimal_literal:
			{
				double val = std::stod(atom.tok.lexeme);
				this->do_reduce(0, 1, subtree
				{
					.tree = ast{.root = ast::node
					{
						.payload = ast::decimal_literal{.val = val},
						.meta = atom.tok.meta_srcloc
					}}
				});
			}
			break;
			case lex::type::identifier:
			{
				std::string val = atom.tok.lexeme;
				this->do_reduce(0, 1, subtree
				{
					.tree = ast{.root = ast::node
					{
						.payload = ast::identifier{.iden = val},
						.meta = atom.tok.meta_srcloc
					}}
				});
			}
			break;
			default:
				diag::error(error_code::syntax, "at {}, invalid token \"{}\"", atom.tok.meta_srcloc.to_string(), atom.tok.lexeme);
			break;
		}
		return false;
	}

	// advance the tokidx and create a new subtree from the token.
	void parser_state::shift()
	{
		const lex::token& tok = this->tokens[this->tokidx];
		this->subtrees.push_back({.tok = tok});
		this->tokidx++;
	}

	bool parser_state::reduce()
	{
		// try to combine any subtrees if it makes sense to do so.
		if(this->subtrees.empty())
		{
			return false;
		}

		subtree cur = this->subtrees.front();
		bool ret = false;
		if(cur.tree != ast{})
		{
			// its something.
			std::visit(util::overload
			{
				[&](ast::integer_literal arg)
				{
					ret = this->reduce_from_integer_literal();
				},
				[&](ast::decimal_literal arg)
				{
					ret = this->reduce_from_decimal_literal();
				},
				[&](ast::identifier arg)
				{
					ret = this->reduce_from_identifier();
				},
				[this, node = cur.tree.root](auto arg)
				{
					this->node_internal_assert(node, false, std::format("don't know how to reduce from the provided non-token atom. variant id: {}", node.payload.index()));
				}
			}, cur.tree.root.payload);
		}
		else
		{
			ret = this->reduce_from_token();
		}
		return ret;
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

		return ret;
	}

	ast tokens(lex::const_token_view toks)
	{
		parser_state state;
		state.tokens = toks;
		return state.parse();
	}
}