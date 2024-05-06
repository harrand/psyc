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

		bool reduce_from_integer_literal(std::size_t offset);
		bool reduce_from_decimal_literal(std::size_t offset);
		bool reduce_from_identifier(std::size_t offset);
		bool reduce_from_expression(std::size_t offset);
		bool reduce_from_token(std::size_t offset);

		ast parse();

		void node_assert(const ast::node& node, bool expr, std::string msg)
		{
			diag::assert_that(expr, error_code::syntax, "at {}: {}", node.meta.to_string(), msg);
		}
		void node_internal_assert(const ast::node& node, bool expr, std::string msg)
		{
			diag::assert_that(expr, error_code::ice, "at {}: {}", node.meta.to_string(), msg);
		}

		struct retriever
		{
			retriever(parser_state& state, std::size_t initial_offset):
			state(state), initial_offset(initial_offset), cursor(0)
			{}

			std::size_t get_offset() const
			{
				return this->initial_offset + cursor;
			}

			template<typename T>
			T must_retrieve(srcloc* loc = nullptr)
			{
				std::optional<T> maybe_value = this->retrieve<T>(loc);
				diag::assert_that(maybe_value.has_value(), error_code::ice, "ruh roh");
				return maybe_value.value();
			}

			template<typename T>
			std::optional<T> retrieve(srcloc* loc = nullptr)
			{
				subtree atom = state.subtrees[get_offset()];
				this->cursor++;
				if constexpr(std::is_same_v<std::decay_t<T>, lex::token>)
				{
					if(atom.tree != ast{})
					{
						return std::nullopt;
					}
					if(loc != nullptr)
					{
						*loc = atom.tok.meta_srcloc;
					}
					return atom.tok;
				}
				else
				{
					if(atom.tree == ast{})
					{
						return std::nullopt;
					}
					const ast::node& node = atom.tree.root;
					if(!std::holds_alternative<T>(node.payload))
					{
						return std::nullopt;
					}
					if(loc != nullptr)
					{
						*loc = node.meta;
					}
					return std::get<T>(node.payload);
				}
			}

			bool avail() const
			{
				return this->state.subtrees.size() > this->get_offset();
			}

			template<typename T>
			void reduce_to(T t, srcloc meta)
			{
				diag::assert_that(this->cursor > 0, error_code::ice, "cannot reduce_to because you never retrieved anything previously using this object.");
				state.do_reduce(this->initial_offset, this->cursor, subtree
				{
					.tree = {.root = ast::node
					{
						.payload = t,
						.meta = meta
					}}
				});
			}

			parser_state& state;
			std::size_t initial_offset;
			std::size_t cursor;
		};

		void shift();
		bool reduce();

		void do_reduce(std::size_t subtrees_offset, std::size_t count_to_remove, subtree replacement)
		{
			this->subtrees.erase(this->subtrees.begin() + subtrees_offset, this->subtrees.begin() + subtrees_offset + count_to_remove);
			this->subtrees.insert(this->subtrees.begin() + subtrees_offset, replacement);
		}

		std::deque<subtree> subtrees = {};
	};

	bool parser_state::reduce_from_integer_literal(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::integer_literal>(&meta);

		if(!retr.avail()){return false;}
		// easy option: it's directly followed by a semicolon. thats an expression.
		auto semicolon = retr.retrieve<lex::token>();
		if(semicolon.has_value() && semicolon->t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			retr.reduce_to(ast::expression{.expr = value}, meta);
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_decimal_literal(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::decimal_literal>(&meta);

		if(!retr.avail()){return false;}
		// easy option: it's directly followed by a semicolon. thats an expression.
		auto semicolon = retr.retrieve<lex::token>();
		if(semicolon.has_value() && semicolon->t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			retr.reduce_to(ast::expression{.expr = value}, meta);
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_identifier(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::identifier>(&meta);

		if(!retr.avail()){return false;}
		// easy option: it's directly followed by a semicolon. thats an expression.
		auto semicolon = retr.retrieve<lex::token>();
		if(semicolon.has_value() && semicolon->t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			retr.reduce_to(ast::expression{.expr = value}, meta);
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_expression(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::expression>(&meta);

		return false;
	}

	bool parser_state::reduce_from_token(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<lex::token>(&meta);
		switch(value.t)
		{
			case lex::type::integer_literal:
			{
				std::int64_t val = std::stoull(value.lexeme);
				retr.reduce_to(ast::integer_literal{.val = val}, meta);
			}
			break;
			case lex::type::decimal_literal:
			{
				double val = std::stod(value.lexeme);
				retr.reduce_to(ast::decimal_literal{.val = val}, meta);
			}
			break;
			case lex::type::identifier:
			{
				std::string val = value.lexeme;
				retr.reduce_to(ast::identifier{.iden = val}, meta);
			}
			break;
			default:
				diag::error(error_code::syntax, "at {}, invalid token \"{}\"", meta.to_string(), value.lexeme);
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

		bool ret = true;
		for(std::size_t i = 0; i < this->subtrees.size(); i++)
		{
			if(this->subtrees[i].tree != ast{})
			{
				// its something.
				std::visit(util::overload
				{
					[&](ast::integer_literal arg)
					{
						ret = this->reduce_from_integer_literal(i);
					},
					[&](ast::decimal_literal arg)
					{
						ret = this->reduce_from_decimal_literal(i);
					},
					[&](ast::identifier arg)
					{
						ret = this->reduce_from_identifier(i);
					},
					[&](ast::expression arg)
					{
						ret = this->reduce_from_expression(i);
					},
					[this, node = this->subtrees[i].tree.root](auto arg)
					{
						this->node_internal_assert(node, false, std::format("don't know how to reduce from the provided non-token atom. variant id: {}", node.payload.index()));
					}
				}, this->subtrees[i].tree.root.payload);
			}
			else
			{
				ret = this->reduce_from_token(i);
			}
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