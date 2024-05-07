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
		ast final_tree;

		bool reduce_from_integer_literal(std::size_t offset);
		bool reduce_from_decimal_literal(std::size_t offset);
		bool reduce_from_identifier(std::size_t offset);
		bool reduce_from_variable_declaration(std::size_t offset);
		bool reduce_from_expression(std::size_t offset);
		bool reduce_from_function_definition(std::size_t offset);
		bool reduce_from_token(std::size_t offset);

		void move_to_final_tree(std::size_t offset)
		{
			ast::path_t path = {this->final_tree.root.children.size()};
			this->final_tree.root.children.push_back({});
			this->subtrees[offset].tree.attach_to(this->final_tree, path);
			this->subtrees.erase(this->subtrees.begin() + offset);
		}

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

			void undo()
			{
				diag::assert_that(this->cursor > 0, error_code::ice, "fooey");
				this->cursor--;
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
		// not a semicolon.
		retr.undo();

		if(!retr.avail()){return false;}
		auto colon1 = retr.retrieve<lex::token>();
		if(colon1.has_value() && colon1->t == lex::type::colon)
		{
			if(!retr.avail()){return false;}
			// could be a:
			// - variable declaration (identifier)
			auto type_name = retr.retrieve<ast::identifier>();
			if(type_name.has_value())
			{
				// definitely a variable declaration.
				// todo: reduce.
				retr.reduce_to(ast::variable_declaration{.var_name = value.iden, .type_name = type_name.value().iden}, meta);
				return true;
			}
			// - function definition (2nd colon)
			retr.undo();
			if(!retr.avail()){return false;}
			auto colon2 = retr.retrieve<lex::token>();
			if(colon2.has_value() && colon2->t == lex::type::colon)
			{
				// probably. keep going to check function definition
				// fnname :: (par1 : ty1, par2 : ty2) -> retty
				//           ^ we are on this bit.
				if(!retr.avail()){return false;}
				auto open_paren = retr.retrieve<lex::token>();
				if(!open_paren.has_value() || open_paren->t != lex::type::open_paren)
				{
					return false;
				}
				if(!retr.avail()){return false;}
				std::vector<ast::variable_declaration> params = {};
				std::optional<lex::token> close_paren = std::nullopt;
				// keep trying to parse the close paren.
				while((close_paren = retr.retrieve<lex::token>(), !close_paren.has_value() || close_paren->t != lex::type::close_paren))
				{
					// everytime we fail, try to parse a variable declaration instead.
					retr.undo();
					auto cur_param = retr.retrieve<ast::variable_declaration>();
					// neither a close paren or a variable decl. cant parse this.
					if(!cur_param.has_value())
					{
						return false;
					}
					params.push_back(cur_param.value());
					// make sure there's room for the next subtree.
					if(!retr.avail()){return false;}
				}
				// we finally parsed the close paren. now we continue parsing the function def.
				// fnname :: (par1 : ty1, par2 : ty2) -> retty
				//                                    ^ we are on this bit.
				if(!retr.avail()){return false;}
				auto ret_arrow = retr.retrieve<lex::token>();
				if(!ret_arrow.has_value() || ret_arrow->t != lex::type::arrow_forward)
				{
					return false;
				}
				if(!retr.avail()){return false;}
				auto return_ty = retr.retrieve<ast::identifier>();
				if(!return_ty.has_value())
				{
					return false;
				}
				if(!retr.avail()){return false;}
				// ok we have another branch here. either:
				// fnname :: (par1 : ty1, par2 : ty2) -> retty = extern;
				// or
				// fnname :: (par1 : ty1, par2 : ty2) -> retty {...}
				// the former is real easy. lets do that now.
				auto equals = retr.retrieve<lex::token>();
				if(equals.has_value() && equals->t == lex::type::operator_equals)
				{
					// better see extern and semicolon.
					if(!retr.avail()){return false;}
					auto extern_specifier = retr.retrieve<ast::identifier>();
					if(!extern_specifier.has_value() || extern_specifier->iden != "extern")
					{
						return false;
					}
					if(!retr.avail()){return false;}
					auto semicolon = retr.retrieve<lex::token>();
					if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
					{
						return false;
					}
					// definitely an extern function definition.
					retr.reduce_to(ast::function_definition{.func_name = value.iden, .params = params, .ret_type = return_ty->iden, .is_extern = true}, meta);
					return true;
				}
				retr.undo();
				// should be a block.
				diag::error(error_code::nyi, "functions with implementation blocks are not yet implemented.");
				return true;	
			}
			// can't parse any further.
			return false;
		}
		retr.undo();
		//if(!retr.avail()){return false;}
		// todo: more shit if the next token isn't a colon.

		return false;
	}

	bool parser_state::reduce_from_variable_declaration(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::variable_declaration>(&meta);

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

		// this is the highest-level it gets. push to final tree.
		this->move_to_final_tree(offset);

		return false;
	}

	bool parser_state::reduce_from_function_definition(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::function_definition>(&meta);

		// this is the highest-level it gets. push to final tree.
		this->move_to_final_tree(offset);

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
				return true;
			}
			break;
			case lex::type::decimal_literal:
			{
				double val = std::stod(value.lexeme);
				retr.reduce_to(ast::decimal_literal{.val = val}, meta);
				return true;
			}
			break;
			case lex::type::identifier:
			{
				std::string val = value.lexeme;
				retr.reduce_to(ast::identifier{.iden = val}, meta);
				return true;
			}
			break;
			default:
				return false;
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
					[&](ast::variable_declaration arg)
					{
						ret = this->reduce_from_variable_declaration(i);
					},
					[&](ast::expression arg)
					{
						ret = this->reduce_from_expression(i);
					},
					[&](ast::function_definition arg)
					{
						ret = this->reduce_from_function_definition(i);
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
		this->final_tree = {.root = {}};
		this->tokidx = 0;

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
				/*
				ast::path_t path = {ret.root.children.size()};
				ret.root.children.push_back({});
				subtree.tree.attach_to(ret, path);
				*/
			}
		}
		if(error_count > 0)
		{
			diag::note("ast so far:\n");
			this->final_tree.pretty_print();
		}
		diag::assert_that(error_count == 0, error_code::syntax, "{} unparsed token errors - see above", error_count);

		return this->final_tree;
	}

	ast tokens(lex::const_token_view toks)
	{
		parser_state state;
		state.tokens = toks;
		return state.parse();
	}
}