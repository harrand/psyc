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

		// given an ast::integer_literal subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_integer_literal(std::size_t offset);
		// given an ast::decimal_literal subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_decimal_literal(std::size_t offset);
		// given an ast::identifier subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_identifier(std::size_t offset);
		// given an ast::variable_declaration subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_variable_declaration(std::size_t offset);
		// given an ast::function_call subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_function_call(std::size_t offset);
		// given an ast::expression subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_expression(std::size_t offset);
		// given an ast::function_definition subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_function_definition(std::size_t offset);
		// given an ast::block subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_block(std::size_t offset);
		// given a non-ast token at the offset, try to reduce it and its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_token(std::size_t offset);

		// given an atom at the given offset. add it as a new child of the final tree. you should only do this if you're certain the atom will undergo no more reductions.
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
			diag::assert_that(expr, error_code::parse, "at {}: {}", node.meta.to_string(), msg);
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
			T must_retrieve(srcloc* loc = nullptr, ast::node* output_node = nullptr)
			{
				std::optional<T> maybe_value = this->retrieve<T>(loc, output_node);
				diag::assert_that(maybe_value.has_value(), error_code::ice, "ruh roh");
				return maybe_value.value();
			}

			template<typename T>
			std::optional<T> retrieve(srcloc* loc = nullptr, ast::node* output_node = nullptr)
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
					if(output_node != nullptr)
					{
						*output_node = node;
					}
					return std::get<T>(node.payload);
				}
			}

			std::optional<ast::expression> retrieve_as_expression(srcloc* loc = nullptr, ast::node* output_node = nullptr)
			{
				auto maybe_expr = this->retrieve<ast::expression>(loc, output_node);
				if(!maybe_expr.has_value())
				{
					this->undo();
					// not an expression.
					// try:
					// - identifier
					auto maybe_iden = this->retrieve<ast::identifier>();
					if(maybe_iden.has_value())
					{
						maybe_expr = ast::expression{.expr = maybe_iden.value()};
						return maybe_expr;
					}
					this->undo();
					// - integer literal
					auto maybe_integer_literal = this->retrieve<ast::integer_literal>();
					if(maybe_integer_literal.has_value())
					{
						maybe_expr = ast::expression{.expr = maybe_integer_literal.value()};
						return maybe_expr;
					}
					this->undo();
					// - decimal literal
					auto maybe_decimal_literal = this->retrieve<ast::decimal_literal>();
					if(maybe_decimal_literal.has_value())
					{
						maybe_expr = ast::expression{.expr = maybe_decimal_literal.value()};
						return maybe_expr;
					}
				}
				return maybe_expr;
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

		// integer literals get reduced to expressions.
		// if there is a leading semicolon - we eat it.
		if(retr.avail())
		{
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
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

		// integer literals get reduced to expressions.
		// if there is a leading semicolon - we eat it.
		if(retr.avail())
		{
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
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
					if(!params.empty())
					{
						// need a comma.
						auto comma = retr.retrieve<lex::token>();
						if(!comma.has_value() || comma->t != lex::type::comma)
						{
							return false;
						}
						if(!retr.avail()){return false;}
					}
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
				ast::node blk_node;
				auto blk = retr.retrieve<ast::block>(nullptr, &blk_node);
				if(!blk.has_value())
				{
					return false;
				}
				retr.reduce_to(ast::function_definition{.func_name = value.iden, .params = params, .ret_type = return_ty->iden, .is_extern = false}, meta);
				ast::node& node = this->subtrees[offset].tree.root;
				node.children = {blk_node};
				// made our function definition with the block as its only child. that block should have its contents as child nodes.
				return true;	
			}
			// can't parse any further.
			return false;
		}
		retr.undo();
		// not a colon either. how about a function call?
		auto open_paren = retr.retrieve<lex::token>();
		volatile bool check = value.iden == "putchar" && retr.get_offset() + 4 < this->subtrees.size();
		if(open_paren.has_value() && open_paren->t == lex::type::open_paren)
		{
			if(!retr.avail()){return false;}
			std::vector<ast::boxed_expression> params = {};
			std::optional<lex::token> close_paren = std::nullopt;
			// keep trying to parse the close paren.
			while((close_paren = retr.retrieve<lex::token>(), !close_paren.has_value() || close_paren->t != lex::type::close_paren))
			{
				// everytime we fail, try to parse a variable declaration instead.
				retr.undo();
				if(!params.empty())
				{
					// need a comma.
					auto comma = retr.retrieve<lex::token>();
					if(!comma.has_value() || comma->t != lex::type::comma)
					{
						return false;
					}
					if(!retr.avail()){return false;}
				}
				// function call parameters can either be:
				auto cur_param = retr.retrieve_as_expression();
				// an expression
				if(!cur_param.has_value())
				{
					return false;
				}
				params.push_back(cur_param.value());
				// make sure there's room for the next subtree.
				if(!retr.avail()){return false;}
			}
			retr.reduce_to(ast::function_call{.function_name = value.iden, .params = params}, meta);
			return true;
		}
		retr.undo();

		// screw you then, assume its an expression.
		auto semicolon = retr.retrieve<lex::token>();
		if(semicolon.has_value() && semicolon->t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			retr.reduce_to(ast::expression{.expr = value}, meta);
			return true;
		}
		// not a semicolon.
		retr.undo();

		// todo: keep going.
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

		// as expressions can either be within a block or standalone, the only way we know for sure its standalone is if its the very first subtree.
		if(offset == 0)
		{
			this->move_to_final_tree(offset);
		}

		return false;
	}

	bool parser_state::reduce_from_function_call(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::function_call>(&meta);

		if(retr.avail())
		{
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value}, meta);
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_function_definition(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		ast::node func_node;
		auto value = retr.must_retrieve<ast::function_definition>(&meta, &func_node);

		// this is the highest-level it gets. push to final tree.
		// however, function definitions *must* only be standalone and not a part of a block for example.
		// for that reason, if its not the very first subtree, we can't move it.

		// note for future: you're probably tearing your hair out implementing methods which means functions *can* be defined beyond top-level scope.
		// in which case, you'll have to `if(offset == 0) move_to_final_tree` and then struct reduction should check for a function definition.
		// however, in the case of a genuinely poorly-placed function definition, you've now fucked up the error messages, coz a bunch of shit won't ever parse coz this stays in the subtree list forever.
		// im afraid thats going to be a bit of a PITA. good luck!
		this->node_assert(func_node, offset == 0, std::format("function \"{}\" is not defined at the top-level scope. all functions must be defined there.", value.func_name));
		this->move_to_final_tree(offset);
		/*
		if(offset == 0)
		{
			this->move_to_final_tree(offset);
		}
		*/

		return false;
	}

	bool parser_state::reduce_from_block(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::block>(&meta);

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
			case lex::type::open_brace:
			{
				// this could be a block.	
				if(!retr.avail()){return false;}
				std::optional<lex::token> close_brace = std::nullopt;
				std::vector<ast::node> expression_nodes = {};
				// keep trying to parse the close brace.
				while((close_brace = retr.retrieve<lex::token>(), !close_brace.has_value() || close_brace->t != lex::type::close_brace))
				{
					retr.undo();
					srcloc expr_loc;
					auto an_expr = retr.retrieve<ast::expression>(&expr_loc);
					if(!an_expr.has_value())
					{
						return false;
					}
					expression_nodes.push_back(ast::node
					{
						.payload = an_expr.value(),
						.meta = expr_loc,
						.children = {},
					});
					if(!retr.avail()){return false;}
				}
				// we got our block.
				retr.reduce_to(ast::block{}, meta);
				auto& result = this->subtrees[offset];
				diag::assert_that(std::holds_alternative<ast::block>(result.tree.root.payload), error_code::ice, "failed to retrieve block spawned at {} even though i literally just created it.", meta.to_string());
				for(const auto& node : expression_nodes)
				{
					result.tree.root.children.push_back(node);
				}
				return true;
			}
			default:
				return false;
			break;
		}
		return false;
	}

	// advance the tokidx and create a new subtree from the token.
	void parser_state::shift()
	{
		// skip over all comments.
		while(this->tokens[this->tokidx].t == lex::type::line_comment)
		{
			if(++this->tokidx >= this->tokens.size())
			{
				// we ran out of shiftin'
				return;
			}
		}
		const lex::token& tok = this->tokens[this->tokidx];
		// note: don't put line comments in the subtree list. as far as the AST exists comments are completely invisible.
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
					[&](ast::function_call arg)
					{
						ret = this->reduce_from_function_call(i);
					},
					[&](ast::expression arg)
					{
						ret = this->reduce_from_expression(i);
					},
					[&](ast::function_definition arg)
					{
						ret = this->reduce_from_function_definition(i);
					},
					[&](ast::block arg)
					{
						ret = this->reduce_from_block(i);
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
			// subtrees remaining means parse error.
			if(error_count == 0)
			{
				// if this is the first error, print the ast out now.
				diag::note("ast so far:\n");
				this->final_tree.pretty_print();
			}
			// tell them how shit they are.
			error_count++;
			if(subtree.tree == ast{})
			{
				diag::error_nonblocking(error_code::parse, "unexpected token(s) at {}: \"{}\"", subtree.tok.meta_srcloc.to_string(), subtree.tok.lexeme);
			}
			else
			{
				diag::error_nonblocking(error_code::parse, "unexpected atom(s) at {}: \"{}\"", subtree.tree.root.meta.to_string(), subtree.tree.root.to_string());
			}
		}
		diag::assert_that(error_count == 0, error_code::parse, "{} unparsed token/atom errors - see above", error_count);

		return this->final_tree;
	}

	ast tokens(lex::const_token_view toks)
	{
		parser_state state;
		state.tokens = toks;
		return state.parse();
	}
}