#include "parse.hpp"
#include "lex.hpp"
#include "diag.hpp"
#include <deque>
#include <functional>
#include <vector>

/*
	what i've tried to build here is a bottom-up parser.
	it's not currently clear whether:
	- this really is a bottom-up parser (i think so) or an obfuscated recursive-descent parser.
	- whether this is a weird shift-reduce parser or more specific (LR(k))
*/

namespace parse
{
	std::string escape(std::string_view literal)
	{
		std::string ret;
		static const std::unordered_map<std::string_view, char> escape_map = {
			{"\\0", '\0'}, // Null terminator
			{"\\a", '\a'}, // Bell (alert)
			{"\\b", '\b'}, // Backspace
			{"\\f", '\f'}, // Formfeed
			{"\\n", '\n'}, // Newline (line feed)
			{"\\r", '\r'}, // Carriage return
			{"\\t", '\t'}, // Horizontal tab
			{"\\v", '\v'}, // Vertical tab
			{"\\\\", '\\'}, // Backslash
			{"\\'", '\''}, // Single quote
			{"\\\"", '\"'}, // Double quote
			{"\\?", '\?'}  // Question mark
		};
		if(literal.size() == 1)
		{
			return std::string{literal};
		}
		for(std::size_t i = 0; i < literal.size(); i++)
		{
			std::string_view substr{literal.data() + i, 2};
			auto iter = escape_map.find(substr);
			if(iter != escape_map.end())
			{
				ret += iter->second;
				i++;
			}
			else
			{
				ret += literal[i];
			}
		}
		return ret;
	}

	bool token_is_unary_operator(const lex::token& t)
	{
		return t.t == lex::type::operator_minus
			|| t.t == lex::type::operator_plus
			|| t.t == lex::type::operator_ref
			|| t.t == lex::type::operator_deref
			|| t.t == lex::type::operator_defer
			|| t.t == lex::type::operator_sizeof;
	}

	bool token_is_binary_operator(const lex::token& t)
	{
		return t.t == lex::type::operator_minus
			|| t.t == lex::type::operator_plus
			|| t.t == lex::type::operator_asterisk
			|| t.t == lex::type::operator_slash
			|| t.t == lex::type::operator_equals
			|| t.t == lex::type::operator_double_equals
			|| t.t == lex::type::operator_notequals
			|| t.t == lex::type::operator_cast;
	}
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
		// given an ast::char_literal subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_char_literal(std::size_t offset);
		// given an ast::string_literal subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_string_literal(std::size_t offset);
		// given an ast::bool_literal subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_bool_literal(std::size_t offset);
		// given an ast::null_literal subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_null_literal(std::size_t offset);
		// given an ast::identifier subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_identifier(std::size_t offset);
		// given an ast::member_access subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_member_access(std::size_t offset);
		// given an ast::array_access subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_array_access(std::size_t offset);
		// given an ast::variable_declaration subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_variable_declaration(std::size_t offset);
		// given an ast::function_call subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_function_call(std::size_t offset);
		// given an ast::method_call subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_method_call(std::size_t offset);
		// given an ast::if_statement subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_if_statement(std::size_t offset);
		// given an ast::for_statement subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_for_statement(std::size_t offset);
		// given an ast::struct_initialiser subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_struct_initialiser(std::size_t offset);
		// given an ast::expression subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_expression(std::size_t offset);
		// given an ast::function_definition subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_function_definition(std::size_t offset);
		// given an ast::struct_definition subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_struct_definition(std::size_t offset);
		// given an ast::block subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_block(std::size_t offset);
		// given an ast::meta_region subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_meta_region(std::size_t offset);
		// given an ast::return_statement subtree at the offset, try to reduce its surrounding tokens/atoms into something bigger. returns true on success, false otherwise.
		bool reduce_from_return_statement(std::size_t offset);
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

		std::optional<ast::node> pop_from_final_tree_if(std::function<bool(const ast::node&)> predicate)
		{
			for(std::size_t i = 0; i < this->final_tree.root.children.size(); i++)
			{
				if(predicate(this->final_tree.root.children[i]))
				{
					ast::node cpy = this->final_tree.root.children[i];
					this->final_tree.root.children.erase(this->final_tree.root.children.begin() + i);
					return cpy;
				}
			}
			return std::nullopt;
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
					this->undo();
					// char literal
					auto maybe_char_literal = this->retrieve<ast::char_literal>();
					if(maybe_char_literal.has_value())
					{
						maybe_expr = ast::expression{.expr = maybe_char_literal.value()};
						return maybe_expr;
					}
					this->undo();
					// string literal
					auto maybe_string_literal = this->retrieve<ast::string_literal>();
					if(maybe_string_literal.has_value())
					{
						maybe_expr = ast::expression{.expr = maybe_string_literal.value()};
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
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
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
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
			return true;
		}
		return false;
	}

	bool parser_state::reduce_from_char_literal(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::char_literal>(&meta);

		// integer literals get reduced to expressions.
		// if there is a leading semicolon - we eat it.
		if(retr.avail())
		{
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
			return true;
		}
		return false;
	}

	bool parser_state::reduce_from_string_literal(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::string_literal>(&meta);

		// integer literals get reduced to expressions.
		// if there is a leading semicolon - we eat it.
		if(retr.avail())
		{
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
			return true;
		}
		return false;
	}

	bool parser_state::reduce_from_bool_literal(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::bool_literal>(&meta);

		// integer literals get reduced to expressions.
		// if there is a leading semicolon - we eat it.
		if(retr.avail())
		{
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
			return true;
		}
		return false;
	}

	bool parser_state::reduce_from_null_literal(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::null_literal>(&meta);

		// integer literals get reduced to expressions.
		// if there is a leading semicolon - we eat it.
		if(retr.avail())
		{
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
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
				// if it has an equals then its going to have an initialiser.
				std::optional<util::box<ast::expression>> initialiser = std::nullopt;
				if(!retr.avail()){return false;}
				auto maybe_initialiser_token = retr.retrieve<lex::token>();
				if(maybe_initialiser_token.has_value() && maybe_initialiser_token->t == lex::type::initialiser)
				{
					// it has an initialiser.
					if(!retr.avail()){return false;}
					auto maybe_initialiser_expr = retr.retrieve<ast::expression>();
					if(!maybe_initialiser_expr.has_value())
					{
						return false;
					}
					initialiser = maybe_initialiser_expr.value();
				}
				else
				{
					retr.undo();
				}
				retr.reduce_to(ast::variable_declaration{.var_name = value.iden, .type_name = type_name.value().iden, .initialiser = initialiser}, meta);
				return true;
			}
			// - function or struct definition (2nd colon)
			retr.undo();
			if(!retr.avail()){return false;}
			auto colon2 = retr.retrieve<lex::token>();
			if(colon2.has_value() && colon2->t == lex::type::colon)
			{
				// probably. keep going to check struct/function definition
				// fnname :: (par1 : ty1, par2 : ty2) -> retty
				//           ^ we are on this bit.
				if(!retr.avail()){return false;}
				auto open_paren = retr.retrieve<lex::token>();
				if(!open_paren.has_value() || open_paren->t != lex::type::open_paren)
				{
					if(open_paren.has_value() && open_paren->t == lex::type::keyword_struct)
					{
						// ah, so we're a struct.
						if(!retr.avail()){return false;}
						ast::node blk_node;
						auto blk = retr.retrieve<ast::block>(nullptr, &blk_node);
						if(!blk.has_value())
						{
							return false;
						}
						retr.reduce_to(ast::struct_definition{.name = value.iden}, meta);
						ast::node& node = this->subtrees[offset].tree.root;
						node.children = {blk_node};
						// made our struct definition with the block as its only child. that block should have its contents as child nodes.
						return true;	
					}
					// ok, not a function either.
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
				// fnname :: (par1 : ty1, par2 : ty2) -> retty extern;
				// or
				// fnname :: (par1 : ty1, par2 : ty2) -> retty {...}
				// the former is real easy. lets do that now.
				auto initialiser_token = retr.retrieve<lex::token>();
				if(initialiser_token.has_value() && initialiser_token->t == lex::type::initialiser)
				{
					if(!retr.avail()){return false;}
					auto extern_specifier = retr.retrieve<ast::identifier>();
					if(extern_specifier.has_value() && extern_specifier->iden == "extern")
					{
						// better see extern and semicolon.
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
					else
					{
						retr.undo();
					}
				}
				else
				{
					retr.undo();
				}
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
		auto dot = retr.retrieve<lex::token>();
		if(dot.has_value() && dot->t == lex::type::dot)
		{
			if(!retr.avail()){return false;}
			auto rhs_iden = retr.retrieve<ast::identifier>();
			if(!rhs_iden.has_value())
			{
				return false;
			}
			if(!retr.avail()){return false;}
			auto method_open = retr.retrieve<lex::token>();
			if(method_open.has_value() && method_open->t == lex::type::open_paren)
			{
				std::vector<ast::boxed_expression> params = {};
				std::optional<lex::token> close_paren = std::nullopt;
				// keep trying to parse the close paren.
				if(!retr.avail()){return false;}
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
				retr.reduce_to(ast::method_call{.lhs = ast::expression{.expr = value, .capped = false}, .function_name = rhs_iden->iden, .params = params}, meta);
				return true;
			}
			retr.undo();
			retr.reduce_to(ast::member_access{.lhs = ast::expression{.expr =value, .capped = false}, .rhs = rhs_iden->iden}, meta);
			return true;
		}
		retr.undo();
		// how about a designated initialiser.
		auto maybe_open_brace = retr.retrieve<lex::token>();
		if(maybe_open_brace.has_value() && maybe_open_brace->t == lex::type::open_brace)
		{
			std::size_t initial_idx = retr.get_offset();
			std::vector<std::pair<std::string, ast::boxed_expression>> designated_initialisers = {};
			std::optional<lex::token> close_brace = std::nullopt;
			// keep trying to parse the close paren.
			if(!retr.avail()){return false;}
			while((close_brace = retr.retrieve<lex::token>(), !close_brace.has_value() || close_brace->t != lex::type::close_brace))
			{
				retr.undo();
				if(!designated_initialisers.empty())
				{
					// need a comma.
					auto comma = retr.retrieve<lex::token>();
					if(!comma.has_value() || comma->t != lex::type::comma)
					{
						return false;
					}
					if(!retr.avail()){return false;}
				}

				// need a dot.
				auto dot = retr.retrieve<lex::token>();
				if(!dot.has_value() || dot->t != lex::type::dot)
				{
					return false;
				}

				// designator
				if(!retr.avail()){return false;}
				auto lhs = retr.retrieve<ast::identifier>();
				if(!lhs.has_value())
				{
					return false;
				}

				// :=
				if(!retr.avail()){return false;}
				auto initialiser = retr.retrieve<lex::token>();
				if(!initialiser.has_value() || initialiser->t != lex::type::initialiser)
				{
					return false;
				}

				// designatee
				if(!retr.avail()){return false;}
				auto expr = retr.retrieve<ast::expression>();
				if(!expr.has_value())
				{
					return false;
				}
				designated_initialisers.push_back({lhs->iden, expr.value()});
			}
			// we're done. note a struct initialiser must contain *at least one* designated initialiser, otherwise all blocks could in theory reduce to a struct initialiser due to braces being very context sentitive.
			if(designated_initialisers.size())
			{
				retr.reduce_to(ast::struct_initialiser{.name = value.iden, .designated_initialisers = designated_initialisers}, meta);
				return true;
			}
			while(retr.get_offset() > initial_idx)
			{
				retr.undo();
			}
		}
		retr.undo();

		// getting difficult here...
		// at some point we need to know whether an identifier should remain an identifier or become an expression
		// e.g given the following scenarios, `foo` should be identifier -> expression:
		//
		// foo;
		// -foo;
		// print(foo)
		// foo - foo;
		
		// however, under the following scenarios we do *not* want that reduction to happen:
		// func :: () -> foo {...}
		// foo()
		// in this case, `foo` must remain an identifier, otherwise the reductions of the actual constructs won't happen.

		// my hacky rule to get around this:
		// identifiers default to expressions iff they are directly proceeded by either: ')' or ';' or any binary operator char (e.g '+', '-')
		// in the semicolon case - the ; is consumed.
		// in all other cases, the extra token is *not* consumed.
		auto semicolon = retr.retrieve<lex::token>();
		if(semicolon.has_value() && semicolon->t == lex::type::semicolon)
		{
			// iden; -> expr
			retr.reduce_to(ast::expression{.expr = value, .capped = true}, meta);
			return true;
		}
		// not a semicolon.
		retr.undo();
		// how about a close paren (or an open brack coz that means array access and we want the lhs to be a full expression instead of an iden)
		auto close_paren = retr.retrieve<lex::token>();
		if(close_paren.has_value() && (close_paren->t == lex::type::close_paren || close_paren->t == lex::type::open_brack || close_paren->t == lex::type::close_brack))
		{
			// iden) -> expr (but do not consume the token)
			retr.undo();
			retr.reduce_to(ast::expression{.expr = value, .capped = false}, meta);
			return true;
		}
		retr.undo();
		// how about an operator.
		auto potential_operator = retr.retrieve<lex::token>();
		if(potential_operator.has_value() && token_is_binary_operator(potential_operator.value()))
		{
			// iden) -> expr (but do not consume the token)
			retr.undo();
			retr.reduce_to(ast::expression{.expr = value, .capped = false}, meta);
			return true;
		}
		// todo: other operators.

		// todo: keep going.
		return false;
	}

	bool parser_state::reduce_from_member_access(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::member_access>(&meta);

		if(!retr.avail()){return false;}
		bool capped = false;

		auto semicolon = retr.retrieve<lex::token>();
		capped = (semicolon.has_value() && semicolon->t == lex::type::semicolon);
		if(!capped)
		{
			retr.undo();
		}

		retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
		return true;
	}

	bool parser_state::reduce_from_array_access(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::array_access>(&meta);

		if(!retr.avail()){return false;}
		bool capped = false;

		auto semicolon = retr.retrieve<lex::token>();
		capped = (semicolon.has_value() && semicolon->t == lex::type::semicolon);
		if(!capped)
		{
			retr.undo();
		}

		retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
		return true;
	}

	bool parser_state::reduce_from_variable_declaration(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::variable_declaration>(&meta);

		// if the variable has a capped initialiser expression, it can go straight away.
		if(value.initialiser.has_value() && value.initialiser.value()->capped)
		{
			retr.reduce_to(ast::expression{.expr = value, .capped = true}, meta);
			return true;
		}

		if(!retr.avail()){return false;}
		// easy option: it's directly followed by a semicolon. thats an expression.
		auto semicolon = retr.retrieve<lex::token>();
		if(semicolon.has_value() && semicolon->t == lex::type::semicolon)
		{
			// this *is* an expression. happy days.
			retr.reduce_to(ast::expression{.expr = value, .capped = true}, meta);
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_if_statement(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::if_statement>(&meta);

		this->move_to_final_tree(offset);
		return false;
	}

	bool parser_state::reduce_from_for_statement(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::for_statement>(&meta);

		this->move_to_final_tree(offset);
		return false;
	}

	bool parser_state::reduce_from_struct_initialiser(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::struct_initialiser>(&meta);

		if(!retr.avail()){return false;}
		bool capped = false;

		auto semicolon = retr.retrieve<lex::token>();
		capped = (semicolon.has_value() && semicolon->t == lex::type::semicolon);
		if(!capped)
		{
			retr.undo();
		}

		retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
		return true;
	}

	bool parser_state::reduce_from_expression(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::expression>(&meta);

		if(retr.avail())
		{
			auto tok = retr.retrieve<lex::token>();
			if(tok.has_value())
			{
				if(token_is_binary_operator(tok.value()))
				{
					if(!retr.avail()){return false;}
					// unary -
					auto operand = retr.retrieve<ast::expression>();
					if(operand.has_value())
					{
						retr.reduce_to(ast::expression{.expr =
							ast::binary_operator
							{
								.lhs_expr = value,
								.op = tok.value(),
								.rhs_expr = operand.value(),
							}, .capped = operand.value().capped}, meta);
						return true;
					}
				}
				else if(tok->t == lex::type::dot)
				{
					// could be a member access.
					if(!retr.avail()){return false;}
					auto iden = retr.retrieve<ast::identifier>();
					if(!iden.has_value())
					{
						return false;
					}
					if(!retr.avail()){return false;}
					auto method_open = retr.retrieve<lex::token>();
					if(method_open.has_value() && method_open->t == lex::type::open_paren)
					{
						std::vector<ast::boxed_expression> params = {};
						std::optional<lex::token> close_paren = std::nullopt;
						// keep trying to parse the close paren.
						if(!retr.avail()){return false;}
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
						retr.reduce_to(ast::method_call{.lhs = value, .function_name = iden->iden, .params = params}, meta);
						return true;
					}
					retr.undo();
					retr.reduce_to(ast::member_access{.lhs = value, .rhs = iden->iden}, meta);
					return true;
				}
				else if(tok->t == lex::type::open_brack)
				{
					if(!retr.avail()){return false;}
					auto expr = retr.retrieve<ast::expression>();
					if(!expr.has_value())
					{
						return false;
					}
					if(!retr.avail()){return false;}
					auto close_brack = retr.retrieve<lex::token>();
					if(!close_brack.has_value() || close_brack->t != lex::type::close_brack)
					{
						return false;
					}
					retr.reduce_to(ast::array_access{.expr = value, .index = expr.value()}, meta);
				}
			}
		}
		// as expressions can either be within a block or standalone, the only way we know for sure its standalone is if its the very first subtree.
		if(value.capped)
		{
			this->move_to_final_tree(offset);
		}
		/*
		if(offset == 0)
		{
			this->move_to_final_tree(offset);
		}
		*/

		return false;
	}

	bool parser_state::reduce_from_function_call(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::function_call>(&meta);

		if(retr.avail())
		{
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
			return true;
		}

		return false;
	}

	bool parser_state::reduce_from_method_call(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::method_call>(&meta);

		if(retr.avail())
		{
			bool capped = true;
			auto semicolon = retr.retrieve<lex::token>();
			if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
			{
				capped = false;
				retr.undo();
			}
			// we got a semicolon, but we're not going to do anything (we're about to swallow it)
			retr.reduce_to(ast::expression{.expr = value, .capped = capped}, meta);
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

	bool parser_state::reduce_from_struct_definition(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		ast::node func_node;
		auto value = retr.must_retrieve<ast::struct_definition>(&meta, &func_node);

		// this is the highest-level it gets. push to final tree.
		// however, function definitions *must* only be standalone and not a part of a block for example.
		// for that reason, if its not the very first subtree, we can't move it.

		// note for future: you're probably tearing your hair out implementing methods which means functions *can* be defined beyond top-level scope.
		// in which case, you'll have to `if(offset == 0) move_to_final_tree` and then struct reduction should check for a function definition.
		// however, in the case of a genuinely poorly-placed function definition, you've now fucked up the error messages, coz a bunch of shit won't ever parse coz this stays in the subtree list forever.
		// im afraid thats going to be a bit of a PITA. good luck!
		this->node_assert(func_node, offset == 0, std::format("struct \"{}\" is not defined at the top-level scope. all structs must be defined there.", value.name));
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

		// block never reduces to anything.
		// it also returns false here, coz it stays put until something else uses it in a separate reduction.
		return false;
	}

	bool parser_state::reduce_from_meta_region(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::meta_region>(&meta);
		// meta regions never reduce to anything.
		// however, they do go straight into the final tree.
		this->move_to_final_tree(offset);

		return false;
	}

	bool parser_state::reduce_from_return_statement(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		auto value = retr.must_retrieve<ast::return_statement>(&meta);
		retr.reduce_to(ast::expression{.expr = value, .capped = true}, meta);
		return true;
	}

	bool parser_state::reduce_from_token(std::size_t offset)
	{
		retriever retr{*this, offset};
		srcloc meta;
		ast::node node;
		auto value = retr.must_retrieve<lex::token>(&meta, &node);
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
			case lex::type::char_literal:
			{
				std::string charlit = value.lexeme;
				this->node_assert(node, !charlit.empty(), "empty char-literal is invalid. must contain a single character.");
				charlit = escape(charlit);
				this->node_assert(node, charlit.size() == 1, std::format("char-literal must consist of 1 char, but \"{}\" contains {}", value.lexeme, charlit.size()));
				retr.reduce_to(ast::char_literal{.val = charlit.front()}, meta);
			}
			break;
			case lex::type::string_literal:
			{
				retr.reduce_to(ast::string_literal{.val = escape(value.lexeme)}, meta);
			}
			break;
			case lex::type::bool_literal:
			{
				bool val;
				if(value.lexeme == "true")
				{
					val = true;
				}
				else if(value.lexeme == "false")
				{
					val = false;	
				}
				else
				{
					diag::assert_that(false, error_code::ice, "unknown bool-literal \"{}\"", value.lexeme);
				}
				retr.reduce_to(ast::bool_literal{.val = val}, meta);
				return true;
			}
			break;
			case lex::type::null_literal:
				retr.reduce_to(ast::null_literal{}, meta);	
				return true;
			break;
			case lex::type::identifier:
			{
				std::string val = value.lexeme;
				retr.reduce_to(ast::identifier{.iden = val}, meta);
				return true;
			}
			break;
			case lex::type::keyword_if:
			{
				if(!retr.avail()){return false;}
				auto expr = retr.retrieve<ast::expression>();
				if(!expr.has_value())
				{
					return false;
				}
				if(!retr.avail()){return false;}
				ast::node blk_node;
				auto blk = retr.retrieve<ast::block>(nullptr, &blk_node);
				if(!blk.has_value())
				{
					return false;
				}
				std::optional<ast::node> else_node = std::nullopt;
				// check for else.
				if(!retr.avail()){return false;}
				auto maybe_else = retr.retrieve<lex::token>();
				if(maybe_else.has_value() && maybe_else->t == lex::type::keyword_else)
				{
					if(!retr.avail()){return false;}
					// block OR another if statement (else-if)
					ast::node blk2_node;
					auto blk2 = retr.retrieve<ast::block>(nullptr, &blk2_node);
					if(!blk2.has_value())
					{
						retr.undo();
						ast::node nested_if_node;
						auto nested_if = retr.retrieve<ast::if_statement>(nullptr, &nested_if_node);
						if(!nested_if.has_value())
						{
							return false;
						}
						nested_if_node.payload = ast::expression{.expr = nested_if.value()};
						blk2_node.payload = ast::block{};
						blk2_node.meta = nested_if_node.meta;
						blk2_node.children = {nested_if_node};
					}
					else_node = blk2_node;
				}
				else
				{
					retr.undo();
				}
				retr.reduce_to(ast::if_statement{.if_expr = expr.value()}, meta);
				ast::node& node = this->subtrees[offset].tree.root;
				node.children = {blk_node};
				if(else_node.has_value())
				{
					node.children.push_back(else_node.value());
				}
				return true;
			}
			break;
			case lex::type::keyword_for:
			{
				if(!retr.avail()){return false;}
				auto init_expr = retr.retrieve<ast::expression>();
				if(!init_expr.has_value())
				{
					return false;
				}

				if(!retr.avail()){return false;}
				auto comma1 = retr.retrieve<lex::token>();
				if(!comma1.has_value() || comma1->t != lex::type::comma)
				{
					return false;
				}

				auto cond_expr = retr.retrieve<ast::expression>();
				if(!cond_expr.has_value())
				{
					return false;
				}

				if(!retr.avail()){return false;}
				auto comma2 = retr.retrieve<lex::token>();
				if(!comma2.has_value() || comma2->t != lex::type::comma)
				{
					return false;
				}

				if(!retr.avail()){return false;}
				auto iter_expr = retr.retrieve<ast::expression>();
				if(!iter_expr.has_value())
				{
					return false;
				}

				if(!retr.avail()){return false;}
				ast::node blk_node;
				auto blk = retr.retrieve<ast::block>(nullptr, &blk_node);
				if(!blk.has_value())
				{
					return false;
				}

				retr.reduce_to(ast::for_statement{.init_expr = init_expr.value(), .cond_expr = cond_expr.value(), .iter_expr = iter_expr.value()}, meta);
				ast::node& node = this->subtrees[offset].tree.root;
				node.children = {blk_node};
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
				// note: its possible some contents of the block have been already sent to the final tree.
				// bring them back and make them children of this.
				std::optional<ast::node> maybe_node = std::nullopt;
				do
				{
					maybe_node = pop_from_final_tree_if([open = meta, close = close_brace->meta_srcloc](const ast::node& n)->bool
					{
						return (open < n.meta) && (n.meta < close);
					});
					if(maybe_node.has_value())
					{
						expression_nodes.push_back(maybe_node.value());
					}
				}while(maybe_node.has_value());
				// expression_nodes may no longer be sorted.
				// sort them in order of srcloc.
				std::sort(expression_nodes.begin(), expression_nodes.end());
				auto& result = this->subtrees[offset];
				diag::assert_that(std::holds_alternative<ast::block>(result.tree.root.payload), error_code::ice, "failed to retrieve block spawned at {} even though i literally just created it.", meta.to_string());
				for(const auto& node : expression_nodes)
				{
					result.tree.root.children.push_back(node);
				}
				return true;
			}
			case lex::type::open_paren:
			{
				if(!retr.avail()){return false;}
				auto maybe_expr = retr.retrieve<ast::expression>();
				if(!maybe_expr.has_value())
				{
					return false;
				}
				if(!retr.avail()){return false;}
				auto close_paren = retr.retrieve<lex::token>();
				if(!close_paren.has_value() || close_paren->t != lex::type::close_paren)
				{
					return false;
				}
				// if there is a leading semicolon - we eat it.
				if(retr.avail())
				{
					bool capped = true;
					auto semicolon = retr.retrieve<lex::token>();
					if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
					{
						capped = false;
						retr.undo();
					}
					// we got a semicolon, but we're not going to do anything (we're about to swallow it)
					maybe_expr->capped |= capped;
					retr.reduce_to(maybe_expr.value(), meta);
					return true;
				}
			}
			break;
			case lex::type::operator_double_equals:
			{
				if(!retr.avail()){return false;}
				auto region = retr.retrieve<ast::variable_declaration>();
				if(!region.has_value())
				{
					return false;
				}
				if(!retr.avail()){return false;}
				auto double_equals = retr.retrieve<lex::token>();
				if(!double_equals.has_value() || double_equals->t != lex::type::operator_double_equals)
				{
					return false;
				}
				if(!retr.avail()){return false;}
				ast::node blk_node;
				auto blk = retr.retrieve<ast::block>(nullptr, &blk_node);
				if(!blk.has_value())
				{
					return false;
				}
				retr.reduce_to(ast::meta_region{.name = region->var_name, .type = region->type_name}, meta);
				ast::node& node = this->subtrees[offset].tree.root;
				node.children = {blk_node};
				return true;
			}
			break;
			case lex::type::return_statement:
			{
				if(!retr.avail()){return false;}
				auto expr = retr.retrieve<ast::expression>();
				if(!expr.has_value())
				{
					retr.undo();
				}
				// if we dont have an expression, we must have a semicolon.
				// if we do have an expression, we need a semicolon if its not already capped.
				if(!expr.has_value() || (expr.has_value() && !expr->capped))
				{
					if(!retr.avail()){return false;}
					auto semicolon = retr.retrieve<lex::token>();
					if(!semicolon.has_value() || semicolon->t != lex::type::semicolon)
					{
						return false;
					}
				}
				retr.reduce_to(ast::return_statement{.expr = expr}, meta);
				return true;
			}
			break;
			default: break;
		}
		if(token_is_unary_operator(value))
		{
			if(!retr.avail()){return false;}
			// unary -
			auto operand = retr.retrieve<ast::expression>();
			if(operand.has_value())
			{
				retr.reduce_to(ast::expression{.expr =
					ast::unary_operator
					{
						.op = value,
						.expr = operand.value(),
					}, .capped = operand.value().capped}, meta);
				return true;
			}
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
					[&](ast::char_literal arg)
					{
						ret = this->reduce_from_char_literal(i);
					},
					[&](ast::string_literal arg)
					{
						ret = this->reduce_from_string_literal(i);
					},
					[&](ast::bool_literal arg)
					{
						ret = this->reduce_from_bool_literal(i);
					},
					[&](ast::null_literal arg)
					{
						ret = this->reduce_from_null_literal(i);
					},
					[&](ast::identifier arg)
					{
						ret = this->reduce_from_identifier(i);
					},
					[&](ast::member_access arg)
					{
						ret = this->reduce_from_member_access(i);
					},
					[&](ast::array_access arg)
					{
						ret = this->reduce_from_array_access(i);
					},
					[&](ast::variable_declaration arg)
					{
						ret = this->reduce_from_variable_declaration(i);
					},
					[&](ast::function_call arg)
					{
						ret = this->reduce_from_function_call(i);
					},
					[&](ast::method_call arg)
					{
						ret = this->reduce_from_method_call(i);
					},
					[&](ast::return_statement arg)
					{
						ret = this->reduce_from_return_statement(i);
					},
					[&](ast::if_statement arg)
					{
						ret = this->reduce_from_if_statement(i);
					},
					[&](ast::for_statement arg)
					{
						ret = this->reduce_from_for_statement(i);
					},
					[&](ast::struct_initialiser arg)
					{
						ret = this->reduce_from_struct_initialiser(i);
					},
					[&](ast::expression arg)
					{
						ret = this->reduce_from_expression(i);
					},
					[&](ast::function_definition arg)
					{
						ret = this->reduce_from_function_definition(i);
					},
					[&](ast::struct_definition arg)
					{
						ret = this->reduce_from_struct_definition(i);
					},
					[&](ast::block arg)
					{
						ret = this->reduce_from_block(i);
					},
					[&](ast::meta_region arg)
					{
						ret = this->reduce_from_meta_region(i);
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