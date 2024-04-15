#include "parse.hpp"
#include "diag.hpp"
#include "lex.hpp"
#include <string>
#include <format>
#include <optional>

namespace parser
{
	// recursive descent parser.
	// does not handle left recursion.
	struct parser_impl
	{
		// UNCONDITIONALLY error out and tell the user where shit got fucked.
		void parser_error(std::string msg) const
		{
			diag::error(std::format("parser error on line {}: {}", this->current_line, msg));
		}

		// CONDITIONALLY error out and tell the user where shit got fucked.
		void parser_assert(bool expr, std::string msg) const
		{
			if(!expr)
			{
				this->parser_error(msg);
			}
		}

		// i am currently at some position in the massive list of tokens.
		// im calling this function coz i want to know if the next token is of a certain type.
		// if it is, move to the next token and return true, otherwise dont move and return false.
		// oh btw i will also skip over comments and newlines at this point.
		bool match(lexer::token::type expected_type)
		{
			while(this->tokidx < this->tokens.size() && (this->tokens[this->tokidx].id == lexer::token::type::newline || this->tokens[this->tokidx].id == lexer::token::type::line_comment))
			{
				this->current_line++;
				this->tokidx++;
			}
			if(this->tokidx < this->tokens.size() && this->tokens[this->tokidx].id == expected_type)
			{
				this->tokidx++;
				return true;
			}
			return false;
		}

		// like match, but now i am **CERTAIN** that i have guessed the next token correctly. im too clever to be wrong so if i guessed wrongly then the .psy code must be fucked so i return an error :)
		void must_match(lexer::token::type expected_type)
		{
			this->parser_assert(this->match(expected_type), std::format("required a specific token {} but did not match.", lexer::token_type_names[static_cast<std::size_t>(expected_type)]));
		}

		// a bit like match. i KNOW its one of multiple types... but not sure quite which.
		// return me the type that it is, or return null if i got it wrong and none of the types match.
		std::optional<lexer::token::type> match_any(std::vector<lexer::token::type> expected_types)
		{
			for(const auto& type : expected_types)
			{
				if(this->match(type))
				{
					return type;
				}
			}
			return std::nullopt;
		}

		// match_any but if im wrong and its none of the types, then the code must be dodgy coz i know best.
		lexer::token::type must_match_any(std::vector<lexer::token::type> expected_types)
		{
			auto maybe_type = this->match_any(expected_types);
			if(!maybe_type.has_value())
			{
				std::string tokens_string;
				for(const auto& tok : expected_types)
				{
					tokens_string += std::format("{} ", lexer::token_type_names[static_cast<int>(tok)]);
				}
				this->parser_error(std::format("line {}: required one of the following token(s) \"{}\" but did not match.", this->current_line, tokens_string));
			}
			return maybe_type.value();
		}

		// what was the last token again?
		std::string last_value() const
		{
			this->parser_assert(this->tokidx > 0, "internal compiler error: last_value() called before any matches.");
			return this->tokens[this->tokidx - 1].value;	
		}

		// add a node payload to the current location in the AST.
		void push_payload(ast::node::payload_t payload)
		{
			this->tree.push(ast::node
			{
				.payload = payload,
				.meta =
				{
					.line_number = this->current_line
				},
				.children = {}
			});
		}
		
		// move to the parent of the current node within the AST.
		void pop()
		{
			this->tree.pop();
		}

		// call this before matching against stuff that you might want to undo.
		void stash_index()
		{
			this->index_stash.push_back(this->tokidx);
		}

		// ok i stashed earlier but i dont need it anymore - im happy the way things are.
		void unstash_index()
		{
			this->parser_assert(this->index_stash.size(), "internal compiler error - attempt to unstash index when no index has been stashed.");
			this->index_stash.pop_back();
		}

		// ok i fugged up. restore the index i stashed earlier please and then unstash it. maybe i took a guess while parsing and got it wrong, so i want to undo the crap and try again.
		void restore_index()
		{
			this->parser_assert(this->index_stash.size(), "internal compiler error - attempt to restore index when no index has been stashed.");
			this->tokidx = this->index_stash.back();
			this->unstash_index();
		}

		// PARSING BITS BEGIN

		// note: these `try_parse_xyz()` functions will parse the current token(s) and construct the xyz corresponding to it, if it makes sense.
		// if the token(s) clearly don't parse into `xyz`, we will return null and rewind any position in the token list we moved.

		std::optional<ast::integer_literal> try_parse_integer_literal()
		{
			this->stash_index();
			if(!this->match(lexer::token::type::integer_literal))
			{
				this->restore_index();
				return std::nullopt;
			}
			this->unstash_index();
			return ast::integer_literal{.val = std::stoi(this->last_value())};
		}

		std::optional<ast::decimal_literal> try_parse_decimal_literal()
		{
			this->stash_index();
			if(!this->match(lexer::token::type::decimal_literal))
			{
				this->restore_index();
				return std::nullopt;
			}
			this->unstash_index();
			return ast::decimal_literal{.val = std::stod(this->last_value())};
		}

		std::optional<ast::string_literal> try_parse_string_literal()
		{
			this->stash_index();
			if(!this->match(lexer::token::type::string_literal))
			{
				this->restore_index();
				return std::nullopt;
			}
			this->unstash_index();
			return ast::string_literal{.val = this->last_value()};
		}

		std::optional<ast::bool_literal> try_parse_bool_literal()
		{
			this->stash_index();
			if(!this->match(lexer::token::type::bool_literal))
			{
				this->restore_index();
				return std::nullopt;
			}
			this->unstash_index();
			std::string boolstr = this->last_value();
			bool val;
			if(boolstr == "true")
			{
				val = true;
			}
			else if(boolstr == "false")
			{
				val = false;
			}
			else
			{
				parser_error(std::format("internal compiler error: i am convinced a bool literal lies here, but i was expecting `true` or `false`, not `{}`", boolstr));
			}
			return ast::bool_literal{.val = val};
		}

		std::optional<ast::function_call> try_parse_function_call()
		{
			this->stash_index();
			// a function call must start with an identifier.
			if(!this->match(lexer::token::type::identifier))
			{
				this->restore_index();
				return std::nullopt;
			}
			std::string function_name = this->last_value();
			if(!this->match(lexer::token::type::open_paren))
			{
				// its not a function call.
				this->restore_index();
				return std::nullopt;
			}
			std::vector<ast::expression> params = {};
			while(!this->match(lexer::token::type::close_paren))
			{
				auto maybe_expr = this->try_parse_expression();
				this->parser_assert(maybe_expr.has_value(), std::format("failed to parse expression as parameter {} during call of function {}", params.size() + 1, function_name));
				params.push_back(maybe_expr.value());
				this->match(lexer::token::type::commar);
			}
			this->unstash_index();
			return ast::function_call{.function_name = function_name, .params = params};
		}

		std::optional<ast::expression> try_parse_expression()
		{
			this->stash_index();
			// to help understand whats going on here, see the variant types in ast::expression::expr.
			// what we're doing here is going through each possible type and attempting to parse.
			std::optional<lexer::token::type> maybe_operator = this->match_any({lexer::token::type::minus, lexer::token::type::bitwise_complement, lexer::token::type::logical_negation, lexer::token::type::plus, lexer::token::type::double_equals, lexer::token::type::equals, lexer::token::type::not_equals});	
			if(maybe_operator.has_value())
			{
				// could either be a unary or binary operator, depending on the token.
				// next thing we want is definitely an expression, either way.
				std::optional<ast::expression> expression_a = this->try_parse_expression();
				this->parser_assert(expression_a.has_value(), "while parsing expression, another nested expression *must* directly follow any unary operator.");
				// if we're a binary operator, we expect another expression.
				// otherwise, we assume we're a unary operator.
				std::optional<ast::expression> expression_b = this->try_parse_expression();
				this->unstash_index();
				if(expression_b.has_value())
				{
					// definitely a binary expression.
					return ast::expression{.expr = std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>{maybe_operator.value(), expression_a.value(), expression_b.value()}};
				}
				else
				{
					// assume a unary expression.
					return ast::expression{.expr = std::pair<ast::unary_operator, util::box<ast::expression>>{maybe_operator.value(), expression_a.value()}};
				}
			}

			auto maybe_integer_literal = this->try_parse_integer_literal();
			if(maybe_integer_literal.has_value())
			{
				this->unstash_index();
				return ast::expression{.expr = maybe_integer_literal.value()};
			}

			auto maybe_decimal_literal = this->try_parse_decimal_literal();
			if(maybe_decimal_literal.has_value())
			{
				this->unstash_index();
				return ast::expression{.expr = maybe_decimal_literal.value()};
			}

			auto maybe_string_literal = this->try_parse_string_literal();
			if(maybe_string_literal.has_value())
			{
				this->unstash_index();
				return ast::expression{.expr = maybe_string_literal.value()};
			}

			auto maybe_bool_literal = this->try_parse_bool_literal();
			if(maybe_bool_literal.has_value())
			{
				this->unstash_index();
				return ast::expression{.expr = maybe_bool_literal.value()};
			}

			auto maybe_function_call = this->try_parse_function_call();
			if(maybe_function_call.has_value())
			{
				this->unstash_index();
				return ast::expression{.expr = maybe_function_call.value()};
			}

			bool is_identifier = this->match(lexer::token::type::identifier);
			if(is_identifier)
			{
				this->unstash_index();
				return ast::expression{.expr = ast::identifier{.name = this->last_value()}};
			}

			// didn't match anything. we're done.
			this->restore_index();
			return std::nullopt;
		}

		std::optional<ast::if_statement> try_parse_if_statement()
		{
			this->stash_index();
			if(this->match(lexer::token::type::keyword) && this->last_value() == "if")
			{
				this->must_match(lexer::token::type::open_paren);
				auto maybe_condition_expr = this->try_parse_expression();
				this->parser_assert(maybe_condition_expr.has_value(), "condition inside if-statement must be an expression. failed to parse expression.");
				this->must_match(lexer::token::type::close_paren);
				this->unstash_index();
				return ast::if_statement{.condition = maybe_condition_expr.value()};
			}
			this->restore_index();
			return std::nullopt;
		}

		std::optional<ast::for_statement> try_parse_for_statement()
		{
			this->stash_index();
			if(this->match(lexer::token::type::keyword) && this->last_value() == "for")
			{
				this->must_match(lexer::token::type::open_paren);
				auto maybe_start_expr = this->try_parse_expression();
				this->parser_assert(maybe_start_expr.has_value(), "first (begin) portion of for-statement must be an expression. failed to parse expression.");

				this->must_match(lexer::token::type::semicolon);

				auto maybe_end_expr = this->try_parse_expression();
				this->parser_assert(maybe_end_expr.has_value(), "second (end) portion of for-statement must be an expression. failed to parse expression.");

				this->must_match(lexer::token::type::semicolon);

				auto maybe_loop_expr = this->try_parse_expression();
				this->parser_assert(maybe_loop_expr.has_value(), "third (loop) portion of for-statement must be an expression. failed to parse expression.");

				this->must_match(lexer::token::type::close_paren);
				this->unstash_index();
				return ast::for_statement{.start = maybe_start_expr.value(), .end = maybe_end_expr.value(), .loop = maybe_loop_expr.value()};
			}
			this->restore_index();
			return std::nullopt;
		}

		std::optional<ast::return_statement> try_parse_return_statement()
		{
			this->stash_index();
			if(this->match(lexer::token::type::keyword) && this->last_value() == "return")
			{
				auto maybe_expression = this->try_parse_expression();
				this->unstash_index();
				if(!maybe_expression.has_value())
				{
					return ast::return_statement{.value = std::nullopt};
				}
				this->parser_assert(maybe_expression.has_value(), "could not parse return expression.");
				return ast::return_statement{.value = maybe_expression.value()};
			}
			this->restore_index();
			return std::nullopt;
		}

		std::optional<ast::variable_declaration> try_parse_variable_declaration()
		{
			this->stash_index();
			// variable declaration *must* start with an identifier (varname).
			if(!this->match(lexer::token::type::identifier))
			{
				this->restore_index();
				return std::nullopt;
			}
			std::string var_name = this->last_value();
			bool follows_variable_syntax = true;
			follows_variable_syntax &= this->match(lexer::token::type::colon);
			follows_variable_syntax &= this->match(lexer::token::type::identifier);
			if(!follows_variable_syntax)
			{
				this->restore_index();
				return std::nullopt;
			}
			std::string type_name = this->last_value();
			// remember: the identifier may not contain the full type (e.g if its an array type)
			if(this->match(lexer::token::type::open_brack))
			{
				// it's an array type.
				// for now, the bits inside the brackets can only be an integer literal , an identifier, or an ellipsis. not some nested expression.
				std::optional<lexer::token::type> matched = this->match_any({lexer::token::type::identifier, lexer::token::type::integer_literal, lexer::token::type::ellipsis});
				std::string subscript_contents = this->last_value();
				parser_assert(matched.has_value(), std::format("cannot parse array-type variable declaration. between the brackets must lie either an integer-literal, identifier or alternatively an ellipsis (\"...\") to signify a dynamic array."));
				if(subscript_contents.empty())
				{
					subscript_contents += lexer::token_type_names[static_cast<int>(matched.value())];
				}
				else if(matched)
				type_name += std::format("[{}]", subscript_contents);
				this->must_match(lexer::token::type::close_brack);
			}
			// it may or may not have an initialiser.
			std::optional<ast::expression> initialiser = std::nullopt;
			if(this->match(lexer::token::type::equals))
			{
				initialiser = this->try_parse_expression();
				this->parser_assert(initialiser.has_value(), std::format("initialiser of new local variable {} could not be parsed properly.", var_name));
			}
			this->unstash_index();
			return ast::variable_declaration{.var_name = var_name, .type_name = type_name, .initialiser = initialiser};
		}

		std::optional<ast::function_definition> try_parse_function_definition()
		{
			this->stash_index();
			// function definition *must* start with an identifier (function name)
			if(!this->match(lexer::token::type::identifier))
			{
				this->restore_index();
				return std::nullopt;
			}
			std::string function_name = this->last_value();
			this->must_match(lexer::token::type::colon);
			this->must_match(lexer::token::type::open_paren);
			std::vector<ast::variable_declaration> params = {};
			while(!this->match(lexer::token::type::close_paren))
			{
				auto maybe_param = this->try_parse_variable_declaration();
				this->parser_assert(maybe_param.has_value(), std::format("expression representing parameter {} of newly-defined function {} could not be parsed correctly", params.size() + 1, function_name));
				params.push_back(maybe_param.value());
				this->match(lexer::token::type::commar);
			}
			this->must_match(lexer::token::type::arrow);
			this->must_match(lexer::token::type::identifier);
			std::string return_type = this->last_value();
			bool is_extern = false;
			if(this->match(lexer::token::type::equals))
			{
				// you're trying to assign something to a function.
				// it must be `extern`
				this->must_match(lexer::token::type::keyword);
				std::string keyword = this->last_value();
				parser_assert(keyword == "extern", std::format("attempt to assign an identifier \"{}\"to a function declaration. this is only valid for \"extern\"", keyword));
				this->must_match(lexer::token::type::semicolon);
				is_extern = true;
			}
			this->unstash_index();
			return ast::function_definition{.function_name = function_name, .params = params, .return_type = return_type, .is_extern = is_extern};
		}

		// a block is not a formal parser construct.
		// imagine i just defined a function, and now im looking at the code inside a pair of braces. that is a block.
		// in other words, its a bunch of code within a function definition. could be anything... variables, expressions... perhaps a nested function definition, or simply nothing at all!
		std::vector<ast::node::payload_t> parse_block(bool continued_block = false)
		{
			std::vector<ast::node::payload_t> ret = {};
			if(!continued_block)
			{
				this->must_match(lexer::token::type::open_brace);
			}
			while(!this->match(lexer::token::type::close_brace))
			{
				auto maybe_return_statement = this->try_parse_return_statement();
				if(maybe_return_statement.has_value())
				{
					ret.push_back(maybe_return_statement.value());
					this->must_match(lexer::token::type::semicolon);
					continue;
				}

				auto maybe_if_statement = this->try_parse_if_statement();
				if(maybe_if_statement.has_value())
				{
					ret.push_back(maybe_if_statement.value());
					// if statement detected. return immediately, as next statements are children.
					return ret;
					//std::vector<ast::node::payload_t> if_block = this->parse_block();
					//ret.insert(ret.end(), if_block.begin(), if_block.end());
				}

				auto maybe_for_statement = this->try_parse_for_statement();
				if(maybe_for_statement.has_value())
				{
					// see above for if-statement.
					ret.push_back(maybe_for_statement.value());
					return ret;
				}

				auto maybe_variable_declaration = this->try_parse_variable_declaration();
				if(maybe_variable_declaration.has_value())
				{
					ret.push_back(maybe_variable_declaration.value());
					this->must_match(lexer::token::type::semicolon);
					continue;
				}
				auto maybe_expression = this->try_parse_expression();
				this->parser_assert(maybe_expression.has_value(), "cannot parse expression within block");
				ret.push_back(maybe_expression.value());
				this->must_match(lexer::token::type::semicolon);
			}
			return ret;
		}

		void handle_payload(ast::node::payload_t payload)
		{
			this->push_payload(payload);
			if(std::holds_alternative<ast::if_statement>(payload) || std::holds_alternative<ast::for_statement>(payload))
			{
				// if we're an if-statement, we need to parse another block and set all those as children, and THEN pop.
				auto if_blk = this->parse_block();
				for(const auto& if_contents : if_blk)
				{
					handle_payload(if_contents);
				}
				// remember - we early-outed coz of the if-statement. now that we're done, let's continue parsing this block.
				this->pop();
				for(const auto& more_contents : this->parse_block(true))
				{
					this->handle_payload(more_contents);
				}
			}
			else
			{
				this->pop();
			}
		}


		void parse()
		{
			while(this->tokidx < this->tokens.size())
			{
				auto maybe_function_definition = this->try_parse_function_definition();
				if(maybe_function_definition.has_value())
				{
					this->push_payload(maybe_function_definition.value());
					// if not extern, then we have a function definition block next.
					if(!maybe_function_definition.value().is_extern)
					{
						auto blk = this->parse_block();
						for(const auto& contents : blk)
						{
							this->handle_payload(contents);
						}
					}
					this->pop();
				}
				else
				{
					// nothing parses. remember, we skip over comments etc anyway.
					// so if the last thing in the program is comments, then it will continually be skipped over but not parse anything, causing an infinite loop.
					// for that reason we just stop here.
					break;
				}
			}
		}

		// PARSING BITS END

		ast get_ast() const
		{
			return this->tree;
		}

		lexer::const_token_view tokens;
		std::size_t tokidx = 0;
		std::size_t current_line = 1;
		std::vector<std::size_t> index_stash = {};
		ast tree = {};
	};

	ast parse(lexer::const_token_view tokens)
	{
		parser_impl parser{.tokens = tokens};
		parser.parse();
		return parser.get_ast();
	}
}