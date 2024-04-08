#include "parse.hpp"
#include "diag.hpp"

namespace parser
{
	/*
	incomplete psy grammar in backus-naur form:

	identifier ::= [a-zA-Z_][a-zA-Z0-9_]*
	typename ::= [a-zA-Z_][a-zA-Z0-9_]*
	parameter ::= typename identifier
	maybe_parameter ::= parameter | ""
	parameter_list ::= parameter , parameter_list | maybe_parameter | ""

	integer_literal ::= [0-9]+
	float_literal ::= [0-9]+\.[0-9]+
	literal := i16_literal | i32_literal
	expression ::= literal | variable | "(" expression ")" | unary_expression | binary_expression

	expression_statement ::= expression ";"
	return_statement ::= "return" expression_statement
	statement ::= return_statement | expression_statement
	compound_statement ::= "{" statement_list "}"

	function_definition ::= identifier ":" "(" parameter_list ")" "->" typename compound_statement
	main_function_definition ::= "main" ":" "(" parameter_list ")" "->" typename compound_statement

	program ::= main_function_definition

	*/
	constexpr const char* default_type_names[] =
	{
		"u0", // empty "void" type
		"i64", // 64 bit signed int type
		"i32", // 32 bit signed int type
		"u64", // 64 bit unsigned int type
		"u32", // 32 but unsigned int type
		"f64", // 64 bit ieee-754 (double)
		"f32", // 32 bit ieee-754 (single aka. float)
		"f16", // 16 bit ieee-754 (half aka. half float)
	};

	struct type_name
	{
		std::string name;
		constexpr bool is_user_defined()
		{
			constexpr auto type_count = sizeof(default_type_names) / sizeof(const char*);
			for(std::size_t i = 0; i < type_count; i++)
			{
				if(this->name == default_type_names[i])
				{
					return true;
				}
			}
			return false;
		}
	};
	struct identifier{std::string name;};

	struct variable{type_name type; identifier identifier;};

	//expression ::= literal | variable | "(" expression ")" | unary_expression | binary_expression

	// ast implementation begin
	const ast::node& ast::current() const
	{
		const node* cur = &this->program;
		for(std::size_t id : this->path)
		{
			cur = &cur->children[id];
		}
		return *cur;
	}

	ast::node& ast::current()
	{
		node* cur = &this->program;
		for(std::size_t id : this->path)
		{
			cur = &cur->children[id];
		}
		return *cur;
	}

	void ast::push(ast::node n)
	{
		auto& cur = this->current();
		auto id = cur.children.size();
		cur.children.push_back(n);
		this->path.push_back(id);
	}

	void ast::push(ast::node::payload_t payload)
	{
		this->push(node{.payload = payload, .children = {}});
	}

	void ast::pop()
	{
		diag::assert_that(this->path.size(), "internal compiler error: popped too many times while parsing.");
		this->path.pop_back();
	}

	// ast implementation end

	class parser
	{
	public:
		parser(lexer::const_token_view tokens): tokens(tokens)
		{

		}

		// match tokens.
		bool match(lexer::token::type expected_type)
		{
			if(this->index < this->tokens.size())
			{
				while(this->tokens[this->index].id == lexer::token::type::newline)
				{
					this->current_line++;
					this->index++;
				}
				if(this->tokens[this->index].id == expected_type)
				{
					this->index++;
					return true;
				}
			}
			return false;
		}

		std::string last_value()
		{
			diag::assert_that(this->index > 0, "internal compiler error: last_value() called before any matches.");
			return this->tokens[this->index - 1].value;
		}

		void expression()
		{
			// function call
			if(this->match(lexer::token::type::identifier))
			{
				std::string name = this->last_value();
				if(this->match(lexer::token::type::open_paren))
				{
					if(this->match(lexer::token::type::close_paren))
					{
						// this was a function call.
						this->tree.push(ast::function_call
						{
							.function_name = {name},
							.parameters = {}
						});
					}
					else
					{
						// e.g: 
						// something(
						// and the next token is not a close. must be a function call with parameters.
						diag::error("internal compiler error: function call with parameters is not yet implemented");
					}
				}
			}
		}

		void statement()
		{
			if(this->match(lexer::token::type::keyword) && this->last_value() == "return")
			{
				this->match(lexer::token::type::identifier);
				std::string return_value = this->last_value();
				this->match(lexer::token::type::semicolon);
				this->tree.push(ast::return_statement{.value = return_value});
			}
			else
			{
				expression();
				this->match(lexer::token::type::semicolon);
			}
		}

		void block()
		{
			this->match(lexer::token::type::open_brace);
			while(!this->match(lexer::token::type::close_brace))
			{
				statement();
			}
		}

		void main_function()
		{
			bool succ = true;
			succ &= this->match(lexer::token::type::identifier);
			succ &= this->last_value() == "main";
			succ &= this->match(lexer::token::type::colon);
			succ &= this->match(lexer::token::type::open_paren);
			succ &= this->match(lexer::token::type::close_paren);
			succ &= this->match(lexer::token::type::arrow);
			succ &= this->match(lexer::token::type::identifier);
			succ &= this->last_value() == "i64";
			this->block();
		}

		void parse()
		{
			this->main_function();
		}

		ast get_ast() const
		{
			return this->tree;
		}

	private:
		lexer::const_token_view tokens;
		std::size_t index = 0;
		std::size_t current_line = 0;
		ast tree = {};
	};

	ast parse(lexer::const_token_view tokens)
	{
		parser p{tokens};
		p.parse();

		return p.get_ast();
	}
}