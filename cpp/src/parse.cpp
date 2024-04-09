#include "parse.hpp"
#include "diag.hpp"
#include <stack>
#include <unordered_map>
#include <format>
#include <optional>

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
		return this->get(this->path);
	}

	ast::path_t ast::current_path() const
	{
		return this->path;
	}

	void ast::push(ast::node n)
	{
		auto& cur = this->current();
		auto id = cur.children.size();
		cur.children.push_back(n);
		this->path.push_back(id);
	}

	const ast::node& ast::get(std::span<const std::size_t> path) const
	{
		const node* n = &this->program;
		for(std::size_t i : path)
		{
			n = &n->children[i];
		}
		return *n;
	}

	ast::node& ast::get(std::span<const std::size_t> path)
	{
		node* n = &this->program;
		for(std::size_t i : path)
		{
			n = &n->children[i];
		}
		return *n;
	}

	void ast::pop()
	{
		diag::assert_that(this->path.size(), "internal compiler error: popped too many times while parsing.");
		this->path.pop_back();
	}

	void ast::pretty_print()
	{
		std::stack<const ast::node*> node_list;
		std::stack<std::size_t> indents;
		node_list.push(&this->program);
		indents.push(0);
		std::cout << "program:\n";
		while(node_list.size())
		{
			const ast::node* cur = node_list.top();
			node_list.pop();
			std::size_t indent = indents.top();
			indents.pop();
			for(std::size_t i = 0; i < indent; i++)
			{
				std::cout << "  ";
			}
			std::visit([](auto&& arg)
			{
				using T = std::decay_t<decltype(arg)>;
				if constexpr(!std::is_same_v<T, std::monostate>)
				{
					arg.pretty_print();
					std::cout << "\n";
				}
			}, cur->payload);
			for(std::size_t i = 0; i < cur->children.size(); i++)
			{
				const auto& child = cur->children[cur->children.size() - 1 - i];
				node_list.push(&child);
				indents.push(indent + 1);
			}
		}
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

		std::optional<ast::path_t> try_get_function_definition(const std::string& fname)
		{
			auto iter = this->defined_functions.find(fname);
			if(iter == this->defined_functions.end())
			{
				return std::nullopt;
			}
			return iter->second;
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
						diag::assert_that(this->try_get_function_definition(name).has_value(), std::format("call to undefined function \"{}\"", name));
						this->push_payload(ast::function_call
						{
							.function_name = {name},
							.parameters = {}
						});
						this->tree.pop();
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
				this->push_payload(ast::return_statement{.value = return_value});
				this->tree.pop();
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

		void function_definition()
		{
			bool succ = true;
			this->match(lexer::token::type::identifier);
			std::string fname = this->last_value();
			succ &= this->match(lexer::token::type::colon);
			succ &= this->match(lexer::token::type::open_paren);
			succ &= this->match(lexer::token::type::close_paren);
			succ &= this->match(lexer::token::type::arrow);
			succ &= this->match(lexer::token::type::identifier);
			std::string return_type = this->last_value();
			if(succ)
			{
				this->push_payload(ast::function_definition{.function_name = {fname}, .return_type = {return_type}});
				auto maybe_existing = this->try_get_function_definition(fname);
				if(maybe_existing.has_value())
				{
					const auto& existing_node = this->tree.get(maybe_existing.value());
					diag::error(std::format("redefinition of function \"{}\" on line {} (previously defined on line {})", fname, this->current_line, existing_node.meta.line_number));
				}
				else
				{
					this->defined_functions[fname] = this->tree.current_path();
				}
				this->block();
				this->tree.pop();
			}
		}

		void parse()
		{
			while(this->index < tokens.size())
			{
				this->function_definition();
			}
			diag::assert_that(this->tree.path.empty(), "internal compiler error: AST path was not empty by the end of parsing.");
		}

		ast get_ast() const
		{
			return this->tree;
		}

	private:
		lexer::const_token_view tokens;
		std::size_t index = 0;
		std::size_t current_line = 1;
		std::unordered_map<std::string, ast::path_t> defined_functions = {};
		ast tree = {};
	};

	ast parse(lexer::const_token_view tokens)
	{
		parser p{tokens};
		p.parse();

		return p.get_ast();
	}
}