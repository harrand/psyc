#include "semantic.hpp"
#include "diag.hpp"
#include <format>
#include <unordered_map>

namespace semantic
{
	struct semantic_state
	{
		// keeps track of all defined functions (name mapped to the path from AST root to the function_definition node).
		std::unordered_map<std::string, parser::ast::path_t> defined_functions = {};
	} global_state;

	std::size_t pass_number = 0;

	void analyse_function_call(const parser::ast::node& node, const parser::ast::function_call& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		// we only analyse function calls in the second pass.
		if(pass_number == 0)
		{
			return;
		}
		auto iter = global_state.defined_functions.find(payload.function_name);
		diag::assert_that(iter != global_state.defined_functions.end(), std::format("call to undefined function \"{}\" at line {}", payload.function_name, node.meta.line_number));

		const auto& func_node = ast.get(iter->second);
		const parser::ast::function_definition& func = std::get<parser::ast::function_definition>(func_node.payload);
		if(func.params.size() != payload.params.size())
		{
			diag::error(std::format("invalid call to function {} at line {}. expects {} parameters, but you provided {}", func.function_name, node.meta.line_number, func.params.size(), payload.params.size()));
			std::cout << std::format("note: see below for signature of {} (line {}):\n\t{}\n", payload.function_name, func_node.meta.line_number, func.to_string());
			return;
		}
		for(std::size_t i = 0; i < func.params.size(); i++)
		{
			const parser::ast::variable_declaration& defined_param = func.params[i];
			const parser::ast::expression& actual_param = payload.params[i];
			// todo: do type-checking on the parameters.
		}
	}

	template<typename T>
	void analyse_thing(const parser::ast::node& node, const T& payload_like, const parser::ast::path_t& path, const parser::ast& ast);

	void analyse_expression(const parser::ast::node& node, const parser::ast::expression& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		analyse_thing(node, payload.expr, path, ast);
	}
	
	void analyse_if_statement(const parser::ast::node& node, const parser::ast::if_statement& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

	}

	void analyse_return_statement(const parser::ast::node& node, const parser::ast::return_statement& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		// todo: ensure that this return statement is actually within a function definition block.
		//		note: to do this - recursively iterate back up through the tree until you find the first function definition. if you never find one - then this return statement is invalid coz you can only return from within a function block.
		// todo: type-check the return expression. if you did the above, you can look at the function definition and figure out what its return type is.
	}

	void analyse_function_definition(const parser::ast::node& node, const parser::ast::function_definition& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		// only analyse definitions in first pass.
		if(pass_number != 0)
		{
			return;
		}
		auto iter = global_state.defined_functions.find(payload.function_name);
		if(iter == global_state.defined_functions.end())
		{
			global_state.defined_functions[payload.function_name] = path;
		}
		else
		{
			const auto& existing_node = ast.get(iter->second);
			diag::error(std::format("redefinition of function \"{}\" at line {} (previously defined on line {})", payload.function_name, node.meta.line_number, existing_node.meta.line_number));
		}
	}

	void analyse_variable_declaration(const parser::ast::node& node, const parser::ast::variable_declaration& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

	}

	void analyse_identifier(const parser::ast::node& node, const parser::ast::identifier& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

	}

	template<typename P>
	void analyse_thing(const parser::ast::node& node, const P& payload_like, const parser::ast::path_t& path, const parser::ast& ast)
	{
		std::visit([&node, &ast, &path](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr(std::is_same_v<T, std::monostate>
			|| std::is_same_v<T, parser::ast::decimal_literal>
			|| std::is_same_v<T, parser::ast::integer_literal>
			|| std::is_same_v<T, parser::ast::string_literal>){}
			else if constexpr(std::is_same_v<T, parser::ast::function_call>)
			{
				analyse_function_call(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::expression>)
			{
				analyse_expression(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::if_statement>)
			{
				analyse_if_statement(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::return_statement>)
			{
				analyse_return_statement(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::function_definition>)
			{
				analyse_function_definition(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::variable_declaration>)
			{
				analyse_variable_declaration(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::identifier>)
			{
				analyse_identifier(node, arg, path, ast);
			}
			else
			{
				diag::fatal_error(std::format("internal compiler error: unknown AST node type (variant id: {}) detecting during semantic analysis.", node.payload.index()));
			}
		}, payload_like);
	}


	void analyse_single_node(parser::ast::path_t path, const parser::ast& ast)
	{
		const parser::ast::node& node = ast.get(path);
		analyse_thing(node, node.payload, path, ast);
	}

	void analyse_node(parser::ast::path_t path, const parser::ast& ast)
	{
		analyse_single_node(path, ast);
		const parser::ast::node& node = ast.get(path);
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			auto child_path = path;
			child_path.push_back(i);
			analyse_node(child_path, ast);
		}
	}

	void first_pass(const parser::ast& ast)
	{
		pass_number = 0;
		analyse_node({}, ast);
	}

	void second_pass(const parser::ast& ast)
	{
		pass_number = 1;
		analyse_node({}, ast);
	}

	void analysis(const parser::ast& ast)
	{
		global_state = {};
		// we do semantic analysis in 2 passes.
		// if we do it in one-pass, then things must be defined *before* being used.
		// i want you to be able to call function foo() before you define it (so long as you do indeed define it at some point).
		//	iirc java/c#/python etc does this, but C/C++ etc doesnt. semantic analysis is pretty cheap anyways in terms of time, so im happy to take the minor perf hit to not have to deal with forward declares.
		// the first pass basically harvests semantic information (e.g function definitions, variables within each scope etc...)
		// second pass uses that information to actually perform verification.
		first_pass(ast);
		second_pass(ast);
	}
}