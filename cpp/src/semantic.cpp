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

		/*
		const auto& func_node = ast.get(iter->second);
		const parser::ast::function_definition& func = std::get<parser::ast::function_definition>(func_node.payload);
		if(func.parameters.size() != payload.parameters.size())
		{
			diag::error(std::format("invalid call to function {} at line {}. expects {} parameters, but you provided {}", func.function_name.name, node.meta.line_number, func.parameters.size(), payload.parameters.size()));
			std::cout << std::format("note: see below for signature of {} (line {}):\n", payload.function_name.name, func_node.meta.line_number);
			func.pretty_print();
			std::cout << "\n";
		}
		for(std::size_t i = 0; i < func.parameters.size(); i++)
		{
			// todo: do type-checking on the parameters.
			const parser::ast::variable_declaration& defined_param = func.parameters[i];
			const parser::ast::expression& actual_param = payload.parameters[i];
			std::visit([&](auto&& arg)
			{
				using T = std::decay_t<decltype(arg)>;
				if constexpr(std::is_same_v<T, parser::ast::integer_literal>)
				{
					diag::assert_that(defined_param.type.name == "i64", std::format("call of {} on line {}: attempt to pass integer literal to param {} of type {}, which cannot be converted.", func.function_name.name, node.meta.line_number, defined_param.name.name, defined_param.type.name));
				}
				else if constexpr(std::is_same_v<T, parser::ast::decimal_literal>)
				{
					diag::assert_that(defined_param.type.name == "f64", std::format("call of {} on line {}: attempt to pass decimal literal to param {} of type {}, which cannot be converted.", func.function_name.name, node.meta.line_number, defined_param.name.name, defined_param.type.name));
				}
				else if constexpr(std::is_same_v<T, parser::ast::string_literal>)
				{
					diag::assert_that(defined_param.type.name == "string", std::format("call of {} on line {}: attempt to pass string literal to param {} of type {}, which cannot be converted.", func.function_name.name, node.meta.line_number, defined_param.name.name, defined_param.type.name));
				}
				else if constexpr(std::is_same_v<T, parser::ast::identifier>)
				{
					// should be a variable name.
					// todo: track variables and do type checking here.
				}
				else
				{
					diag::error(std::format("internal compiler error - failed to semantically analyse parameter value {} ({}) in call to {} on line {}", defined_param.name.name, defined_param.type.name, func.function_name.name, node.meta.line_number));
				}
			}, actual_param);
		}
		*/
	}

	template<typename T>
	void analyse_thing(const parser::ast::node& node, const T& payload_like, const parser::ast::path_t& path, const parser::ast& ast);

	void analyse_expression(const parser::ast::node& node, const parser::ast::expression& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		analyse_thing(node, payload.expr, path, ast);
	}

	void analyse_return_statement(const parser::ast::node& node, const parser::ast::return_statement& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

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
		analyse_node({}, ast);
	}

	void second_pass(const parser::ast& ast)
	{
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
		pass_number = 0;
		first_pass(ast);
		pass_number++;
		second_pass(ast);
	}
}