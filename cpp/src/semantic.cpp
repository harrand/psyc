#include "semantic.hpp"
#include "diag.hpp"
#include <format>
#include <unordered_map>

namespace semantic
{
	struct semantic_state
	{
		std::unordered_map<std::string, parser::ast::path_t> defined_functions = {};
	} global_state;

	void analyse_identifier(const parser::ast::node& node, const parser::ast::identifier& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

	}

	void analyse_function_call(const parser::ast::node& node, const parser::ast::function_call& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		auto iter = global_state.defined_functions.find(payload.function_name.name);
		diag::assert_that(iter != global_state.defined_functions.end(), std::format("call to undefined function \"{}\" at line {}", payload.function_name.name, node.meta.line_number));

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
	}

	void analyse_return_statement(const parser::ast::node& node, const parser::ast::return_statement& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

	}

	void analyse_function_definition(const parser::ast::node& node, const parser::ast::function_definition& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		auto iter = global_state.defined_functions.find(payload.function_name.name);
		if(iter == global_state.defined_functions.end())
		{
			global_state.defined_functions[payload.function_name.name] = path;
		}
		else
		{
			const auto& existing_node = ast.get(iter->second);
			diag::error(std::format("redefinition of function \"{}\" at line {} (previously defined on line {})", payload.function_name.name, node.meta.line_number, existing_node.meta.line_number));
		}
	}

	void analyse_variable_declaration(const parser::ast::node& node, const parser::ast::variable_declaration& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{

	}

	void analyse_single_node(parser::ast::path_t path, const parser::ast& ast)
	{
		const parser::ast::node& node = ast.get(path);
		std::visit([&node, &ast, &path](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr(std::is_same_v<T, std::monostate>
			|| std::is_same_v<T, parser::ast::decimal_literal>
			|| std::is_same_v<T, parser::ast::integer_literal>
			|| std::is_same_v<T, parser::ast::string_literal>){}
			else if constexpr(std::is_same_v<T, parser::ast::identifier>)
			{
				analyse_identifier(node, arg, path, ast);
			}
			else if constexpr(std::is_same_v<T, parser::ast::function_call>)
			{
				analyse_function_call(node, arg, path, ast);
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
		}, node.payload);
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

	void analysis(const parser::ast& ast)
	{
		global_state = {};
		analyse_node({}, ast);
	}
}