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

	void analyse_function_call(const parser::ast::node& node, const parser::ast::function_call& payload, const parser::ast::path_t& path, const parser::ast& ast)
	{
		auto iter = global_state.defined_functions.find(payload.function_name.name);
		diag::assert_that(iter != global_state.defined_functions.end(), std::format("call to undefined function \"{}\" at line {}", payload.function_name.name, node.meta.line_number));

		const parser::ast::function_definition& func = std::get<parser::ast::function_definition>(ast.get(iter->second).payload);
		diag::assert_that(func.parameters.size() == payload.parameters.size(), std::format("invalid call to function {}. expects {} parameters, but you provided {}", func.function_name.name, func.parameters.size(), payload.parameters.size()));
		for(std::size_t i = 0; i < func.parameters.size(); i++)
		{
			// todo: do type-checking on the parameters.
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
			diag::error("redefinition of function \"{}\" at line {} (previously defined on line {})", payload.function_name.name, node.meta.line_number, existing_node.meta.line_number);
		}
	}


	void analyse_single_node(parser::ast::path_t path, const parser::ast& ast)
	{
		const parser::ast::node& node = ast.get(path);
		std::visit([&node, &ast, &path](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr(std::is_same_v<T, std::monostate>){}
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