#include "semantic.hpp"
#include "diag.hpp"
#include "lex.hpp"
#include <format>
#include <unordered_map>

namespace semantic
{
	struct semantic_state
	{
		// keeps track of all defined functions (name mapped to the path from AST root to the function_definition node).
		std::unordered_map<std::string, ast::path_t> defined_functions = {};
	} global_state;

	std::size_t pass_number = 0;
	std::size_t error_count = 0;
	std::size_t warning_count = 0;
	bool is_meta_region = false;

	void semantic_assert(bool expr, const ast::node& node, std::string msg)
	{
		diag::assert_that(expr, std::format("on line {}: {}", node.meta.line_number, msg));
		if(!expr)
		{
			error_count++;
		}
	}

	void semantic_warning(const ast::node& node, std::string msg)
	{
		diag::warning(std::format("on line {}: {}", node.meta.line_number, msg));
		warning_count++;
	}

	template<typename T>
	void analyse_thing(const ast::node& node, const T& payload_like, const ast::path_t& path, const ast& tree);

	void analyse_unary_operator(const ast::node& node, const std::pair<ast::unary_operator, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		// we have parsed an expression as: "<UnaryOp> <Expression>"
		// some unary operators only support certain rhs values.
		// e.g minus only works on rhs values that are integer/decimal literals (or variables/expressions that narrow down into the former 2). we can emit an error for example if rhs is a string literal e.g: -"hello"
		// todo: semantic checks for the other unary operators (see ast.hpp for the unary operator types)
		const auto& [op, expr] = payload;
		if(op.type == lexer::token::type::minus)
		{
			// expr must be another expression/decimal/integer literal.
			semantic_assert
			(
				std::holds_alternative<std::pair<ast::unary_operator, util::box<ast::expression>>>(expr->expr) ||
				std::holds_alternative<std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>(expr->expr) ||
				std::holds_alternative<ast::integer_literal>(expr->expr) ||
				std::holds_alternative<ast::decimal_literal>(expr->expr) ||
				std::holds_alternative<ast::identifier>(expr->expr),
				node,
				std::format("right-side of unary operator \"{}\" must either be an integer-literal, decimal-literal, subexpression or identifier", lexer::token_type_names[static_cast<int>(op.type)])
			);
		}
		else if(op.type == lexer::token::type::bitwise_complement)
		{
			// expr must be another expression/integer literal.
			semantic_assert
			(
				std::holds_alternative<std::pair<ast::unary_operator, util::box<ast::expression>>>(expr->expr) ||
				std::holds_alternative<std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>(expr->expr) ||
				std::holds_alternative<ast::integer_literal>(expr->expr) ||
				std::holds_alternative<ast::identifier>(expr->expr),
				node,
				std::format("right-side of unary operator \"{}\" must either be an integer-literal, subexpression or identifier", lexer::token_type_names[static_cast<int>(op.type)])
			);
		}
		else if(op.type == lexer::token::type::logical_negation)
		{
			semantic_assert
			(
				std::holds_alternative<std::pair<ast::unary_operator, util::box<ast::expression>>>(expr->expr) ||
				std::holds_alternative<std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>(expr->expr) ||
				std::holds_alternative<ast::bool_literal>(expr->expr) ||
				std::holds_alternative<ast::identifier>(expr->expr),
				node,
				std::format("right-side of unary operator \"{}\" must either be a bool-literal, subexpression or identifier", lexer::token_type_names[static_cast<int>(op.type)])
			);
		}
		else
		{
			semantic_warning(node, "semantic analysis for this particular unary operator is not yet implemented.");
		}
		
		analyse_thing(node, expr->expr, path, tree);
	}

	void analyse_binary_operator(const ast::node& node, const std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		const auto& [op, expr1, expr2] = payload;
		if(op.type == lexer::token::type::plus || op.type == lexer::token::type::minus || op.type == lexer::token::type::double_equals || op.type == lexer::token::type::equals || op.type == lexer::token::type::not_equals)
		{
			semantic_assert
			(
				(
				std::holds_alternative<std::pair<ast::unary_operator, util::box<ast::expression>>>(expr1->expr) ||
				std::holds_alternative<std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>(expr1->expr) ||
				std::holds_alternative<ast::decimal_literal>(expr1->expr) ||
				std::holds_alternative<ast::integer_literal>(expr1->expr) ||
				std::holds_alternative<ast::identifier>(expr1->expr)) ||
				std::holds_alternative<ast::function_call>(expr1->expr)

				&&

				(
				std::holds_alternative<std::pair<ast::unary_operator, util::box<ast::expression>>>(expr2->expr) ||
				std::holds_alternative<std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>(expr2->expr) ||
				std::holds_alternative<ast::decimal_literal>(expr2->expr) ||
				std::holds_alternative<ast::integer_literal>(expr2->expr) ||
				std::holds_alternative<ast::identifier>(expr2->expr)) ||
				std::holds_alternative<ast::function_call>(expr2->expr),
				node,
				std::format("both sides of binary operator \"{}\" must either be an integer-literal, decimal-literal, subexpression, funciton-call or identifier", lexer::token_type_names[static_cast<int>(op.type)])

			);
		}
		else
		{
			semantic_warning(node, "semantic analysis for this particular binary operator is not yet implemented.");
		}
		analyse_thing(node, expr1->expr, path, tree);
		analyse_thing(node, expr2->expr, path, tree);
	}

	void analyse_function_call(const ast::node& node, const ast::function_call& payload, const ast::path_t& path, const ast& tree)
	{
		// we only analyse function calls in the second pass.
		if(pass_number == 0)
		{
			return;
		}
		auto iter = global_state.defined_functions.find(payload.function_name);
		semantic_assert(iter != global_state.defined_functions.end(), node, std::format("call to undefined function \"{}\" at line {}", payload.function_name, node.meta.line_number));

		const auto& func_node = tree.get(iter->second);
		const ast::function_definition& func = std::get<ast::function_definition>(func_node.payload);
		if(func.params.size() != payload.params.size())
		{
			semantic_assert(false, node, std::format("invalid call to function {}. expects {} parameters, but you provided {}", func.function_name, func.params.size(), payload.params.size()));
			std::cout << std::format("note: see below for signature of {} (line {}):\n\t{}\n", payload.function_name, func_node.meta.line_number, func.to_string());
			return;
		}
		for(std::size_t i = 0; i < func.params.size(); i++)
		{
			const ast::variable_declaration& defined_param = func.params[i];
			const ast::expression& actual_param = payload.params[i];
			// todo: do type-checking on the parameters.
			// note: function parameters cannot be semantically analysed as a normal expression, so we do something similar but bespoke here.
			analyse_thing(node, actual_param.expr, path, tree);
		}
	}

	void analyse_expression(const ast::node& node, const ast::expression& payload, const ast::path_t& path, const ast& tree)
	{
		analyse_thing(node, payload.expr, path, tree);
	}
	
	void analyse_if_statement(const ast::node& node, const ast::if_statement& payload, const ast::path_t& path, const ast& tree)
	{

	}

	void analyse_for_statement(const ast::node& node, const ast::for_statement& payload, const ast::path_t& path, const ast& tree)
	{

	}

	void analyse_return_statement(const ast::node& node, const ast::return_statement& payload, const ast::path_t& path, const ast& tree)
	{
		// todo: ensure that this return statement is actually within a function definition block.
		//		note: to do this - recursively iterate back up through the tree until you find the first function definition. if you never find one - then this return statement is invalid coz you can only return from within a function block.
		// todo: type-check the return expression. if you did the above, you can look at the function definition and figure out what its return type is.
	}

	void analyse_function_definition(const ast::node& node, const ast::function_definition& payload, const ast::path_t& path, const ast& tree)
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
			const auto& existing_node = tree.get(iter->second);
			semantic_assert(false, node, std::format("redefinition of function \"{}\" (previously defined on line {})", payload.function_name, existing_node.meta.line_number));
		}
	}

	void analyse_variable_declaration(const ast::node& node, const ast::variable_declaration& payload, const ast::path_t& path, const ast& tree)
	{
		if(pass_number != 0)
		{
			return;
		}
		if(payload.initialiser.has_value())
		{
			analyse_expression(node, payload.initialiser.value(), path, tree);
		}
	}

	void analyse_struct_definition(const ast::node& node, const ast::struct_definition& payload, const ast::path_t& path, const ast& tree)
	{

	}

	void analyse_member_access(const ast::node& node, const ast::member_access& payload, const ast::path_t& path, const ast& tree)
	{

	}

	void analyse_identifier(const ast::node& node, const ast::identifier& payload, const ast::path_t& path, const ast& tree)
	{
		// we can do this on any pass. even both
		// however, if we do it on both, then we potentially report the same error twice (and waste work).
		// function calls however only are semantically analysed on the 2nd pass. during that, we might also want to semantically analyse its parameters which could be an identifier (which would skip if we only do this on first-pass)
		// for that reason, we do it on second pass.
		if(pass_number == 0)
		{
			return;
		}
		bool variable_exists = tree.try_find_variable_from(path, payload.name).has_value();
		auto maybe_defined_function = global_state.defined_functions.find(payload.name);
		if(maybe_defined_function != global_state.defined_functions.end())
		{
			if(variable_exists)
			{
				semantic_assert(false, node, std::format("ambiguous use of previously-defined variable \"{}\" because there is a function defined with the same name on line {}. recommendation: rename one of the two.", payload.name, tree.get(maybe_defined_function->second).meta.line_number));
			}
			else
			{
				semantic_assert(false, node, std::format("attempt to reference function \"{}\" as if it were a variable.", payload.name));
			}
			return;
		}
		semantic_assert(variable_exists, node, std::format("use of undefined variable {}. could not find definition beforehand.", payload.name));
	}

	void analyse_meta_region(const ast::node& node, const ast::meta_region& payload, const ast::path_t& path, const ast& tree)
	{

	}

	template<typename P>
	void analyse_thing(const ast::node& node, const P& payload_like, const ast::path_t& path, const ast& tree)
	{
		std::visit([&node, &tree, &path](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr(std::is_same_v<T, std::monostate>
			|| std::is_same_v<T, ast::decimal_literal>
			|| std::is_same_v<T, ast::integer_literal>
			|| std::is_same_v<T, ast::char_literal>
			|| std::is_same_v<T, ast::string_literal>
			|| std::is_same_v<T, ast::bool_literal>){}
			else if constexpr(std::is_same_v<T, ast::function_call>)
			{
				analyse_function_call(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::if_statement>)
			{
				analyse_if_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::for_statement>)
			{
				analyse_for_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::return_statement>)
			{
				analyse_return_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::function_definition>)
			{
				analyse_function_definition(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::variable_declaration>)
			{
				analyse_variable_declaration(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::struct_definition>)
			{
				analyse_struct_definition(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::member_access>)
			{
				analyse_member_access(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::identifier>)
			{
				analyse_identifier(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::expression>)
			{
				analyse_expression(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::meta_region>)
			{
				analyse_meta_region(node, arg, path, tree);
			}
			else
			{
				// could be one of the unique expression variant types.
				if constexpr(std::is_same_v<T, std::pair<ast::unary_operator, util::box<ast::expression>>>)
				{
					analyse_unary_operator(node, arg, path, tree);
				}
				else if constexpr(std::is_same_v<T, std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>)
				{
					analyse_binary_operator(node, arg, path, tree);
				}
				else
				{
					semantic_assert(false, node, std::format("internal compiler error: unknown AST node type (variant id: {}) detecting during semantic analysis.", node.payload.index()));
				}
			}
		}, payload_like);
	}

	void analyse_single_node(ast::path_t path, const ast& tree)
	{
		const ast::node& node = tree.get(path);
		analyse_thing(node, node.payload, path, tree);
	}

	void analyse_node(ast::path_t path, const ast& tree)
	{
		analyse_single_node(path, tree);
		const ast::node& node = tree.get(path);
		if(std::holds_alternative<ast::meta_region>(node.payload))
		{
			// analysed the meta region node already.
			// its block does not make sense in the context of the program, so we do not semantically analyse it like it were a normal part of the AST.
			return;
		}
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			auto child_path = path;
			child_path.push_back(i);
			analyse_node(child_path, tree);
		}
	}

	void first_pass(const ast& tree)
	{
		pass_number = 0;
		analyse_node({}, tree);
	}

	void second_pass(const ast& tree)
	{
		pass_number = 1;
		analyse_node({}, tree);
	}

	void summary()
	{
		const char* col = "";
		if(error_count > 0)
		{
			col = "\033[1;31m";
		}
		else if(warning_count > 0)
		{
			col = "\033[1;33m";
		}
		diag::print(std::format("{}{} errors, {} warnings detected.{}", col, error_count, warning_count, "\033[0m"));
		if(error_count > 0)
		{
			diag::fatal_error("compilation terminating due to semantic analysis error(s).");
		}
	}

	void analysis(const ast& tree)
	{
		global_state = {};
		// we do semantic analysis in 2 passes.
		// if we do it in one-pass, then things must be defined *before* being used.
		// i want you to be able to call function foo() before you define it (so long as you do indeed define it at some point).
		//	iirc java/c#/python etc does this, but C/C++ etc doesnt. semantic analysis is pretty cheap anyways in terms of time, so im happy to take the minor perf hit to not have to deal with forward declares.
		// the first pass basically harvests semantic information (e.g function definitions, variables within each scope etc...)
		// second pass uses that information to actually perform verification.
		first_pass(tree);
		second_pass(tree);
		summary();
	}
}