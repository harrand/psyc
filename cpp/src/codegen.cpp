#include "codegen.hpp"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"

#include <format>
#include <map>
#include <memory>
namespace codegen
{
	namespace context
	{
		static std::unique_ptr<llvm::LLVMContext> ctx = nullptr;
		static std::unique_ptr<llvm::IRBuilder<>> builder = nullptr;
		static std::unique_ptr<llvm::Module> mod = nullptr;
		static std::map<std::string, llvm::Value*> named_values = {};
		static bool initialised = false;

		void initialise()
		{
			if(initialised)
			{
				return;
			}
			ctx = std::make_unique<llvm::LLVMContext>();
			mod = std::make_unique<llvm::Module>("apparantly a jit", *ctx);

			builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
			initialised = true;
		}
	}

	template<typename T>
	llvm::Value* codegen_thing(const ast::node& node, const T& payload_like, const ast::path_t& path, const ast& tree);

	llvm::Value* codegen_unary_operator(const ast::node& node, const std::pair<ast::unary_operator, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_binary_operator(const ast::node& node, const std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_function_call(const ast::node& node, const ast::function_call& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_expression(const ast::node& node, const ast::expression& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}
	
	llvm::Value* codegen_if_statement(const ast::node& node, const ast::if_statement& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_return_statement(const ast::node& node, const ast::return_statement& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_function_definition(const ast::node& node, const ast::function_definition& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_variable_declaration(const ast::node& node, const ast::variable_declaration& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_identifier(const ast::node& node, const ast::identifier& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	template<typename P>
	llvm::Value* codegen_thing(const ast::node& node, const P& payload_like, const ast::path_t& path, const ast& tree)
	{
		llvm::Value* ret = nullptr;
		std::visit([&node, &tree, &path](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr(std::is_same_v<T, std::monostate>
			|| std::is_same_v<T, ast::decimal_literal>
			|| std::is_same_v<T, ast::integer_literal>
			|| std::is_same_v<T, ast::string_literal>
			|| std::is_same_v<T, ast::bool_literal>){}
			else if constexpr(std::is_same_v<T, ast::function_call>)
			{
				codegen_function_call(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::if_statement>)
			{
				codegen_if_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::return_statement>)
			{
				codegen_return_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::function_definition>)
			{
				codegen_function_definition(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::variable_declaration>)
			{
				codegen_variable_declaration(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::identifier>)
			{
				codegen_identifier(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::expression>)
			{
				codegen_expression(node, arg, path, tree);
			}
			else
			{
				// could be one of the unique expression variant types.
				if constexpr(std::is_same_v<T, std::pair<ast::unary_operator, util::box<ast::expression>>>)
				{
					codegen_unary_operator(node, arg, path, tree);
				}
				else if constexpr(std::is_same_v<T, std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>)
				{
					codegen_binary_operator(node, arg, path, tree);
				}
				else
				{
					semantic_assert(false, node, std::format("internal compiler error: unknown AST node type (variant id: {}) detecting during codegen.", node.payload.index()));
				}
			}
		}, payload_like);
		return ret;
	}

	void codegen_single_node(ast::path_t path, const ast& tree)
	{
		const ast::node& node = tree.get(path);
		llvm::Value* val = codegen_thing(node, node.payload, path, tree);
	}

	void codegen_node(ast::path_t path, const ast& tree)
	{
		codegen_single_node(path, tree);
		const ast::node& node = tree.get(path);
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			auto child_path = path;
			child_path.push_back(i);
			codegen_node(child_path, tree);
		}
	}

	void generate(const ast& tree)
	{
		context::initialise();
		codegen_single_node({}, tree);
	}
}