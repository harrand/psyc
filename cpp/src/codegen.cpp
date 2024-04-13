#include "codegen.hpp"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
// to print out module directly to string.
#include "llvm/Support/raw_os_ostream.h"


#include <format>
#include <map>
#include <memory>
#include <stack>
namespace codegen
{
	namespace context
	{
		static std::unique_ptr<llvm::LLVMContext> ctx = nullptr;
		static std::stack<llvm::IRBuilder<>> builders = {};
		//static std::unique_ptr<llvm::IRBuilder<>> builder = nullptr;
		static std::unique_ptr<llvm::Module> mod = nullptr;
		static std::map<std::string, llvm::Value*> named_values = {};
		static llvm::BasicBlock* entry_point = nullptr;
		static bool initialised = false;

		llvm::IRBuilder<>& current_builder()
		{
			return builders.top();
		}

		void initialise()
		{
			if(initialised)
			{
				return;
			}
			ctx = std::make_unique<llvm::LLVMContext>();
			mod = std::make_unique<llvm::Module>("apparantly a jit", *ctx);

			builders.emplace(*ctx);
			initialised = true;
		}
	}

	llvm::Type* get_llvm_type(std::string type_name)
	{
		if(type_name == "i64" || type_name == "u64")
		{
			return llvm::Type::getInt64Ty(*context::ctx);
		}
		else if(type_name == "i32" || type_name == "u32")
		{
			return llvm::Type::getInt32Ty(*context::ctx);
		}
		else if(type_name == "i16" || type_name == "u16")
		{
			return llvm::Type::getInt16Ty(*context::ctx);
		}
		else if(type_name == "bool")
		{
			return llvm::Type::getInt1Ty(*context::ctx);
		}
		else if(type_name == "string")
		{
			return llvm::PointerType::get(llvm::IntegerType::get(*context::ctx, 8), 0);
		}
		else if(type_name == "u0")
		{
			return llvm::Type::getVoidTy(*context::ctx);
		}
		diag::error(std::format("internal compiler error: cannot recognise type {} in the context of LLVM-IR", type_name));
		return nullptr;
	}

	template<typename T>
	llvm::Value* codegen_thing(const ast::node& node, const T& payload_like, const ast::path_t& path, const ast& tree);

	llvm::Value* codegen_decimal_literal(const ast::node& node, const ast::decimal_literal& payload, const ast::path_t& path, const ast& tree)
	{
		return llvm::ConstantFP::get(*context::ctx, llvm::APFloat{payload.val});
	}

	llvm::Value* codegen_integer_literal(const ast::node& node, const ast::integer_literal& payload, const ast::path_t& path, const ast& tree)
	{
		return llvm::ConstantInt::get(*context::ctx, llvm::APInt{64, static_cast<std::uint64_t>(payload.val), true});
	}

	llvm::Value* codegen_string_literal(const ast::node& node, const ast::string_literal& payload, const ast::path_t& path, const ast& tree)
	{
		return llvm::ConstantDataArray::getString(*context::ctx, payload.val, true);
	}

	llvm::Value* codegen_bool_literal(const ast::node& node, const ast::bool_literal& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_expression(const ast::node& node, const ast::expression& payload, const ast::path_t& path, const ast& tree);

	llvm::Value* codegen_unary_operator(const ast::node& node, const std::pair<ast::unary_operator, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		llvm::Value* operand_value = codegen_expression(node, *payload.second, path, tree);
		switch(payload.first.type)
		{
			case lexer::token::type::minus:
				return context::current_builder().CreateNeg(operand_value);
			break;
			default:
				diag::error("internal compiler error: a particular unary operator (nearby to line {}) was not recognised in the context of its equivalent LLVM-IR.", node.meta.line_number);
				return nullptr;
			break;
		}
	}

	llvm::Value* codegen_binary_operator(const ast::node& node, const std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		const auto&[op, lhs, rhs] = payload;
		llvm::Value* lhs_value = codegen_expression(node, *lhs, path, tree);
		llvm::Value* rhs_value = codegen_expression(node, *rhs, path, tree);

		switch(op.type)
		{
			case lexer::token::type::plus:
				return context::current_builder().CreateAdd(lhs_value, rhs_value);
			break;
			case lexer::token::type::minus:
				return context::current_builder().CreateSub(lhs_value, rhs_value);
			break;
			default:
				diag::error("internal compiler error: a particular binary operator (nearby to line {}) was not recognised in the context of its equivalent LLVM-IR.", node.meta.line_number);
				return nullptr;
			break;
		}
	}

	llvm::Value* codegen_function_call(const ast::node& node, const ast::function_call& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	llvm::Value* codegen_expression(const ast::node& node, const ast::expression& payload, const ast::path_t& path, const ast& tree)
	{
		return codegen_thing(node, payload.expr, path, tree);
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
		llvm::Type* return_type = get_llvm_type(payload.return_type);
		std::vector<llvm::Type*> param_types;
		param_types.reserve(payload.params.size());
		for(const auto& param : payload.params)
		{
			llvm::Type* param_type = get_llvm_type(param.type_name);
			diag::assert_that(param_type != nullptr, std::format("internal compiler error: parameter \"{}\" of function \"{}\" failed to be resolved to a valid LLVM type.", param.var_name, payload.function_name));
			param_types.push_back(param_type);
		}
		llvm::FunctionType* fty = llvm::FunctionType::get(return_type, param_types, false);
		llvm::Function* function = llvm::Function::Create(fty, llvm::Function::ExternalLinkage, payload.function_name, *context::mod);
		// function_name "main" is an entry point.
		llvm::BasicBlock* entry_block = llvm::BasicBlock::Create(*context::ctx, "function_entry_point", function);
		if(payload.function_name == "main")
		{
			context::entry_point = entry_block;
		}
		std::size_t arg_counter = 0;
		for(llvm::Argument& arg : function->args())
		{
			arg.setName(payload.params[arg_counter].var_name);
			arg_counter++;
		}
		// new block. child nodes now get processed.
		context::builders.emplace(entry_block);
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			const auto& child = node.children[i];
			ast::path_t child_path = path;
			child_path.push_back(i);
			codegen_thing(child, child.payload, child_path, tree);
		}
		context::builders.pop();
		return nullptr;
	}

	llvm::Value* codegen_variable_declaration(const ast::node& node, const ast::variable_declaration& payload, const ast::path_t& path, const ast& tree)
	{
		llvm::Type* ty = get_llvm_type(payload.type_name);
		llvm::Value* var = context::current_builder().CreateAlloca(ty, nullptr, payload.var_name);
		if(payload.initialiser.has_value())
		{
			llvm::Value* init_value = codegen_expression(node, payload.initialiser.value(), path, tree);
			diag::assert_that(init_value != nullptr, "internal compiler error: variable declaration initialiser expression did not codegen correctly - returned a null LLVM value.");
			context::current_builder().CreateStore(init_value, var);
		}
		return var;
	}

	llvm::Value* codegen_identifier(const ast::node& node, const ast::identifier& payload, const ast::path_t& path, const ast& tree)
	{
		return nullptr;
	}

	template<typename P>
	llvm::Value* codegen_thing(const ast::node& node, const P& payload_like, const ast::path_t& path, const ast& tree)
	{
		llvm::Value* ret = nullptr;
		std::visit([&node, &tree, &path, &ret](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr(std::is_same_v<T, std::monostate>){}
			else if constexpr(std::is_same_v<T, ast::decimal_literal>)
			{
				ret = codegen_decimal_literal(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::integer_literal>)
			{
				ret = codegen_integer_literal(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::string_literal>)
			{
				ret = codegen_string_literal(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::bool_literal>)
			{
				ret = codegen_bool_literal(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::function_call>)
			{
				ret = codegen_function_call(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::if_statement>)
			{
				ret = codegen_if_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::return_statement>)
			{
				ret = codegen_return_statement(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::function_definition>)
			{
				ret = codegen_function_definition(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::variable_declaration>)
			{
				ret = codegen_variable_declaration(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::identifier>)
			{
				ret = codegen_identifier(node, arg, path, tree);
			}
			else if constexpr(std::is_same_v<T, ast::expression>)
			{
				ret = codegen_expression(node, arg, path, tree);
			}
			else
			{
				// could be one of the unique expression variant types.
				if constexpr(std::is_same_v<T, std::pair<ast::unary_operator, util::box<ast::expression>>>)
				{
					ret = codegen_unary_operator(node, arg, path, tree);
				}
				else if constexpr(std::is_same_v<T, std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>>)
				{
					ret = codegen_binary_operator(node, arg, path, tree);
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

	void generate(const ast& tree)
	{
		context::initialise();
		const auto& root = tree.get({});
		for(std::size_t i = 0; i < root.children.size(); i++)
		{
			codegen_single_node(ast::path_t{i}, tree);
		}

		std::string ir_string;
		llvm::raw_string_ostream os{ir_string};
		context::mod->print(os, nullptr);
		diag::message(std::format("llvm ir: \n{}", ir_string));
	}
}