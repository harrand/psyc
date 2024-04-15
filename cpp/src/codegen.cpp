#include "codegen.hpp"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h" 

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

		struct scope_reference
		{
			std::unordered_map<std::string, llvm::AllocaInst*> variables = {};
			std::unordered_map<std::size_t, scope_reference> children = {};
			llvm::Function* current_function = nullptr;
		};
		static scope_reference scope_manager;

		void register_variable(ast::path_view_t path, std::string variable_name, llvm::AllocaInst* value)
		{
			diag::assert_that(value != nullptr, std::format("internal compiler error: nullptr llvm::Value* passed for variable name {}", variable_name));
			scope_reference* current_scope = &scope_manager;
			if(path.size())
			{
				for(std::size_t idx : path.subspan(0, path.size() - 1))
				{
					current_scope = &current_scope->children[idx];
				}
			}
			current_scope->variables[variable_name] = value;
		}

		void register_function(ast::path_view_t path, llvm::Function* func)
		{
			diag::assert_that(func != nullptr, "internal compiler error: nullptr llvm::Function* passed");
			scope_reference* current_scope = &scope_manager;
			if(path.size())
			{
				for(std::size_t idx : path.subspan(0, path.size() - 1))
				{
					current_scope = &current_scope->children[idx];
				}
			}
			current_scope->current_function = func;
		}

		llvm::Function* get_enclosing_function(ast::path_view_t path)
		{
			std::vector<llvm::Function*> func_stack = {};
			scope_reference* current_scope = &scope_manager;
			if(path.size())
			{
				for(std::size_t idx : path)
				{
					if(current_scope->current_function != nullptr)
					{
						func_stack.push_back(current_scope->current_function);
					}
					current_scope = &current_scope->children[idx];
				}
			}
			if(func_stack.size())
			{
				return func_stack.back();
			}
			return nullptr;
		}

		llvm::AllocaInst* try_find_variable(ast::path_view_t path, std::string variable_name)
		{
			scope_reference* current_scope = &scope_manager;
			if(path.size())
			{
				for(std::size_t idx : path)
				{
					auto maybe_var_loc = current_scope->variables.find(variable_name);
					if(maybe_var_loc != current_scope->variables.end())
					{
						return maybe_var_loc->second;
					}
					current_scope = &current_scope->children[idx];	
				}
			}
			return nullptr;
		}

		llvm::IRBuilder<>& current_builder()
		{
			return builders.top();
		}

		void initialise(std::string filename)
		{
			filename += ".psy";
			ctx = std::make_unique<llvm::LLVMContext>();
			mod = std::make_unique<llvm::Module>(filename, *ctx);

			builders.emplace(*ctx);
		}

		void terminate()
		{
			entry_point = nullptr;
			named_values.clear();
			builders = {};
			mod = nullptr;
			ctx = nullptr;
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
		else if(type_name == "i8" || type_name == "u8")
		{
			return llvm::Type::getInt8Ty(*context::ctx);
		}
		else if(type_name == "f64")
		{
			return llvm::Type::getDoubleTy(*context::ctx);
		}
		else if(type_name == "f32")
		{
			return llvm::Type::getFloatTy(*context::ctx);
		}
		else if(type_name == "f16")
		{
			return llvm::Type::getHalfTy(*context::ctx);
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

	llvm::Value* dereference_ptr(llvm::Value* ptr)
	{
		if(ptr->getType()->isPointerTy())
		{
			llvm::Type* value_type = static_cast<llvm::AllocaInst*>(ptr)->getAllocatedType();
			//return context::current_builder().CreateLoad(var->getAllocatedType(), var, payload.name);
			ptr = context::current_builder().CreateLoad(value_type, ptr, ptr->getName());
		}
		return ptr;
	}

	/*
	// warning: this code is cursed. caused me infinite loops in generated code.
	void try_narrow(llvm::Value*& lhs, llvm::Value*& rhs)
	{
		bool prefer_left = lhs->getType()->getIntegerBitWidth() > rhs->getType()->getIntegerBitWidth();
		if(prefer_left)
		{
			rhs = context::current_builder().CreateSExt(rhs, lhs->getType());
		}
		else
		{
			lhs = context::current_builder().CreateSExt(lhs, rhs->getType());
		}
	}
	*/

	void try_narrow_rhs(llvm::Value* lhs, llvm::Value*& rhs)
	{
		bool rhs_must_shrink = lhs->getType()->getIntegerBitWidth() < rhs->getType()->getIntegerBitWidth();
		if(rhs_must_shrink)
		{
			rhs = context::current_builder().CreateTrunc(rhs, lhs->getType());
		}
		else
		{
			rhs = context::current_builder().CreateSExt(rhs, lhs->getType());
		}
	}

	template<typename T>
	llvm::Value* codegen_thing(const ast::node& node, const T& payload_like, const ast::path_t& path, const ast& tree);

	llvm::Value* codegen_char_literal(const ast::node& node, const ast::char_literal& payload, const ast::path_t& path, const ast& tree)
	{
		return llvm::ConstantInt::get(*context::ctx, llvm::APInt{8, static_cast<std::uint64_t>(payload.val)});
	}

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
		return llvm::ConstantInt::get(*context::ctx, llvm::APInt{1, payload.val ? 1u : 0u, true});
	}

	llvm::Value* codegen_expression(const ast::node& node, const ast::expression& payload, const ast::path_t& path, const ast& tree);

	llvm::Value* codegen_unary_operator(const ast::node& node, const std::pair<ast::unary_operator, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		llvm::Value* operand_value = codegen_expression(node, *payload.second, path, tree);
		switch(payload.first.type)
		{
			case lexer::token::type::minus:
				if(operand_value->getType()->isIntegerTy())
				{
					return context::current_builder().CreateNeg(operand_value);
				}
				else if(operand_value->getType()->isFloatingPointTy())
				{
					return context::current_builder().CreateFNeg(operand_value);
				}
				else
				{
					diag::error("internal compiler error: unary operator `-` used on expression that was neither an integral type nor a floating-point type. however, this ought to have been caught by semantic analysis. please submit a bug report.");
					return nullptr;
				}
			break;
			default:
				diag::error(std::format("internal compiler error: a particular unary operator (nearby to line {}) was not recognised in the context of its equivalent LLVM-IR.", node.meta.line_number));
				return nullptr;
			break;
		}
	}

	llvm::Value* codegen_binary_operator(const ast::node& node, const std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>& payload, const ast::path_t& path, const ast& tree)
	{
		const auto&[op, lhs, rhs] = payload;
		llvm::Value* lhs_value = codegen_expression(node, *lhs, path, tree);
		llvm::Value* rhs_value = codegen_expression(node, *rhs, path, tree);
		llvm::Type* lhs_t = lhs_value->getType();
		llvm::Type* rhs_t = rhs_value->getType();

		const bool want_lhs_ptr = op.type == lexer::token::type::equals;
		if(!want_lhs_ptr && lhs_t != rhs_t)
		{
			// if the types dont match...
			// is lhs T* where rhs is T? then we dereference.
			lhs_value = dereference_ptr(lhs_value);
			lhs_t = lhs_value->getType();
		}

		// i.e they are both integers but of different size/signedness.
		if(lhs_t != rhs_t && lhs_t->isIntegerTy() && rhs_t->isIntegerTy())
		{
			// warning: can have cursed results. disabled for now (meaning that assignments additions etc must have exact same integer types.)
			//try_narrow(lhs_value, rhs_value);
		}

		switch(op.type)
		{
			case lexer::token::type::plus:
				return context::current_builder().CreateAdd(lhs_value, rhs_value);
			break;
			case lexer::token::type::minus:
				return context::current_builder().CreateSub(lhs_value, rhs_value);
			break;
			case lexer::token::type::double_equals:
				return context::current_builder().CreateICmpEQ(lhs_value, rhs_value);
			break;
			case lexer::token::type::not_equals:
				return context::current_builder().CreateICmpNE(lhs_value, rhs_value);
			break;
			case lexer::token::type::equals:
			{
				return context::current_builder().CreateStore(rhs_value, lhs_value);
			}
			break;
			default:
				diag::error("internal compiler error: a particular binary operator (nearby to line {}) was not recognised in the context of its equivalent LLVM-IR.", node.meta.line_number);
				return nullptr;
			break;
		}
	}

	llvm::Value* codegen_function_call(const ast::node& node, const ast::function_call& payload, const ast::path_t& path, const ast& tree)
	{
		llvm::Function* func = context::mod->getFunction(payload.function_name);
		diag::assert_that(func != nullptr, std::format("internal compiler error: could not locate function {} within LLVM module.", payload.function_name));
		std::vector<llvm::Value*> param_values = {};
		for(std::size_t i = 0; i < payload.params.size(); i++)
		{
			const ast::expression& param = payload.params[i];
			llvm::Value* param_val = codegen_expression(node, param, path, tree);

			llvm::Argument* arg = func->getArg(i);
			// types dont match. probably a pointer we need to dereference.
			if(param_val->getType() != arg->getType() && param_val->getType()->isPointerTy())
			{
				param_val = dereference_ptr(param_val);
			}
			if(param_val->getType()->isIntegerTy() && arg->getType()->isIntegerTy() && (param_val->getType() != arg->getType()))
			{
				// integers of different bits/signedness. try narrowing.
				try_narrow_rhs(arg, param_val);
			}
			diag::assert_that(param_val != nullptr, std::format("internal compiler error: retrieving LLVM value for parameter {} in function call {} yielded nullptr.", param_values.size(), payload.function_name));
			param_values.push_back(param_val);
		}
		return context::current_builder().CreateCall(func, param_values, func->getReturnType() == llvm::Type::getVoidTy(*context::ctx) ? "" : "calltmp");
	}

	llvm::Value* codegen_expression(const ast::node& node, const ast::expression& payload, const ast::path_t& path, const ast& tree)
	{
		return codegen_thing(node, payload.expr, path, tree);
	}
	
	llvm::Value* codegen_if_statement(const ast::node& node, const ast::if_statement& payload, const ast::path_t& path, const ast& tree)
	{
		llvm::Value* cond_value = codegen_expression(node, payload.condition, path, tree);
		diag::assert_that(cond_value != nullptr, std::format("internal compiler error: if-condition expression codegen yielded nullptr."));
		cond_value = context::current_builder().CreateICmpNE(cond_value, llvm::ConstantInt::get(cond_value->getType(), llvm::APInt{cond_value->getType()->getIntegerBitWidth(), 0u}), "ifcond");

		llvm::Function* enclosing_function = context::get_enclosing_function(path);
		llvm::BasicBlock* if_true = llvm::BasicBlock::Create(*context::ctx, "then", enclosing_function);
		llvm::BasicBlock* after_if = llvm::BasicBlock::Create(*context::ctx, "after_if");
		context::current_builder().CreateCondBr(cond_value, if_true, after_if);

		enclosing_function->insert(enclosing_function->end(), after_if);

		context::builders.emplace(if_true);
		// note: basicblocks cannot have terminators in the middle, only as the last instruction.
		// remember a terminator is a branch/ret
		// so if our if statement block contains a return, it already has a terminator. that means adding the branch to to after-if is erroneous.
		// hence this if_block_contains_return check.
		bool if_block_contains_return = false;
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			const auto& child = node.children[i];
			if_block_contains_return |= std::holds_alternative<ast::return_statement>(child.payload);
			ast::path_t child_path = path;
			child_path.push_back(i);
			codegen_thing(child, child.payload, child_path, tree);
		}
		if(!if_block_contains_return)
		{
			context::current_builder().CreateBr(after_if);
		}
		context::builders.pop();
		context::builders.pop(); context::builders.emplace(after_if);
		return cond_value;
	}

	llvm::Value* codegen_for_statement(const ast::node& node, const ast::for_statement& payload, const ast::path_t& path, const ast& tree)
	{
		// first thing is easy - codegen the initial expression.
		llvm::Value* start = codegen_expression(node, payload.start, path, tree);
		diag::assert_that(start != nullptr, "fooey");

		llvm::Function* enclosing_function = context::get_enclosing_function(path);
		llvm::BasicBlock* loop_block = llvm::BasicBlock::Create(*context::ctx, "for_loop", enclosing_function);
		llvm::BasicBlock* after_for = llvm::BasicBlock::Create(*context::ctx, "after_for");
		context::current_builder().CreateCondBr(codegen_expression(node, payload.end, path, tree), loop_block, after_for);
		enclosing_function->insert(enclosing_function->end(), after_for);
		context::builders.emplace(loop_block);
		// note: basicblocks cannot have terminators in the middle, only as the last instruction.
		// remember a terminator is a branch/ret
		// so if our if statement block contains a return, it already has a terminator. that means adding the branch to to after-if is erroneous.
		// hence this if_block_contains_return check.
		bool if_block_contains_return = false;
		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			const auto& child = node.children[i];
			if_block_contains_return |= std::holds_alternative<ast::return_statement>(child.payload);
			ast::path_t child_path = path;
			child_path.push_back(i);
			codegen_thing(child, child.payload, child_path, tree);
		}
		codegen_expression(node, payload.loop, path, tree);
		if(!if_block_contains_return)
		{
			context::current_builder().CreateCondBr(codegen_expression(node, payload.end, path, tree), loop_block, after_for);
		}
		context::builders.pop();
		context::builders.pop(); context::builders.emplace(after_for);
		return nullptr;
	}

	llvm::Value* codegen_return_statement(const ast::node& node, const ast::return_statement& payload, const ast::path_t& path, const ast& tree)
	{
		if(!payload.value.has_value())
		{
			context::current_builder().CreateRetVoid();
			return nullptr;
		}
		llvm::Value* retval = codegen_expression(node, payload.value.value(), path, tree);
		diag::assert_that(retval != nullptr, "fooey");
		context::current_builder().CreateRet(dereference_ptr(retval));
		return retval;
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
		std::size_t arg_counter = 0;
		for(llvm::Argument& arg : function->args())
		{
			arg.setName(payload.params[arg_counter].var_name);
			arg_counter++;
		}
		context::register_function(path, function);
		if(!payload.is_extern)
		{
			llvm::BasicBlock* entry_block = llvm::BasicBlock::Create(*context::ctx, "entry", function);
			// function_name "main" is an entry point.
			if(payload.function_name == "main")
			{
				context::entry_point = entry_block;
			}
			context::builders.emplace(entry_block);
			std::size_t scope_level = context::builders.size();
			for(std::size_t i = 0; i < node.children.size(); i++)
			{
				const auto& child = node.children[i];
				ast::path_t child_path = path;
				child_path.push_back(i);
				codegen_thing(child, child.payload, child_path, tree);
			}
			while(context::builders.size() > scope_level)
			{
				context::builders.pop();
			}
		}
		return nullptr;
	}

	llvm::AllocaInst* codegen_variable_declaration(const ast::node& node, const ast::variable_declaration& payload, const ast::path_t& path, const ast& tree)
	{
		llvm::Type* ty = get_llvm_type(payload.type_name);
		llvm::AllocaInst* var = context::current_builder().CreateAlloca(ty, nullptr, payload.var_name);
		if(payload.initialiser.has_value())
		{
			llvm::Value* init_value = codegen_expression(node, payload.initialiser.value(), path, tree);
			diag::assert_that(init_value != nullptr, "internal compiler error: variable declaration initialiser expression did not codegen correctly - returned a null LLVM value.");
			context::current_builder().CreateStore(init_value, var);
		}
		context::register_variable(path, payload.var_name, var);
		return var;
	}

	// note: 
	llvm::Value* codegen_identifier(const ast::node& node, const ast::identifier& payload, const ast::path_t& path, const ast& tree)
	{
		// is it a local variable?
		// var is the real thing.
		llvm::AllocaInst* var = context::try_find_variable(path, payload.name);
		if(var != nullptr)
		{
			// this is a copy.
			//return context::current_builder().CreateLoad(var->getAllocatedType(), var, payload.name);
			return var;
		}

		// how about a function parameter?
		llvm::Function* enclosing_function = context::get_enclosing_function(path);
		if(enclosing_function != nullptr)
		{
			llvm::Argument* arg;
			for(auto iter = enclosing_function->arg_begin(); iter != enclosing_function->arg_end(); iter++)
			{
				if(iter->getName() == payload.name)
				{
					return &*iter;
				}
			}
		}
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
			else if constexpr(std::is_same_v<T, ast::char_literal>)
			{
				ret = codegen_char_literal(node, arg, path, tree);
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
			else if constexpr(std::is_same_v<T, ast::for_statement>)
			{
				ret = codegen_for_statement(node, arg, path, tree);
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

	std::filesystem::path generate(const ast& tree, std::string filename)
	{
		context::initialise(filename);
		filename += ".o";
		const auto& root = tree.get({});
		for(std::size_t i = 0; i < root.children.size(); i++)
		{
			codegen_single_node(ast::path_t{i}, tree);
		}

		std::string ir_string;
		llvm::raw_string_ostream os{ir_string};
		context::mod->print(os, nullptr);
		diag::message(std::format("llvm ir: \n{}", ir_string));

		auto target_triple = llvm::sys::getDefaultTargetTriple();
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		std::string error;
		auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
		if(target == nullptr)
		{
			diag::error(std::format("error while retrieving LLVM output target information(s): {}", error));
		}
		const char* cpu = "generic";
		const char* features = "";
		llvm::TargetOptions opt;
		auto target_machine = target->createTargetMachine(target_triple, cpu, features, opt, llvm::Reloc::PIC_);
		// configure module (no i have no idea whats going on).
		context::mod->setDataLayout(target_machine->createDataLayout());
		context::mod->setTargetTriple(target_triple);
		std::error_code ec;
		llvm::raw_fd_ostream dst(filename, ec, llvm::sys::fs::OF_None);
		if(ec)
		{
			diag::error(std::format("error while generating object files: {}", ec.message()));
		}
		llvm::legacy::PassManager pass;
		auto file_type = llvm::CodeGenFileType::ObjectFile;
		if(target_machine->addPassesToEmitFile(pass, dst, nullptr, file_type, false))
		{
			diag::error(std::format("target machine cannot emit a file of this type."));
		}
		pass.run(*context::mod);
		dst.flush();

		context::terminate();
		return std::filesystem::path{filename};
	}
}