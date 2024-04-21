#include "codegen.hpp"
#include "diag.hpp"
#include "semantic.hpp"
#include "llvm/IR/LLVMContext.h"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h" 

namespace codegen
{
	static std::unique_ptr<llvm::LLVMContext> ctx = nullptr;
	static std::unique_ptr<llvm::Module> program = nullptr;
	static std::unique_ptr<llvm::IRBuilder<>> builder = nullptr;
	// global variables are RAII objects meaning we will have to deal with their lifetimes. to do that, we're just chucking them in there, and spreading the underlying ptr all around the place (as we're allocing we have pointer stability so its safe).
	static std::vector<std::unique_ptr<llvm::GlobalVariable>> global_variable_storage = {};

	void static_initialise()
	{
		ctx = std::make_unique<llvm::LLVMContext>();
	}

	void static_terminate()
	{
		diag::assert_that(program == nullptr, "internal compiler error: a llvm Module was not destroyed prior to static terminate");
		ctx = nullptr;
	}

	void cleanup_program()
	{
		// note: tearing down all this state is actually ridiculously error prone.
		// cant unlink globals until the program stops referencing them.
		// globals must all be destroyed before program itself dies.
		// so:
		// 1.) program stops referencing everything
		if(program != nullptr)
		{
			program->dropAllReferences();
			// 2.) unlink globals from parent (but doesnt erase them)
			for(auto& glob_ptr : global_variable_storage)
			{
				glob_ptr->removeFromParent();
			}
		}
		// 3.) kill globals and then program etc...

		global_variable_storage.clear();
		builder = nullptr;
		program = nullptr;
	}

	void codegen_structs(const ast& tree, const semantic::state& state);
	void codegen_functions(const ast& tree, const semantic::state& state);
	void codegen_global_variables(const ast& tree, const semantic::state& state);
	void codegen_single_node(const ast& tree, const semantic::state& state, ast::path_t path);

	// generate the program according to the ast and state.
	// resultant module will be left in `program`.
	void generate(const ast& tree, const semantic::state& state, std::string llvm_module_name)
	{
		diag::assert_that(ctx != nullptr, "internal compiler error: forgot to codegen::static_initialise");
		diag::assert_that(program == nullptr, "internal compiler error: call to codegen::generate when a previous program was not popped/written.");
		diag::assert_that(builder == nullptr, "internal compiler error: call to codegen::generate when a previous program's llvm-ir-builder was not popped/written.");
		program = std::make_unique<llvm::Module>(llvm_module_name, *ctx);
		builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

		// codegen the structs and functions first, as other code might use them before they're defined (which we allow).
		codegen_structs(tree, state);
		codegen_functions(tree, state);
		codegen_global_variables(tree, state);

		for(std::size_t i = 0; i < tree.program.children.size(); i++)
		{
			codegen_single_node(tree, state, ast::path_t{i});
		}
		llvm::verifyModule(*program);
	}

	std::string get_ir()
	{
		std::string ir_string;
		llvm::raw_string_ostream os{ir_string};
		program->print(os, nullptr);
		return ir_string;
	}

	// given a previous `generate`, retrieve an owning pointer to the result.
	std::unique_ptr<llvm::Module> pop()
	{
		std::unique_ptr<llvm::Module> ret = std::move(program);
		cleanup_program();
		return ret;
	}

	// given a previous `generate`, write the resultant program to an object filename.
	void write_to_object_file(std::filesystem::path object_filename)
	{
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
		program->setDataLayout(target_machine->createDataLayout());
		program->setTargetTriple(target_triple);
		std::error_code ec;
		llvm::raw_fd_ostream dst(object_filename.string(), ec, llvm::sys::fs::OF_None);
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
		pass.run(*program);
		dst.flush();
		// clear out the module afterwards.
		cleanup_program();
	}

	/////////////////////////////////////// TYPE SYSTEM ///////////////////////////////////////

	// we have a nice organised type system.
	// llvm's is much more complicated. thanks to opaque pointers, it's not that simple. for that reason, we deal with our own types as much as possible, and just convert it to its equivalent llvm type at the last possible second (thats what this function does).
	// returns nullptr in the case of an undefined type.
	// will ICE if you provided a really dodgy type.
	llvm::Type* as_llvm_type(const type& ty, const semantic::state& state)
	{
		if(ty.is_undefined())
		{
			// todo: potentially emit a compiler error? is it ever valid to pass in an undefined type?
			return nullptr;
		}
		if(ty.is_struct())
		{
			const semantic::struct_t* structdata = state.try_find_struct(ty.name());
			diag::assert_that(structdata != nullptr, std::format("internal compiler error: could not find struct named \"{}\" within the semantic analysis state.", ty.name()));
			diag::assert_that(structdata->userdata != nullptr, std::format("internal compiler error: found struct named \"{}\" but its userdata ptr is null, meaning it has not been codegen'd and i currently need it while evaluating something else.", ty.name()));
			// see codegen_structs to confirm that indeed it is a llvm::StructType*
			return static_cast<llvm::StructType*>(structdata->userdata);
		}
		if(ty.is_primitive())
		{
			switch(ty.as_primitive())
			{
				case primitive_type::i64:
					[[fallthrough]];
				case primitive_type::u64:
					return llvm::Type::getInt64Ty(*ctx);
				break;

				case primitive_type::i32:
					[[fallthrough]];
				case primitive_type::u32:
					return llvm::Type::getInt32Ty(*ctx);
				break;

				case primitive_type::i16:
					[[fallthrough]];
				case primitive_type::u16:
					return llvm::Type::getInt16Ty(*ctx);
				break;

				case primitive_type::i8:
					[[fallthrough]];
				case primitive_type::u8:
					return llvm::Type::getInt8Ty(*ctx);
				break;

				case primitive_type::u0:
					return llvm::Type::getVoidTy(*ctx);
				break;

				case primitive_type::boolean:
					return llvm::Type::getInt1Ty(*ctx);
				break;

				case primitive_type::f64:
					return llvm::Type::getDoubleTy(*ctx);
				break;
				case primitive_type::f32:
					return llvm::Type::getFloatTy(*ctx);
				break;
				case primitive_type::f16:
					return llvm::Type::getHalfTy(*ctx);
				break;

				default:
					diag::fatal_error(std::format("internal compiler error: unrecognised primitive type \"{}\" used in source", ty.name()));
					return nullptr;
				break;
			}
		}
		// todo: implement
		diag::fatal_error(std::format("internal compiler error: could not convert type \"{}\" to its corresponding llvm::Type*", ty.name()));
		return nullptr;
	}

	llvm::Type* as_llvm_type(const util::box<type>& ty, const semantic::state& state)
	{
		return as_llvm_type(*ty, state);
	}

	llvm::Value* load_as(llvm::Value* ptr, const semantic::state& state, type hint = type::undefined())
	{
		llvm::Type* llvm_ty = ptr->getType();
		llvm::Type* llvm_hint = as_llvm_type(hint, state);
		if(llvm_ty == llvm_hint)
		{
			// you're fine as you are.
			return ptr;
		}
		// ok types arent the same.
		if(llvm_ty->isPointerTy() && !hint.is_pointer())
		{
			// so you gave me a pointer but what you expected is not a pointer.
			// perhaps you want to do a load?
			return builder->CreateLoad(llvm_hint, ptr);
		}
		if(!llvm_ty->isPointerTy() && hint.is_pointer())
		{
			// you gave me a value but you want a pointer...
			diag::fatal_error("type system panic! dont know what to do here");
		}
		if(llvm_ty->isPointerTy() && hint.is_pointer())
		{
			// they're both pointers, but also different types...
			// dont opaque pointers make this impossible?
			diag::fatal_error("type system panic! whats an opaque pointer lmao");
		}
		// ok they are both non-pointers of different types.
		// are they both ints?
		if(llvm_ty->isIntegerTy() && llvm_hint->isIntegerTy())
		{
			// need to do integer promotion.
			std::size_t incorrect_bits = llvm_ty->getIntegerBitWidth();
			std::size_t correct_bits = llvm_hint->getIntegerBitWidth();
			if(incorrect_bits < correct_bits)
			{
				return builder->CreateZExtOrBitCast(ptr, llvm_hint);
			}
			else if(incorrect_bits > correct_bits)
			{
				return builder->CreateTruncOrBitCast(ptr, llvm_hint);
			}
			else
			{
				diag::fatal_error(std::format("load_as received two different types but they are both {}-bit integers. perhaps signedness issue? not sure how to fix that yet im afraid.", correct_bits));
			}
		}
		// pretty sure the code fucked this up as they are both non-pointers but of different types - let the caller handle this.
		return nullptr;
	}

	/////////////////////////////////////// TOP-LEVEL CODEGEN ///////////////////////////////////////

	void codegen_a_struct(const semantic::struct_t& structdata, const semantic::state& state)
	{
		if(structdata.userdata != nullptr)
		{
			// already done this one. stop.
			return;
		}
		std::vector<llvm::Type*> llvm_data_members;
		for(const struct_type::data_member& member : structdata.ty.data_members)
		{
			if(member.type->is_struct())
			{
				struct_type member_struct_ty = member.type->as_struct();	
				if(member_struct_ty.name == structdata.ty.name)
				{
					// the struct is a data member of itself.
					diag::fatal_error(std::format("struct \"{}\"'s data member called \"{}\" with the exact same struct type. recursive structs are not supported.", structdata.ty.name, member.member_name));
				}
				// ok, a data member is another struct, which we may not have codegen'd yet (thus as_llvm_type will fail).
				// lets just codegen it now.
				codegen_a_struct(*state.try_find_struct(member.type->as_struct().name), state);
			}
			llvm_data_members.push_back(as_llvm_type(member.type, state));
		}

		llvm::StructType* llvm_ty = llvm::StructType::create(llvm_data_members, structdata.ty.name, false);
		// write the struct type ptr into the struct data so it can easily be retrieved later.
		structdata.userdata = llvm_ty;
	}

	void codegen_structs(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, structdata] : state.struct_decls)
		{
			codegen_a_struct(structdata, state);
		}
	}

	void codegen_functions(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, funcdata] : state.functions)
		{
			diag::assert_that(funcdata.userdata == nullptr, std::format("internal compiler error: while running codegen for function \"{}\", userdata ptr was not-null, implying it has already been codegen'd", name));
			llvm::Type* llvm_return = as_llvm_type(funcdata.return_ty, state);
			std::vector<llvm::Type*> llvm_params;
			for(const semantic::local_variable_t& param : funcdata.params)
			{
				llvm_params.push_back(as_llvm_type(param.ty, state));
			}

			llvm::FunctionType* llvm_fty = llvm::FunctionType::get(llvm_return, llvm_params, false);
			llvm::Function* llvm_fn = llvm::Function::Create(llvm_fty, llvm::Function::ExternalLinkage, name, program.get());
			std::size_t arg_counter = 0;
			for(llvm::Argument& arg : llvm_fn->args())
			{
				// note: userdata for a variable declaration (that is a parameter) is an llvm::Argument* underlying.
				// unlike a global variable that is llvm::GlobalVariable* and a local variable that is llvm::AllocaInst*.
				funcdata.params[arg_counter].userdata = &arg;
				arg.setName(funcdata.params[arg_counter++].name);
			}

			funcdata.userdata = llvm_fn;
		}
	}

	void codegen_global_variables(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, gvardata] : state.global_variables)
		{
			llvm::Type* llvm_ty = as_llvm_type(gvardata.ty, state);
			// note: globals may have an initialiser, which means we will need to codegen that even though we're so early on.
			const ast::node& node = tree.get(gvardata.context);
			diag::assert_that(std::holds_alternative<ast::variable_declaration>(node.payload), std::format("internal compiler error: AST node corresponding to global variable \"{}\" (line {}) was not infact a variable declaration (variant id {}). something has gone horrendously wrong.", gvardata.name, node.meta.line_number, node.payload.index()));
			const auto& decl = std::get<ast::variable_declaration>(node.payload);

			llvm::Constant* llvm_initialiser = nullptr;
			if(decl.initialiser.has_value())
			{
				// todo: assign llvm_initialiser to the codegen'd expression.
			}

			// create our owning global variable.
			std::unique_ptr<llvm::GlobalVariable> llvm_gvar = std::make_unique<llvm::GlobalVariable>(*program, llvm_ty, false, llvm::GlobalValue::ExternalLinkage, llvm_initialiser);
			// keep ahold of the underlying ptr.
			gvardata.userdata = llvm_gvar.get();
			// move the owning ptr into our global storage.
			global_variable_storage.push_back(std::move(llvm_gvar));
		}
	}

	/////////////////////////////////////// NODE CODEGEN ///////////////////////////////////////

	struct data
	{
		const ast& tree;
		const ast::node& node;
		const ast::path_t& path;
		const semantic::state& state;

		std::size_t line() const
		{
			return this->tree.get(path).meta.line_number;
		}

		void fatal_error(std::string msg) const
		{
			diag::fatal_error(std::format("(codegen) line {} - {}", this->line(), msg));
		}

		void warning(std::string msg) const
		{
			diag::warning(std::format("(codegen) line {} - {}", this->line(), msg));
		}

		void assert_that(bool expr, std::string msg) const
		{
			if(!expr)
			{
				this->fatal_error(msg);	
			}
		}
	};

	template<typename P>
	llvm::Value* codegen_thing(const data& d, const P& payload);

	llvm::Value* integer_literal(const data& d, ast::integer_literal payload)
	{
		// decimal literals are always i64
		return llvm::ConstantInt::get(*ctx, llvm::APInt{64, static_cast<std::uint64_t>(payload.val), true});
	}

	llvm::Value* decimal_literal(const data& d, ast::decimal_literal payload)
	{
		// decimal literals are always f64 (double)
		return llvm::ConstantFP::get(builder->getDoubleTy(), llvm::APFloat{payload.val});
	}

	llvm::Value* char_literal(const data& d, ast::char_literal payload)
	{
		return llvm::ConstantInt::get(*ctx, llvm::APInt{8, static_cast<std::uint64_t>(payload.val)});
	}

	llvm::Value* string_literal(const data& d, ast::string_literal payload)
	{
		return llvm::ConstantDataArray::getString(*ctx, payload.val, true);
	}

	llvm::Value* bool_literal(const data& d, ast::bool_literal payload)
	{
		return llvm::ConstantInt::get(*ctx, llvm::APInt{1, payload.val ? 1u : 0u, true});
	}

	llvm::Value* expression(const data& d, ast::expression payload)
	{
		return codegen_thing(d, payload.expr);
	}

	llvm::Value* unary_expression(const data& d, unary_expression_t payload)
	{
		llvm::Value* operand = expression(d, *payload.second);
		d.assert_that(operand != nullptr, std::format("operand of unary expression could not be properly deduced. likely a syntax error."));
		const type* ty = d.state.try_get_type_from_node(d.path);
		d.assert_that(ty != nullptr, "internal compiler error: could not deduce type of expression");
		d.assert_that(ty->is_primitive(), std::format("operand to unary operator should be a primitive type. instead, it is a \"{}\"", ty->name()));
		switch(payload.first.type)
		{
			case lexer::token::type::minus:
				if(ty->is_integer_type())
				{
					return builder->CreateNeg(operand);
				}
				else if(ty->is_floating_point_type())
				{
					return builder->CreateFNeg(operand);
				}
				else
				{
					d.fatal_error(std::format("operand of unary operator \"-\" must be a floating or integer type, but instead it is a \"{}\"", ty->name()));
					return nullptr;
				}
			break;
			default:
				d.fatal_error("internal compiler error: codegen for this unary operator is not yet implemented.");
				return nullptr;
			break;
		}
	}

	llvm::Value* binary_expression(const data& d, binary_expression_t payload)
	{
		const auto&[op, lhs, rhs] = payload;

		const type* ty = d.state.try_get_type_from_node(d.path);
		d.assert_that(ty != nullptr, "internal compiler error: type of binary expression could not be deduced.");
		llvm::Value* lhs_value = expression(d, *lhs);
		llvm::Value* rhs_value = expression(d, *rhs);
		d.assert_that(lhs_value != nullptr, "lhs operand to binary operator could not be properly deduced. syntax error?");
		d.assert_that(rhs_value != nullptr, "rhs operand to binary operator could not be properly deduced. syntax error?");
		llvm::Value* ret = nullptr;

		bool want_lhs_ptr = op.type == lexer::token::type::equals;
		if(!want_lhs_ptr)
		{
			lhs_value = load_as(lhs_value, d.state, *ty);
		}
		rhs_value = load_as(rhs_value, d.state, *ty);
		switch(op.type)
		{
			case lexer::token::type::plus:
				ret = builder->CreateAdd(lhs_value, rhs_value);
			break;
			case lexer::token::type::minus:
				ret = builder->CreateSub(lhs_value, rhs_value);
			break;
			case lexer::token::type::double_equals:
				ret = builder->CreateICmpEQ(lhs_value, rhs_value);
			break;
			case lexer::token::type::not_equals:
				ret = builder->CreateICmpNE(lhs_value, rhs_value);
			break;
			case lexer::token::type::equals:
			{
				builder->CreateStore(rhs_value, lhs_value);
				// createstore returns a void type'd value, so we dont care about that for ret.
				// just use the type semantic analysis gave us.
				ret = rhs_value;
			}
			break;
			default:
				d.fatal_error("internal compiler error: binary operator is not yet implemented.");
			break;
		}
		if(ret == nullptr)
		{
			d.fatal_error("internal compiler error: a particular binary operator was not recognised in the context of its equivalent LLVM-IR.");
		}
		else
		{
			const type* ty = d.state.try_get_type_from_node(d.path);
			d.assert_that(ty != nullptr, "internal compiler error: type system failed to deduce type of binary operator expression");
			d.assert_that(as_llvm_type(*ty, d.state) == ret->getType(), std::format("internal compiler error: binary operator expression's deduced type \"{}\" does not match the real underlying LLVM value type. i.e type system has failed.", ty->name()));
		}
		return ret;
	}

	llvm::Value* identifier(const data& d, ast::identifier payload, type* output_identifier_type = nullptr)
	{
		// could be:
		// a global variable
		const semantic::local_variable_t* gvar = d.state.try_find_global_variable(payload.name);
		if(gvar != nullptr)
		{
			d.assert_that(gvar->userdata != nullptr, std::format("internal compiler error: userdata for global variable \"{}\" within semantic analysis state was nullptr (i.e it still hasn't been codegen'd yet)", gvar->name));
			if(output_identifier_type != nullptr)
			{
				*output_identifier_type = gvar->ty;
			}
			return static_cast<llvm::GlobalVariable*>(gvar->userdata);
		}
		// a local variable
		const semantic::local_variable_t* var = d.state.try_find_local_variable(d.path, payload.name);
		if(var != nullptr)
		{
			d.assert_that(var->userdata != nullptr, std::format("internal compiler error: userdata for local variable \"{}\" within semantic analysis state was nullptr (i.e it still hasn't been codegen'd yet)", var->name));
			if(output_identifier_type != nullptr)
			{
				*output_identifier_type = var->ty;
			}
			return static_cast<llvm::AllocaInst*>(var->userdata);
		}
		// a function parameter
		const semantic::function_t* parent_function = d.state.try_find_parent_function(d.tree, d.path);
		if(parent_function != nullptr)
		{
			// go through its parameters and see if we're one.
			for(const semantic::local_variable_t& param : parent_function->params)
			{
				if(param.name == payload.name)
				{
					// this is us!
					auto* arg = static_cast<llvm::AllocaInst*>(param.userdata);
					d.assert_that(arg != nullptr, std::format("internal compiler error: argument \"{}\" to defined function \"{}\" was not codegen'd properly, as its userdata is null. this should've been written to during the pre-pass over functions.", payload.name, parent_function->name));
					// its easy. the llvm::Argument* is the actual argument value itself. we just return it as if it were a value!
					// you can just CreateStore with it as a target. exactly the same as a local variable... i think...
					if(output_identifier_type != nullptr)
					{
						*output_identifier_type = param.ty;
					}
					return arg;
				}
			}
		}

		d.fatal_error(std::format("could not evaluate identifier \"{}\". tried global variable, local variable and function parameter :(", payload.name));
		return nullptr;
	}

	llvm::Value* function_call(const data& d, ast::function_call payload)
	{
		const semantic::function_t* func = d.state.try_find_function(payload.function_name);
		d.assert_that(func != nullptr, std::format("call to undefined function \"{}\"", payload.function_name));
		auto* llvm_func = static_cast<llvm::Function*>(func->userdata);
		d.assert_that(llvm_func != nullptr, std::format("internal compiler error: function \"{}\" had a nullptr userdata, implying it has not been codegen'd", payload.function_name));
		std::vector<llvm::Value*> llvm_params = {};
		for(std::size_t i = 0; i < payload.params.size(); i++)
		{
			type param_ty = func->params[i].ty;
			llvm::Value* param_val = expression(d, payload.params[i]);
			// note: no narrowing as of yet.
			d.assert_that(param_val != nullptr, std::format("internal compiler error: in call to function \"{}\", underlying value of expression passed to parameter {} (\"{}\") could not be deduced correctly", payload.function_name, i, func->params[i].name));
			llvm::Value* loaded_val = load_as(param_val, d.state, param_ty);
			d.assert_that(loaded_val != nullptr, std::format("type mismatch! expected type \"{}\" but load_as returned nullptr, implying the parameter value does not match the correct type.", param_ty.name()));
			llvm_params.push_back(loaded_val);
		}	
		return builder->CreateCall(llvm_func, llvm_params, func->return_ty.is_void() ? "" : "calltmp");
	}

	llvm::Value* member_access(const data& d, ast::member_access payload)
	{
		// it must be a global/local variable or function parameter.
		type ty = type::undefined();
		// unleash our cheeky hack - when we codegen the identifier, let us know which type it came up with.
		llvm::Value* lhs_var = identifier(d, payload.lhs, &ty);

		d.assert_that(ty.is_struct(), std::format("lhs identifier of member access \"{}\" is not a struct type, but instead a \"{}\"", payload.lhs.name, ty.name()));
		struct_type struct_ty = ty.as_struct();
		// which data member are we?
		// note: if we dont match to a data member here, then the code must be ill-formed.
		std::optional<std::size_t> data_member_idx = std::nullopt;
		for(std::size_t i = 0; i < struct_ty.data_members.size(); i++)
		{
			const struct_type::data_member& member = struct_ty.data_members[i];
			if(member.member_name == payload.rhs.name)
			{
				// its this one!
				data_member_idx = i;
			}
		}
		d.assert_that(data_member_idx.has_value(), std::format("access to non-existent data member named \"{}\" within struct \"{}\"", payload.rhs.name, struct_ty.name));
		// get the data member via GEP
		llvm::Type* llvm_struct_ty = as_llvm_type(ty, d.state);
		llvm::Value* ret = builder->CreateStructGEP(llvm_struct_ty, lhs_var, data_member_idx.value());
		return ret;
	}

	llvm::Value* if_statement(const data& d, ast::if_statement payload)
	{
		return nullptr;
	}

	llvm::Value* for_statement(const data& d, ast::for_statement payload)
	{
		return nullptr;
	}

	llvm::Value* return_statement(const data& d, ast::return_statement payload)
	{
		if(!payload.value.has_value())
		{
			return builder->CreateRetVoid();
		}
		const type* ret_ty = d.state.try_get_type_from_node(d.path);
		d.assert_that(ret_ty != nullptr, std::format("internal compiler error: type of non-void return expression could not be properly deduced. semantic analysis fucked up."));
		llvm::Value* retval = expression(d, payload.value.value());
		d.assert_that(retval != nullptr, std::format("internal compiler error: value of non-void return expression could not be properly deduced."));
		return builder->CreateRet(load_as(retval, d.state, *ret_ty));
	}

	llvm::Value* variable_declaration(const data& d, ast::variable_declaration payload)
	{
		if(d.path.size() <= 1)
		{
			// its a global variable.
			// globals are already done in the pre-pass.
			return static_cast<llvm::GlobalVariable*>(d.state.try_find_global_variable(payload.var_name)->userdata);
		}
		const semantic::local_variable_t* var = d.state.try_find_local_variable(d.path, payload.var_name);
		d.assert_that(var != nullptr, std::format("could not find local variable \"{}\"", payload.var_name));
		llvm::Type* llvm_ty = as_llvm_type(var->ty, d.state);
		llvm::AllocaInst* llvm_var = builder->CreateAlloca(llvm_ty, nullptr, payload.var_name);
		if(payload.initialiser.has_value())
		{
			if(var->ty.is_struct())
			{
				d.fatal_error("inline initialiser of a struct-typed variable is not yet implemented.");
			}	
			else
			{
				llvm::Value* init_value = expression(d, payload.initialiser.value());
				d.assert_that(init_value != nullptr, std::format("internal compiler error: variable declaration \"{}\"'s initialiser expression codegen'd to nullptr.", payload.var_name));
				builder->CreateStore(init_value, llvm_var);
			}
		}

		var->userdata = llvm_var;
		return llvm_var;
	}

	llvm::Value* function_definition(const data& d, ast::function_definition payload)
	{
		// functions are already done in the pre-pass.
		// however, none of their bodies are.
		// add the body if it exists.
		const semantic::function_t* func_ptr = d.state.try_find_function(payload.function_name);
		d.assert_that(func_ptr != nullptr, std::format("internal compiler error: semantic information for function \"{}\" did not exist.", payload.function_name));
		const semantic::function_t& funcdata = *func_ptr;

		auto* llvm_fn = static_cast<llvm::Function*>(funcdata.userdata);
		d.assert_that(llvm_fn != nullptr, std::format("internal compiler error: llvm::Function* mapping for pre-def'd function \"{}\" did not exist.", payload.function_name));

		const ast::node& node = d.tree.get(funcdata.context);
		diag::assert_that(std::holds_alternative<ast::function_definition>(node.payload), std::format("internal compiler error: AST node corresponding to function definition \"{}\" (line {}) was not infact a function definition (variant id {}). something has gone horrendously wrong.", funcdata.name, node.meta.line_number, node.payload.index()));
		const auto& decl = std::get<ast::function_definition>(node.payload);
		if(!decl.is_extern)
		{
			// create a new basic block.	
			llvm::BasicBlock* blk = llvm::BasicBlock::Create(*ctx, "entry", llvm_fn);
			builder->SetInsertPoint(blk);
			// before we go onto child nodes...
			// the parameters themselves are values. to write to them is not really a thing coz they aren't pointers.
			// so what we do here is create pointers to each parameter on the stack.
			for(const semantic::local_variable_t& param : funcdata.params)
			{
				// get the initial argument
				auto* arg = static_cast<llvm::Argument*>(param.userdata);
				// problem is, if this argument is a struct type, CreateStructGEP doesn't like it coz i have no way of getting a pointer to it.
				// i dont really know why. i guess function parameters in LLVM arent really considered normal local variables on the stack
				// well im gonna do that now and hope that the optimiser saves me (feel free to remove this crap and just pass the arg directly if you've figured this out since).
				llvm::AllocaInst* ptr = builder->CreateAlloca(arg->getType(), nullptr, std::format("paramcpy_{}", std::string{arg->getName()}));
				builder->CreateStore(arg, ptr);
				param.userdata = ptr;
			}

			bool has_return = false;
			for(std::size_t i = 0; i < node.children.size(); i++)
			{
				const ast::node& child = node.children[i];
				ast::path_t child_path = funcdata.context;
				child_path.push_back(i);
				if(std::holds_alternative<ast::return_statement>(child.payload))
				{
					has_return = true;
				}
				// todo: codegen these nodes.
				codegen_single_node(d.tree, d.state, child_path);
			}

			if(!has_return && funcdata.return_ty.is_void())
			{
				// automatically add a return :) you're welcome
				builder->CreateRetVoid();
			}
		}
		llvm::verifyFunction(*llvm_fn);
		return nullptr;
	}

	llvm::Value* struct_definition(const data& d, ast::struct_definition payload)
	{
		// functions are already done in the pre-pass.
		return nullptr;
	}

	llvm::Value* meta_region(const data& d, ast::meta_region payload)
	{
		// functions are already done in the pre-pass.
		return nullptr;
	}

	template<typename ... Ts> 
	struct overload : Ts ... { 
		using Ts::operator() ...;
	};
	template<class... Ts> overload(Ts...) -> overload<Ts...>;

	template<typename P>
	llvm::Value* codegen_thing(const data& d, const P& payload)
	{
		llvm::Value* ret = nullptr;
		auto dispatch = overload
		{
			[&](ast::integer_literal lit)
			{
				ret = integer_literal(d, lit);
			},
			[&](ast::decimal_literal lit)
			{
				ret = decimal_literal(d, lit);
			},
			[&](ast::char_literal lit)
			{
				ret = char_literal(d, lit);
			},
			[&](ast::string_literal lit)
			{
				ret = string_literal(d, lit);
			},
			[&](ast::bool_literal lit)
			{
				ret = bool_literal(d, lit);
			},
			[&](ast::binary_operator op)
			{
				d.fatal_error("dispatch error");
			},
			[&](ast::unary_operator op)
			{
				d.fatal_error("dispatch error");
			},
			[&](unary_expression_t uexpr)
			{
				ret = unary_expression(d, uexpr);
			},
			[&](binary_expression_t bexpr)
			{
				ret = binary_expression(d, bexpr);
			},
			[&](ast::identifier id)
			{
				ret = identifier(d, id);
			},
			[&](ast::function_call call)
			{
				ret = function_call(d, call);
			},
			[&](ast::member_access mem)
			{
				ret = member_access(d, mem);
			},
			[&](ast::expression expr)
			{
				ret = expression(d, expr);
			},
			[&](ast::if_statement ifst)
			{
				ret = if_statement(d, ifst);
			},
			[&](ast::for_statement forst)
			{
				ret = for_statement(d, forst);
			},
			[&](ast::return_statement returnst)
			{
				ret = return_statement(d, returnst);
			},
			[&](ast::variable_declaration decl)
			{
				ret = variable_declaration(d, decl);
			},
			[&](ast::function_definition fdef)
			{
				ret = function_definition(d, fdef);
			},
			[&](ast::struct_definition sdef)
			{
				ret = struct_definition(d, sdef);
			},
			[&](ast::meta_region meta)
			{
				ret = meta_region(d, meta);
			},
			[&](auto)
			{
				d.fatal_error("dispatch error (no codegen support for this node payload)");
			}

		};
		std::visit(dispatch, payload);
		return ret;
	}

	void codegen_single_node(const ast& tree, const semantic::state& state, ast::path_t path)
	{
		const ast::node& node = tree.get(path);
		codegen_thing({.tree = tree, .node = node, .path = path, .state = state}, node.payload);
	}
}