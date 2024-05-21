#include "codegen.hpp"
#include "diag.hpp"
#include "builtin.hpp"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h" 

namespace code
{
	std::string output::dump_ir() const
	{
		if(this->codegen_handle == nullptr)
		{
			return "<no output>";
		}
		std::string ir_string;
		llvm::raw_string_ostream os{ir_string};
		reinterpret_cast<llvm::Module*>(this->codegen_handle)->print(os, nullptr);
		return ir_string;
	}

	std::string output::get_output_filename() const
	{
		return std::filesystem::path{this->module_name + ".o"}.filename().string();
	}

	void output::write_to_object_file(const build::info& binfo) const
	{
		std::string object_filename = (binfo.compiler_args.output_dir / this->get_output_filename()).string();

		std::string error;
		const llvm::Target* target = llvm::TargetRegistry::lookupTarget(binfo.target_triple, error);
		if(target == nullptr)
		{
			diag::error(error_code::codegen, "error while retrieving LLVM output target information(s): {}", error);
		}
		const char* cpu = "generic";
		const char* features = "";
		llvm::TargetOptions opt;
		auto target_machine = target->createTargetMachine(binfo.target_triple, cpu, features, opt, llvm::Reloc::PIC_);
		// configure module (no i have no idea whats going on).
		auto* program = static_cast<llvm::Module*>(this->codegen_handle);
		program->setDataLayout(target_machine->createDataLayout());
		program->setTargetTriple(binfo.target_triple);
		std::error_code ec;
		llvm::raw_fd_ostream dst(object_filename, ec, llvm::sys::fs::OF_None);
		if(ec)
		{
			diag::error(error_code::codegen, "error while generating object files: {}", ec.message());
		}
		llvm::legacy::PassManager pass;
		auto file_type = llvm::CodeGenFileType::ObjectFile;
		if(target_machine->addPassesToEmitFile(pass, dst, nullptr, file_type, false))
		{
			diag::error(error_code::codegen, "target machine cannot emit a file of this type.");
		}
		pass.run(*program);
		dst.flush();
	}

	// global state:
	static std::unique_ptr<llvm::LLVMContext> ctx = nullptr;
	static std::unique_ptr<llvm::Module> program = nullptr;
	static std::unique_ptr<llvm::IRBuilder<>> builder = nullptr;
	static std::unique_ptr<llvm::DIBuilder> debug = nullptr;
	// global variables are RAII objects meaning we will have to deal with their lifetimes. to do that, we're just chucking them in there, and spreading the underlying ptr all around the place (as we're allocing we have pointer stability so its safe).
	static std::vector<std::unique_ptr<llvm::GlobalVariable>> global_variable_storage = {};

	struct object_debug_info
	{
		llvm::DICompileUnit* cu = nullptr;
		llvm::DIFile* file = nullptr;
		std::vector<llvm::DIScope*> scopes = {};
		void push_scope(llvm::DIScope* scope)
		{
			this->scopes.push_back(scope);
		}

		void pop_scope()
		{
			diag::assert_that(this->scopes.size(), error_code::ice, "pop_scope invoked but we're apparantly at the top-level");
		}
		void emit_null_location() const
		{
			builder->SetCurrentDebugLocation(llvm::DebugLoc());
		}
		void emit_location(const ast::node& node) const
		{
			llvm::DIScope* scope = this->cu;
			if(this->scopes.size())
			{
				scope = this->scopes.back();
			}
			builder->SetCurrentDebugLocation(llvm::DILocation::get(scope->getContext(), node.meta.line, node.meta.column, scope));
		}
	} debug_info;

	void static_initialise()
	{
		ctx = std::make_unique<llvm::LLVMContext>();
	}

	void static_terminate()
	{
		diag::assert_that(program == nullptr, error_code::ice, "an LLVM Module was not destroyed prior to static terminate");
		ctx = nullptr;
	}

	void* unsafe_release()
	{
		return program.release();
	}

	void cleanup()
	{
		// note: tearing down all this state is actually ridiculously error prone.
		// cant unlink globals until the program stops referencing them.
		// globals must all be destroyed before program itself dies.
		// so:
		// 1.) program stops referencing everything
		if(program != nullptr)
		{
			program->dropAllReferences();
		}
		// 2.) unlink globals from parent (but doesnt erase them)
		for(auto& glob_ptr : global_variable_storage)
		{
			glob_ptr->removeFromParent();
		}
		// 3.) kill globals and then program etc...

		global_variable_storage.clear();
		builder = nullptr;
		debug = nullptr;
		debug_info = {};
		program = nullptr;
	}

	llvm::Type* as_llvm_type(const type& ty, const semal::output& semal_input)
	{
		diag::assert_that(!ty.is_undefined(), error_code::ice, "undefined type made it to codegen!");
		if(ty.is_pointer())
		{
			return llvm::PointerType::get(*ctx, 0);
		}
		if(ty.is_struct())
		{
			const semal::struct_t* structdata = semal_input.try_find_struct(ty.name());
			diag::assert_that(structdata != nullptr, error_code::type, "struct type \"{}\" was not found within semantic analysis state, which is weird as it was recognised as a struct type. perhaps semantic analysis bug?", ty.name());
			diag::assert_that(structdata->userdata != nullptr, error_code::ice, "found struct named \"{}\" but its userdata ptr is null, meaning it has not been codegen'd and i currently need it while evaluating something else.", ty.name());
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
					diag::error(error_code::ice, "unrecognised primitive type \"{}\" used in source", ty.name());
					return nullptr;
				break;
			}
		}
		// todo: implement
		diag::error(error_code::ice, "could not convert type \"{}\" to its corresponding llvm::Type*", ty.name());
		return nullptr;
	}

	void codegen_struct(const semal::struct_t& structdata, const semal::output& semal_input);
	void codegen_function(const semal::function_t& funcdata, const semal::output& semal_input);
	void codegen_global_variable(const semal::local_variable_t& globdata, const semal::output& semal_input);
	void codegen_nodes(const ast& tree, const semal::output& input);

	output generate(const ast& tree, const semal::output& input, const build::info& binfo, std::string module_name)
	{
		diag::assert_that(program == nullptr && builder == nullptr && debug == nullptr, error_code::ice, "previous codegen state has not been erased and you want me to move onto codegening another file...");
		output ret
		{
			.codegen_handle = nullptr,
			.module_name = module_name
		};
		program = std::make_unique<llvm::Module>(module_name, *ctx);
		builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
		if(binfo.config == build::config_type::debug)
		{
			debug = std::make_unique<llvm::DIBuilder>(*program);
			debug_info.cu = debug->createCompileUnit(llvm::dwarf::DW_LANG_C, debug->createFile(module_name.c_str(), "."), "Psy Compiler", /*optimised*/ false, "", 0);
			debug_info.file = debug->createFile(debug_info.cu->getFilename(), debug_info.cu->getDirectory());
		}

		// todo: codegen logic goes here.
		for(const auto& [structname, structdata] : input.struct_decls)
		{
			codegen_struct(structdata, input);
		}
		for(const auto& [funcname, funcdata] : input.functions)
		{
			codegen_function(funcdata, input);
		}
		for(const auto& [globname, globdata] : input.global_variables)
		{
			codegen_global_variable(globdata, input);
		}

		codegen_nodes(tree, input);

		llvm::verifyModule(*program);
		if(debug != nullptr)
		{
			debug->finalize();
		}
		ret.codegen_handle = program.get();
		return ret;
	}

	// top level codegen bits. not thinking of nodes very much yet.

	void codegen_struct(const semal::struct_t& structdata, const semal::output& semal_input)
	{
		if(structdata.userdata != nullptr)
		{
			// already codegen'd. stop.
			return;
		}

		std::vector<llvm::Type*> llvm_data_members;
		for(const struct_type::data_member& member : structdata.ty.data_members)
		{
			if(member.ty->is_struct())
			{
				struct_type memty = member.ty->as_struct();
				codegen_struct(*semal_input.try_find_struct(memty.name.c_str()), semal_input);
			}
			llvm_data_members.push_back(as_llvm_type(*member.ty, semal_input));
		}
		llvm::StructType* llvm_ty = llvm::StructType::create(llvm_data_members, structdata.ty.name, false);
		structdata.userdata = llvm_ty;
	}

	void codegen_function(const semal::function_t& funcdata, const semal::output& semal_input)
	{
		funcdata.ctx.assert_that(funcdata.userdata == nullptr, error_code::ice, "while running codegen for function \"{}\", userdata ptr was not-null, implying it has already been codegen'd", funcdata.name);
		llvm::Type* llvm_return = as_llvm_type(funcdata.return_ty, semal_input);
		std::vector<llvm::Type*> llvm_params;
		for(const semal::local_variable_t& param : funcdata.params)
		{
			llvm_params.push_back(as_llvm_type(param.ty, semal_input));
		}

		llvm::FunctionType* llvm_fty = llvm::FunctionType::get(llvm_return, llvm_params, false);
		llvm::Function* llvm_fn = llvm::Function::Create(llvm_fty, llvm::Function::ExternalLinkage, funcdata.name, program.get());
		std::size_t arg_counter = 0;
		for(llvm::Argument& arg : llvm_fn->args())
		{
			// note: userdata for a variable declaration (that is a parameter) is an llvm::Argument* underlying.
			// unlike a global variable that is llvm::GlobalVariable* and a local variable that is llvm::AllocaInst*.
			funcdata.params[arg_counter].userdata = &arg;
			arg.setName(funcdata.params[arg_counter++].name);
		}

		funcdata.userdata = llvm_fn;
		// dont do blocks, just generate the functions themselves.
	}
	
	struct value
	{
		llvm::Value* llv = nullptr;
		type ty = type::undefined();
		bool is_variable = false;
		std::string variable_name = "";
	};

	struct data
	{
		const semal::context& ctx;
		const semal::output& state;
	};
	template<typename P>
	value codegen_thing(const data& d, const P& payload);

	void codegen_global_variable(const semal::local_variable_t& globdata, const semal::output& semal_input)
	{
		if(globdata.userdata != nullptr)
		{
			// already done previously.
			return;
		}
		llvm::Type* llvm_ty = as_llvm_type(globdata.ty, semal_input);
		// note: globals may have an initialiser, which means we will need to codegen that even though we're so early on.
		const ast::node& node = globdata.ctx.node();
		/*
			if(std::holds_alternative<ast::expression>(node.payload))
			{
				auto expr = std::get<ast::expression>(node.payload);
				if(std::holds_alternative<ast::variable_declaration>(expr.expr))
				*/
		globdata.ctx.assert_that(std::holds_alternative<ast::expression>(globdata.ctx.node().payload), error_code::ice, "node corresponding to global variable \"{}\" was not infact an expression (variant id {}). something has gone horrendously wrong.", globdata.name, node.payload.index());
		const auto& expr = std::get<ast::expression>(globdata.ctx.node().payload);
		globdata.ctx.assert_that(std::holds_alternative<ast::variable_declaration>(expr.expr), error_code::ice, "node corresponding to global variable \"{}\" was an expression, but the expression did not resolve to a variable declaration (variant id {}). something has gone horrendously wrong.", globdata.name, expr.expr.index());
		const auto& decl = std::get<ast::variable_declaration>(expr.expr);

		// create our owning global variable.
		std::unique_ptr<llvm::GlobalVariable> llvm_glob = std::make_unique<llvm::GlobalVariable>(*program, llvm_ty, false, llvm::GlobalValue::ExternalLinkage, nullptr);
		llvm_glob->setName(globdata.name);

		if(decl.initialiser.has_value())
		{
			// todo: assign llvm_initialiser to the codegen'd expression.
			value init_value = codegen_thing(data{.ctx = globdata.ctx, .state = semal_input}, decl.initialiser.value()->expr);
			globdata.ctx.assert_that(init_value.llv != nullptr, error_code::ice, "global variable \"{}\"'s initialiser expression codegen'd to a null value", globdata.name);
			llvm_glob->setInitializer(static_cast<llvm::Constant*>(init_value.llv));
		}
		else
		{
			globdata.ctx.error(error_code::codegen, "global variable \"{}\" does not have an initialiser. all globals *must* have an initialiser.", globdata.name);
		}

		// keep ahold of the underlying ptr.
		globdata.userdata = llvm_glob.get();
		// move the owning ptr into our global storage.
		global_variable_storage.push_back(std::move(llvm_glob));	
	}

	// UTILITY STUFF
	llvm::BasicBlock* get_defer_block_if_exists(llvm::Function* fn)
	{
		llvm::BasicBlock* back = &fn->back();
		if(back->getName() == "defer")
		{
			return back;
		}
		return nullptr;
	}

	llvm::BasicBlock* safe_get_defer_block(llvm::Function* fn)
	{
		// get the defer block if there is one.
		// if not, make one.
		llvm::BasicBlock* already = get_defer_block_if_exists(fn);
		if(already != nullptr)
		{
			return already;
		}
		return llvm::BasicBlock::Create(*ctx, "defer", fn);
	}

	value get_variable_val(const value& val, const data& d)
	{
		if(!val.ty.is_pointer())
		{
			d.ctx.error(error_code::ice,"variable \"{}\" was not a pointer (although the variable might not be a pointer, as far as the codegen implementation is concerned, it should be a pointer to whatever type you set it to)", val.variable_name);
		}
		return
		{
			.llv = builder->CreateLoad(as_llvm_type(val.ty.dereference(), d.state), val.llv),
			.ty = val.ty.dereference(),
			.is_variable = false,
			.variable_name = val.variable_name
		};
	}

	llvm::Value* load_as(llvm::Value* ptr, const data& d, type from, type to, bool force_explicit_conversion = false, bool check = true)
	{
		llvm::Type* llvm_to = as_llvm_type(to, d.state);
		llvm::Type* llvm_from = as_llvm_type(from, d.state);
		if(check && ptr->getType() != llvm_from)
		{
			diag::error(error_code::ice, "type system failure");
		}
		conversion_type conv = conversion_type::impossible;
		if(force_explicit_conversion)
		{
			conv = from.is_explicitly_convertible_to(to);
		}
		else
		{
			conv = from.is_implicitly_convertible_to(to);
		}
		switch(conv)
		{
			case conversion_type::impossible:
				d.ctx.error(error_code::type, "cannot implicitly convert \"{}\" to \"{}\". did you forget to use weak/cast?", from.name(), to.name());
			break;
			case conversion_type::none:
				d.ctx.assert_that(llvm_from == llvm_to, error_code::ice, "implicit conversion from \"{}\" to \"{}\" was considered a no-op, but LLVM IR doesn't agree.", from.name(), to.name());
				// no conversion necessary.
				return ptr;
			break;
			case conversion_type::i2i:
			{
				// some int type to another int type.
				std::size_t incorrect_bits = llvm_from->getIntegerBitWidth();
				std::size_t correct_bits = llvm_to->getIntegerBitWidth();
				if(incorrect_bits < correct_bits)
				{
					ptr = builder->CreateZExtOrBitCast(ptr, llvm_to);
				}
				else if(incorrect_bits > correct_bits)
				{
					ptr = builder->CreateTruncOrBitCast(ptr, llvm_to);
				}
				if(from.is_signed_integer_type() && to.is_unsigned_integer_type())
				{
					// signed -> unsigned
					ptr = builder->CreateTrunc(ptr, llvm_to);	
				}
				else if(from.is_unsigned_integer_type() && to.is_signed_integer_type())
				{
					// unsigned -> signed
					ptr = builder->CreateSExt(ptr, llvm_to);
				}
				return ptr;
			}
			case conversion_type::f2f:
				// some float type to another float type
				d.ctx.error(error_code::nyi, "converting between different floating point types (\"{}\" to \"{}\") is NYI", from.name(), to.name());
			break;
			case conversion_type::i2f:
				// some int type to another float type
				d.ctx.error(error_code::nyi, "converting from integer type to float type (\"{}\" to \"{}\") is NYI", from.name(), to.name());
			break;
			case conversion_type::p2p:
				// llvm pointers are opaque, so can trivially just return the same pointer here.
				return ptr;
			break;
			case conversion_type::i2p:
				return builder->CreateIntToPtr(ptr, llvm::PointerType::get(*ctx, 0));
			break;
		}
		return nullptr;
		/*
		// i8* and T* are implicitly convertible to-and-from.
		if(from == to || (from.is_pointer() && to.is_pointer() && from.pointer_level == to.pointer_level && (from.dereference() == type::from_primitive(primitive_type::i8) || to.dereference() == type::from_primitive(primitive_type::i8))))
		{
			// nothing to do.
			return ptr;
		}
		if(from.pointer_level > to.pointer_level)
		{
			// maybe need to load.
			if(from.dereference() == to)
			{
				// definitely need to load.
				return builder->CreateLoad(llvm_to, ptr);
			}
			else
			{
				return load_as(builder->CreateLoad(llvm_to, ptr), state, from.dereference(), to, false);
			}
		}
		// integer promotion.
		if(llvm_from->isIntegerTy() && llvm_to->isIntegerTy())
		{
			// need to do integer promotion.
			std::size_t incorrect_bits = llvm_from->getIntegerBitWidth();
			std::size_t correct_bits = llvm_to->getIntegerBitWidth();
			if(incorrect_bits < correct_bits)
			{
				ptr = builder->CreateZExtOrBitCast(ptr, llvm_to);
			}
			else if(incorrect_bits > correct_bits)
			{
				ptr = builder->CreateTruncOrBitCast(ptr, llvm_to);
			}
			if(from.is_signed_integer_type() && to.is_unsigned_integer_type())
			{
				// signed -> unsigned
				ptr = builder->CreateTrunc(ptr, llvm_to);	
			}
			else if(from.is_unsigned_integer_type() && to.is_signed_integer_type())
			{
				// unsigned -> signed
				ptr = builder->CreateSExt(ptr, llvm_to);
			}
			return ptr;
		}
		// pretty sure the code fucked this up as they are both non-pointers but of different types - let the caller handle this.
		diag::fatal_error("internal compiler error: codegen load_as returned nullptr");
		return nullptr;
		*/
	}

	// NODE STUFF

	value expression(const data& d, ast::expression payload)
	{
		return codegen_thing(d, payload.expr);
	}

	value integer_literal(const data& d, ast::integer_literal payload)
	{
		// decimal literals are always i64
		return
		{
			.llv = llvm::ConstantInt::get(*ctx, llvm::APInt{64, static_cast<std::uint64_t>(payload.val), true}),
			.ty = type::from_primitive(primitive_type::i64),
			.is_variable = false
		};
	}

	value decimal_literal(const data& d, ast::decimal_literal payload)
	{
		return
		{
			.llv = llvm::ConstantFP::get(builder->getDoubleTy(), llvm::APFloat{payload.val}),
			.ty = type::from_primitive(primitive_type::f64),
			.is_variable = false
		};
	}

	/*
	value char_literal(const data& d, ast::char_literal payload)
	{
		return
		{
			.llv = llvm::ConstantInt::get(*ctx, llvm::APInt{8, static_cast<std::uint64_t>(payload.val)}),
			.ty = type::from_primitive(primitive_type::i8),
			.is_variable = false
		};
	}

	value string_literal(const data& d, ast::string_literal payload)
	{
		return
		{
			.llv = builder->CreateGlobalStringPtr(payload.val, std::format("strlit_{}", payload.val), 0, program.get()),
			.ty = type::from_primitive(primitive_type::i8).pointer_to(),
			.is_variable = false
		};
	}
	*/

	value bool_literal(const data& d, ast::bool_literal payload)
	{
		return
		{
			.llv = llvm::ConstantInt::get(*ctx, llvm::APInt{1, payload.val ? 1u : 0u, true}),
			.ty = type::from_primitive(primitive_type::boolean),
			.is_variable = false
		};
	}

	value unary_operator(const data& d, const ast::unary_operator& payload)
	{
		const semal::function_t* enclosing_fn = d.state.try_find_parent_function(*d.ctx.tree, d.ctx.path);
		d.ctx.assert_that(enclosing_fn != nullptr, error_code::ice, "enclosing function of unary operator could not be found.");

		auto* llvm_enclosing_fn = static_cast<llvm::Function*>(enclosing_fn->userdata);
		llvm::BasicBlock* cur_block = builder->GetInsertBlock();
		bool defer = payload.op.t == lex::type::operator_defer;

		if(defer)
		{
			llvm::BasicBlock* defer_blk = safe_get_defer_block(llvm_enclosing_fn);
			builder->SetInsertPoint(defer_blk);	
		}
		value operandv = expression(d, *payload.expr);
		if(defer)
		{
			builder->SetInsertPoint(cur_block);
		}
		d.ctx.assert_that(operandv.llv != nullptr, error_code::codegen, std::format("operand of unary expression could not be properly deduced. likely a syntax error."));
		//d.assert_that(ty != nullptr, "internal compiler error: could not deduce type of expression");
		//d.assert_that(ty->is_primitive(), std::format("operand to unary operator should be a primitive type. instead, it is a \"{}\"", ty->name()));
		if(operandv.is_variable && payload.op.t != lex::type::operator_ref && payload.op.t != lex::type::operator_deref)
		{
			operandv = get_variable_val(operandv, d);
		}
		switch(payload.op.t)
		{
			case lex::type::operator_ref:
				d.ctx.assert_that(operandv.ty.is_pointer(), error_code::codegen, "`ref xyz`, xyz must be a pointer (as it must be a local, global or function param.)");
				operandv.is_variable = false;
				return operandv;
			break;
			case lex::type::operator_deref:
				d.ctx.assert_that(operandv.ty.is_pointer(), error_code::codegen, "`deref xyz`, xyz must be a pointer (coz its a deref...)");
				return
				{
					.llv = builder->CreateLoad(as_llvm_type(operandv.ty.dereference(), d.state), operandv.llv),
					.ty = operandv.ty.dereference(),
					.is_variable = operandv.is_variable,
					.variable_name = operandv.variable_name
				};
			break;
			case lex::type::operator_minus:
			{
				llvm::Value* llv = nullptr;
				if(operandv.ty.is_integer_type())
				{
					llv = builder->CreateNeg(operandv.llv);
				}
				else if(operandv.ty.is_floating_point_type())
				{
					llv = builder->CreateFNeg(operandv.llv);
				}
				else
				{
					d.ctx.error(error_code::codegen,"operand of unary operator \"-\" must be a floating or integer type, but instead it is a \"{}\"", operandv.ty.name());
					return {};
				}
				return
				{
					.llv = llv,
					.ty = operandv.ty,
					.is_variable = false
				};
			}
			break;
			case lex::type::operator_defer: return operandv; break;
			default:
				d.ctx.error(error_code::nyi, "codegen for unary operator \"{}\" is not yet implemented.", payload.op.lexeme);
				return {};
			break;
		}
	}

	value binary_operator(const data& d, const ast::binary_operator& payload)
	{
		// remember, type of binary expression is: type of the lhs expression
		value lhs_value = expression(d, *payload.lhs_expr);
		type ty = lhs_value.ty;

		type rhs_ty = type::undefined();
		if(payload.op.t == lex::type::operator_cast)
		{
			// cast has different syntax so handle it here and early return.
			d.ctx.assert_that(std::holds_alternative<ast::identifier>(payload.rhs_expr->expr), error_code::codegen,"in a cast, rhs of the cast token \"{}\" must be an identifier, not an expression or anything else.", payload.op.lexeme);
			std::string type_name = std::get<ast::identifier>(payload.rhs_expr->expr).iden;
			rhs_ty = d.state.get_type_from_name(type_name);
			if(lhs_value.is_variable)
			{
				lhs_value = get_variable_val(lhs_value, d);
				ty = lhs_value.ty;
			}
			return
			{
				.llv = load_as(lhs_value.llv, d, ty, rhs_ty, true),
				.ty = rhs_ty,
				.is_variable = lhs_value.is_variable,
				.variable_name = lhs_value.variable_name
			};
		}
		value rhs_value = expression(d, *payload.rhs_expr);
		rhs_ty = rhs_value.ty;
		d.ctx.assert_that(lhs_value.llv != nullptr, error_code::codegen, "lhs operand to binary operator could not be properly deduced. syntax error?");
		d.ctx.assert_that(rhs_value.llv != nullptr, error_code::codegen, "rhs operand to binary operator could not be properly deduced. syntax error?");
		value ret = {};

		bool want_lhs_ptr = payload.op.t == lex::type::operator_equals;

		if(!want_lhs_ptr && lhs_value.is_variable)
		{
			lhs_value = get_variable_val(lhs_value, d);
			ty = lhs_value.ty;
		}
		if(rhs_value.is_variable)
		{
			rhs_value = get_variable_val(rhs_value, d);
			rhs_ty = rhs_value.ty;
		}
		switch(payload.op.t)
		{
			case lex::type::operator_plus:
				if(lhs_value.ty.is_floating_point_type())
				{
					ret.llv = builder->CreateFAdd(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else if(lhs_value.ty.is_integer_type())
				{
					ret.llv = builder->CreateAdd(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else
				{
					d.ctx.error(error_code::codegen,"Addition can only be performed on an integer or floating-point type. You provided \"{}\" and \"{}\"", lhs_value.ty.name(), rhs_value.ty.name());
				}
				ret.ty = lhs_value.ty;
				ret.is_variable = false;
			break;
			case lex::type::operator_minus:
				if(lhs_value.ty.is_floating_point_type())
				{
					ret.llv = builder->CreateFSub(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else if(lhs_value.ty.is_integer_type())
				{
					ret.llv = builder->CreateSub(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else
				{
					d.ctx.error(error_code::codegen, "Subtraction can only be performed on an integer or floating-point type. You provided \"{}\" and \"{}\"", lhs_value.ty.name(), rhs_value.ty.name());
				}
				ret.ty = lhs_value.ty;
				ret.is_variable = false;
			break;
			case lex::type::operator_asterisk:
				if(lhs_value.ty.is_floating_point_type())
				{
					ret.llv = builder->CreateFMul(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else if(lhs_value.ty.is_integer_type())
				{
					ret.llv = builder->CreateMul(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else
				{
					d.ctx.error(error_code::codegen, "Multiplication can only be performed on an integer or floating-point type. You provided \"{}\" and \"{}\"", lhs_value.ty.name(), rhs_value.ty.name());
				}
				ret.ty = lhs_value.ty;
				ret.is_variable = false;
			break;
			case lex::type::operator_slash:
				if(lhs_value.ty.is_floating_point_type())
				{
					ret.llv = builder->CreateFDiv(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else if(lhs_value.ty.is_unsigned_integer_type())
				{
					ret.llv = builder->CreateUDiv(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else if(lhs_value.ty.is_signed_integer_type())
				{
					ret.llv = builder->CreateSDiv(lhs_value.llv, load_as(rhs_value.llv, d, rhs_value.ty, lhs_value.ty));
				}
				else
				{
					d.ctx.error(error_code::codegen, "Division can only be performed on an integer or floating-point type. You provided \"{}\" and \"{}\"", lhs_value.ty.name(), rhs_value.ty.name());
				}
				ret.ty = lhs_value.ty;
				ret.is_variable = false;
			break;
			case lex::type::operator_double_equals:
				if(lhs_value.ty.is_floating_point_type())
				{
					ret.llv = builder->CreateFCmpUEQ(lhs_value.llv, rhs_value.llv);
				}
				else if(lhs_value.ty.is_integer_type())
				{
					ret.llv = builder->CreateICmpEQ(lhs_value.llv, rhs_value.llv);
				}
				ret.ty = type::from_primitive(primitive_type::boolean);
				ret.is_variable = false;
			break;
			case lex::type::operator_notequals:
				if(lhs_value.ty.is_floating_point_type())
				{
					ret.llv = builder->CreateFCmpUNE(lhs_value.llv, rhs_value.llv);
				}
				else if(lhs_value.ty.is_integer_type())
				{
					ret.llv = builder->CreateICmpNE(lhs_value.llv, rhs_value.llv);
				}
				ret.ty = type::from_primitive(primitive_type::boolean);
				ret.is_variable = false;
			break;
			/*
			case lex::type::not_equals:
				ret.llv = builder->CreateICmpNE(lhs_value.llv, rhs_value.llv);
				ret.ty = type::from_primitive(primitive_type::boolean);
				ret.is_variable = false;
			break;
			*/
			case lex::type::operator_equals:
			{
				/*
				std::string value_string;
				llvm::raw_string_ostream os{value_string};
				rhs_value.llv->print(os);
				volatile std::string rhs_string = value_string;
				value_string = "";
				lhs_value.llv->print(os);
				volatile std::string lhs_string = value_string;
				*/
				
				builder->CreateStore(rhs_value.llv, lhs_value.llv);
				// createstore returns a void type'd value, so we dont care about that for ret.
				// just use the type semantic analysis gave us.
				ret = lhs_value;
			}
			break;
			default:
				d.ctx.error(error_code::nyi, "binary operator \"{}\" is not yet implemented.", payload.op.lexeme);
			break;
		}
		if(ret.llv == nullptr)
		{
			d.ctx.error(error_code::ice, "binary operator produced null value. that should never happen.");
		}
		return ret;
	}

	value identifier(const data& d, ast::identifier payload, type* output_identifier_type = nullptr)
	{
		// could be:
		// a global variable
		const semal::local_variable_t* gvar = d.state.try_find_global_variable(payload.iden);
		if(gvar != nullptr)
		{
			if(gvar->userdata == nullptr)
			{
				// hasn't been codegen'd yet. this means we're probably in the middle of another global variable's initialiser.
				// we have to do it now.
				codegen_global_variable(*gvar, d.state);
			}
			d.ctx.assert_that(gvar->userdata != nullptr, error_code::ice,"userdata for global variable \"{}\" within semantic analysis state was nullptr (i.e it still hasn't been codegen'd yet)", gvar->name);
			if(output_identifier_type != nullptr)
			{
				*output_identifier_type = gvar->ty;
			}
			return
			{
				.llv = static_cast<llvm::GlobalVariable*>(gvar->userdata),
				.ty = gvar->ty.pointer_to(),
				.is_variable = true,
				.variable_name = gvar->name
			};
		}
		// a local variable
		const semal::local_variable_t* var = d.state.try_find_local_variable(d.ctx.path, payload.iden);
		if(var != nullptr)
		{
			d.ctx.assert_that(var->userdata != nullptr, error_code::ice, "userdata for local variable \"{}\" within semantic analysis state was nullptr (i.e it still hasn't been codegen'd yet)", var->name);
			if(output_identifier_type != nullptr)
			{
				*output_identifier_type = var->ty;
			}
			return
			{
				.llv = static_cast<llvm::AllocaInst*>(var->userdata),
				.ty = var->ty.pointer_to(),
				.is_variable = true,
				.variable_name = var->name
			};
		}
		// a function parameter
		const semal::function_t* parent_function = d.state.try_find_parent_function(*d.ctx.tree, d.ctx.path);
		if(parent_function != nullptr)
		{
			// go through its parameters and see if we're one.
			for(const semal::local_variable_t& param : parent_function->params)
			{
				if(param.name == payload.iden)
				{
					// this is us!
					auto* arg = static_cast<llvm::AllocaInst*>(param.userdata);
					d.ctx.assert_that(arg != nullptr, error_code::ice, "argument \"{}\" to defined function \"{}\" was not codegen'd properly, as its userdata is null. this should've been written to during the pre-pass over functions.", payload.iden, parent_function->name);
					// its easy. the llvm::Argument* is the actual argument value itself. we just return it as if it were a value!
					// you can just CreateStore with it as a target. exactly the same as a local variable... i think...
					if(output_identifier_type != nullptr)
					{
						*output_identifier_type = param.ty;
					}
					return
					{
						.llv = arg,
						.ty = param.ty.pointer_to(),
						.is_variable = true,
						.variable_name = param.name
					};
				}
			}
		}

		d.ctx.error(error_code::ice, "could not evaluate identifier \"{}\". tried global variable, local variable and function parameter :(", payload.iden);
		return {};
	}

	value codegen_builtin(const data& d, ast::function_call payload, builtin b);

	value function_call(const data& d, ast::function_call payload)
	{
		const semal::function_t* func = d.state.try_find_function(payload.function_name);
		if(func->is_builtin)
		{
			return codegen_builtin(d, payload, try_find_builtin(payload.function_name));
		}
		d.ctx.assert_that(func != nullptr, error_code::codegen, "call to undefined function \"{}\"", payload.function_name);
		auto* llvm_func = static_cast<llvm::Function*>(func->userdata);
		d.ctx.assert_that(llvm_func != nullptr, error_code::ice, "function \"{}\" had a nullptr userdata, implying it has not been codegen'd", payload.function_name);
		std::vector<llvm::Value*> llvm_params = {};
		for(std::size_t i = 0; i < payload.params.size(); i++)
		{
			type expected_param_ty = func->params[i].ty;

			value param_val = expression(d, *payload.params[i]);
			if(param_val.is_variable)
			{
				param_val = get_variable_val(param_val, d);
			}
			// note: no narrowing as of yet.
			d.ctx.assert_that(param_val.llv != nullptr, error_code::ice, "in call to function \"{}\", underlying value of expression passed to parameter {} (\"{}\") could not be deduced correctly", payload.function_name, i, func->params[i].name);
			llvm::Value* loaded_val = load_as(param_val.llv, d, param_val.ty, expected_param_ty);
			d.ctx.assert_that(loaded_val != nullptr, error_code::type, "expected type \"{}\" but load_as returned nullptr, implying the parameter value does not match the correct type.", expected_param_ty.name());
			llvm_params.push_back(loaded_val);
		}	
		return 
		{
			.llv = builder->CreateCall(llvm_func, llvm_params, func->return_ty.is_void() ? "" : "calltmp"),
			.ty = func->return_ty,
			.is_variable = false
		};
	}

	value member_access(const data& d, ast::member_access payload)
	{
		value lhs_var = expression(d, *payload.lhs);
		d.ctx.assert_that(lhs_var.ty.is_pointer(), error_code::type, "lhs of member access should be a variable that resolves to a pointer type. instead it resolved to \"{}\"", lhs_var.ty.name());
		lhs_var.ty = lhs_var.ty.dereference();

		d.ctx.assert_that(lhs_var.ty.is_struct(), error_code::type, "lhs identifier of member access is not a struct type, but instead a \"{}\"", lhs_var.ty.name());
		struct_type struct_ty = lhs_var.ty.as_struct();
		// which data member are we?
		// note: if we dont match to a data member here, then the code must be ill-formed.
		std::optional<std::size_t> data_member_idx = std::nullopt;
		for(std::size_t i = 0; i < struct_ty.data_members.size(); i++)
		{
			const struct_type::data_member& member = struct_ty.data_members[i];
			if(member.member_name == payload.rhs)
			{
				// its this one!
				data_member_idx = i;
			}
		}
		d.ctx.assert_that(data_member_idx.has_value(), error_code::codegen, "access to non-existent data member named \"{}\" within struct \"{}\"", payload.rhs, struct_ty.name);
		// get the data member via GEP
		llvm::Type* llvm_struct_ty = as_llvm_type(lhs_var.ty, d.state);
		llvm::Value* ret = builder->CreateStructGEP(llvm_struct_ty, lhs_var.llv, data_member_idx.value());
		return
		{
			.llv = ret,
			.ty = struct_ty.data_members[data_member_idx.value()].ty->pointer_to(),
			.is_variable = true,
			.variable_name = struct_ty.data_members[data_member_idx.value()].member_name
		};
	}

	llvm::BasicBlock* block(const data& d, const char* name = "", bool force_no_parent = false)
	{
		const semal::function_t* parent_fn = d.state.try_find_parent_function(*d.ctx.tree, d.ctx.path);
		llvm::Function* llvm_parent = nullptr;
		if(!force_no_parent && parent_fn != nullptr)
		{
			llvm_parent = static_cast<llvm::Function*>(parent_fn->userdata);
		}

		const ast::node& node = d.ctx.node();
		d.ctx.assert_that(std::holds_alternative<ast::block>(node.payload), error_code::codegen, "atom(s) at location do not constitute an implementation block.");
		return llvm::BasicBlock::Create(*ctx, name, llvm_parent);
	}

	value if_statement(const data& d, ast::if_statement payload)
	{
		const ast::node& node = d.ctx.node();
		const ast::node& if_blk_node = node.children[0];
		const ast::node* else_blk_node = node.children.size() == 2 ? &node.children[1] : nullptr;
		d.ctx.assert_that(std::holds_alternative<ast::block>(if_blk_node.payload), error_code::codegen, "ur if block sucks.");
		if(else_blk_node != nullptr)
		{
			d.ctx.assert_that(std::holds_alternative<ast::block>(else_blk_node->payload), error_code::codegen, "ur else block sucks.");
		}

		value llvm_cond = expression(d, *payload.if_expr);
		d.ctx.assert_that(llvm_cond.llv != nullptr, error_code::ice, "could not codegen condition inside if-statement");
		// if its a bool with any qualifiers, or implicitly converible to a plain bool, then we're good to go.
		d.ctx.assert_that((llvm_cond.ty.is_primitive() && llvm_cond.ty.as_primitive() == primitive_type::boolean) || llvm_cond.ty.is_implicitly_convertible_to(type::from_primitive(primitive_type::boolean)) == conversion_type::none, error_code::type, "expression within if-condition does not resolve to a boolean.");

		const semal::function_t* parent_function = d.state.try_find_parent_function(*d.ctx.tree, d.ctx.path);
		d.ctx.assert_that(parent_function != nullptr && parent_function->userdata != nullptr, error_code::ice, "could not deduct parent enclosing function within if-statement");
		auto* llvm_parent_fn = static_cast<llvm::Function*>(parent_function->userdata);

		llvm::BasicBlock* if_blk = nullptr;
		auto if_blk_path = d.ctx.path;
		{
			if_blk_path.push_back(0);
			if_blk = block(
			data{
				.ctx =
				{
					.tree = d.ctx.tree,
					.path = if_blk_path
				},
				.state = d.state
			}, "if_true");
		}
		llvm::BasicBlock* else_blk = nullptr;
		auto else_blk_path = d.ctx.path;
		if(else_blk_node != nullptr)
		{
			else_blk_path.push_back(1);
			else_blk = block(
			data{
				.ctx =
				{
					.tree = d.ctx.tree,
					.path = else_blk_path
				},
				.state = d.state
			}, "if_else");
		}

		auto do_if_blk = [if_blk, if_blk_node, if_blk_path, &d]()->bool
		{
			bool contains_unconditional_return = false;
			for(std::size_t i = 0; i < if_blk_node.children.size(); i++)
			{
				auto child_path = if_blk_path;
				child_path.push_back(i);
				semal::context ctx
				{
					.tree = d.ctx.tree,
					.path = child_path
				};
				codegen_thing(data{
				.ctx = ctx,
				.state = d.state
				}, if_blk_node.children[i].payload);
				contains_unconditional_return |= (std::holds_alternative<ast::expression>(if_blk_node.children[i].payload) && std::holds_alternative<ast::return_statement>(std::get<ast::expression>(if_blk_node.children[i].payload).expr));
				// if we see a return before the last instruction, compile error.
				// i.e if i < last and unconditional return, boom
				ctx.assert_that(i >= (if_blk_node.children.size() - 1) || !contains_unconditional_return, error_code::codegen, "detected early-return within if block. all other code within the if-block is dead code.");
			}
			return contains_unconditional_return;
		};

		auto do_else_blk = [else_blk, else_blk_node, else_blk_path, &d]()
		{
			bool contains_unconditional_return = false;
			for(std::size_t i = 0; i < else_blk_node->children.size(); i++)
			{
				auto child_path = else_blk_path;
				child_path.push_back(i);
				semal::context ctx
				{
					.tree = d.ctx.tree,
					.path = child_path
				};
				codegen_thing(data{
				.ctx = ctx,
				.state = d.state
				}, else_blk_node->children[i].payload);
				contains_unconditional_return |= (std::holds_alternative<ast::expression>(else_blk_node->children[i].payload) && std::holds_alternative<ast::return_statement>(std::get<ast::expression>(else_blk_node->children[i].payload).expr));
				// if we see a return before the last instruction, compile error.
				// i.e if i < last and unconditional return, boom
				ctx.assert_that(i >= (else_blk_node->children.size() - 1) || !contains_unconditional_return, error_code::codegen, "detected early-return within else block. all other code within the else-block is dead code.");
			}
			return contains_unconditional_return;
		};

		llvm::BasicBlock* cont_blk = llvm::BasicBlock::Create(*ctx, "cont", llvm_parent_fn);
		if(else_blk == nullptr)
		{
			// if
			builder->CreateCondBr(llvm_cond.llv, if_blk, cont_blk);
			builder->SetInsertPoint(if_blk);
			if(!do_if_blk())
			{
				builder->CreateBr(cont_blk);
			}

			builder->SetInsertPoint(cont_blk);
		}
		else
		{
			// if-else
			builder->CreateCondBr(llvm_cond.llv, if_blk, else_blk);
			builder->SetInsertPoint(if_blk);
			if(!do_if_blk())
			{
				builder->CreateBr(cont_blk);
			}

			builder->SetInsertPoint(else_blk);
			if(!do_else_blk())
			{
				builder->CreateBr(cont_blk);
			}
			
			builder->SetInsertPoint(cont_blk);
		}

		return {};
	}

	value for_statement(const data& d, ast::for_statement payload)
	{
		const ast::node& node = d.ctx.node();
		const ast::node& blk = node.children.front();
		d.ctx.assert_that(std::holds_alternative<ast::block>(blk.payload), error_code::codegen, "ur for block sucks.");
		value init = expression(d, *payload.init_expr);
		value cond = expression(d, *payload.cond_expr);
		//d.ctx.warning("for-statements are not yet implemented. no corresponding code will be generated.");

		const semal::function_t* parent_function = d.state.try_find_parent_function(*d.ctx.tree, d.ctx.path);
		d.ctx.assert_that(parent_function != nullptr && parent_function->userdata != nullptr, error_code::ice, "could not deduct parent enclosing function within if-statement");
		auto* llvm_parent_fn = static_cast<llvm::Function*>(parent_function->userdata);

		llvm::BasicBlock* iter_blk = nullptr;
		auto iter_blk_path = d.ctx.path;
		{
			iter_blk_path.push_back(0);
			iter_blk = block(
				data
				{
					.ctx = 
					{
						.tree = d.ctx.tree,
						.path = iter_blk_path
					},
					.state = d.state
				}, "for_loop"
			);
		}

		auto do_blk = [iter_blk, blk, iter_blk_path, &d, &payload]()->bool
		{
			bool contains_unconditional_return = false;
			// do the block.
			for(std::size_t i = 0; i < blk.children.size(); i++)
			{
				auto child_path = iter_blk_path;
				child_path.push_back(i);
				semal::context ctx
				{
					.tree = d.ctx.tree,
					.path = child_path
				};
				codegen_thing(data{
				.ctx = ctx,
				.state = d.state
				}, blk.children[i].payload);
				contains_unconditional_return |= (std::holds_alternative<ast::expression>(blk.children[i].payload) && std::holds_alternative<ast::return_statement>(std::get<ast::expression>(blk.children[i].payload).expr));
				// if we see a return before the last instruction, compile error.
				// i.e if i < last and unconditional return, boom
				ctx.assert_that(i >= (blk.children.size() - 1) || !contains_unconditional_return, error_code::codegen, "detected early-return within for-loop block. all other code within the for-block is dead code.");
			}
			// do the iter of the for loop.
			expression(d, *payload.iter_expr);
			return contains_unconditional_return;
		};

		llvm::BasicBlock* cont_blk = llvm::BasicBlock::Create(*ctx, "cont", llvm_parent_fn);
		// if
		builder->CreateCondBr(cond.llv, iter_blk, cont_blk);
		builder->SetInsertPoint(iter_blk);
		if(!do_blk())
		{
			builder->CreateCondBr(expression(d, *payload.cond_expr).llv, iter_blk, cont_blk);
			//builder->CreateBr(cont_blk);
		}

		builder->SetInsertPoint(cont_blk);
		return {};
	}

	value return_statement(const data& d, ast::return_statement payload)
	{
		const semal::function_t* func = d.state.try_find_parent_function(*d.ctx.tree, d.ctx.path);
		if(!d.ctx.tree->try_get_next(d.ctx.path).has_value())
		{
			auto* llvm_func = static_cast<llvm::Function*>(func->userdata);	
			llvm::BasicBlock* blk = get_defer_block_if_exists(llvm_func);
			if(blk != nullptr)
			{
				// this function has a defer.
				// what we need to do is put a branch from here to there, and put our return at the end of the defer block instead.
				builder->CreateBr(blk);
				builder->SetInsertPoint(blk);
			}
		}
		if(!payload.expr.has_value())
		{
			return
			{
				.llv = builder->CreateRetVoid(),
				.ty = type::from_primitive(primitive_type::u0),
				.is_variable = false
			};
		}
		type expected_ret_ty = func->return_ty;

		value retval = expression(d, *payload.expr.value());
		d.ctx.assert_that(retval.llv != nullptr, error_code::ice, "value of non-void return expression could not be properly deduced.");
		if(retval.is_variable)
		{
			retval = get_variable_val(retval, d);
		}
		return
		{
			.llv = builder->CreateRet(load_as(retval.llv, d, retval.ty, expected_ret_ty)),
			.ty = expected_ret_ty,
			.is_variable = false
		};
	}

	value variable_declaration(const data& d, ast::variable_declaration payload)
	{
		if(d.ctx.path.size() <= 1)
		{
			// its a global variable.
			// globals are already done in the pre-pass.
			const semal::local_variable_t* glob = d.state.try_find_global_variable(payload.var_name);
			return
			{
				.llv = static_cast<llvm::GlobalVariable*>(glob->userdata),
				.ty = glob->ty,
				.is_variable = true,
				.variable_name = glob->name
			};
		}
		const semal::local_variable_t* var = d.state.try_find_local_variable(d.ctx.path, payload.var_name);
		d.ctx.assert_that(var != nullptr, error_code::codegen,"could not find local variable \"{}\"", payload.var_name);
		llvm::Type* llvm_ty = as_llvm_type(var->ty, d.state);
		llvm::AllocaInst* llvm_var = builder->CreateAlloca(llvm_ty, nullptr, payload.var_name);
		if(payload.initialiser.has_value())
		{
			if(var->ty.is_struct())
			{
				d.ctx.error(error_code::nyi, "inline initialiser of a struct-typed variable is not yet implemented.");
			}	
			else
			{
				value init_value = expression(d, *payload.initialiser.value());
				if(init_value.is_variable)
				{
					init_value = get_variable_val(init_value, d);
				}
				d.ctx.assert_that(init_value.llv != nullptr, error_code::ice, "variable declaration \"{}\"'s initialiser expression codegen'd to nullptr.", payload.var_name);
				builder->CreateStore(init_value.llv, llvm_var);
			}
		}

		var->userdata = llvm_var;
		return
		{
			.llv = llvm_var,
			.ty = var->ty,
			.is_variable = true,
			.variable_name = var->name
		};
	}

	value function_definition(const data& d, ast::function_definition payload)
	{
		// functions are already done in the pre-pass.
		// however, none of their bodies are.
		// add the body if it exists.
		const semal::function_t* func_ptr = d.state.try_find_function(payload.func_name);
		d.ctx.assert_that(func_ptr != nullptr, error_code::ice, "internal compiler error: semantic information for function \"{}\" did not exist.", payload.func_name);
		const semal::function_t& funcdata = *func_ptr;

		auto* llvm_fn = static_cast<llvm::Function*>(funcdata.userdata);
		d.ctx.assert_that(llvm_fn != nullptr, error_code::ice, "llvm::Function* mapping for pre-def'd function \"{}\" did not exist.", payload.func_name);

		const ast::node& func_node = funcdata.ctx.node();
		funcdata.ctx.assert_that(std::holds_alternative<ast::function_definition>(func_node.payload), error_code::ice, "AST node corresponding to function definition \"{}\" (line {}) was not infact a function definition (variant id {}). something has gone horrendously wrong.", funcdata.name, func_node.payload.index());
		const auto& decl = std::get<ast::function_definition>(func_node.payload);
		if(!decl.is_extern)
		{
			d.ctx.assert_that(func_node.children.size() == 1, error_code::codegen, "non-extern function \"{}\" must have exactly one child node, but it actually has {}", funcdata.name, func_node.children.size());
			const auto& node = func_node.children.front();
			d.ctx.assert_that(std::holds_alternative<ast::block>(node.payload), error_code::codegen, "non-extern function \"{}\"'s child node was not a block (variant id {})", funcdata.name, node.payload.index());
			auto blk_path = d.ctx.path;
			blk_path.push_back(0);
			llvm::BasicBlock* blk = block({.ctx = {.tree = d.ctx.tree, .path = blk_path}, .state = d.state}, "entry");
			builder->SetInsertPoint(blk);
			// before we go onto child nodes...
			// the parameters themselves are values. to write to them is not really a thing coz they aren't pointers.
			// so what we do here is create pointers to each parameter on the stack.
			for(const semal::local_variable_t& param : funcdata.params)
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
				ast::path_t child_path = blk_path;
				child_path.push_back(i);
				if(std::holds_alternative<ast::expression>(child.payload) && std::holds_alternative<ast::return_statement>(std::get<ast::expression>(child.payload).expr))
				{
					has_return = true;
				}
				semal::context new_ctx = {.tree = d.ctx.tree, .path = child_path};
				codegen_thing({.ctx = new_ctx, .state = d.state}, new_ctx.node().payload);
			}

			if(!has_return)
			{
				if(funcdata.return_ty.is_void())
				{
					// automatically add a return :) you're welcome
					builder->CreateRetVoid();
				}
				else
				{
					d.ctx.error(error_code::codegen, "missing return value for function \"{}\"", payload.func_name);
				}
			}
			if(!has_return && funcdata.return_ty.is_void())
			{
			}
		}
		llvm::verifyFunction(*llvm_fn);
		return {};
	}

	value struct_definition(const data& d, ast::struct_definition payload)
	{
		// functions are already done in the pre-pass.
		const semal::struct_t* structdef = d.state.try_find_struct(payload.name);
		for(const auto&[name, method] : structdef->methods)
		{
			const auto& node = method.ctx.node();
			d.ctx.assert_that(std::holds_alternative<ast::function_definition>(node.payload), error_code::ice, "pooey");
			function_definition(data
			{
				.ctx = method.ctx,
				.state = d.state
			}, std::get<ast::function_definition>(node.payload));
		}
		return {};
	}

	value meta_region(const data& d, ast::meta_region payload)
	{
		// functions are already done in the pre-pass.
		return {};
	}

	template<typename P>
	value codegen_thing(const data& d, const P& payload)
	{
		value ret = {};
		auto dispatch = util::overload
		{
			[&](ast::integer_literal lit)
			{
				ret = integer_literal(d, lit);
			},
			[&](ast::decimal_literal lit)
			{
				ret = decimal_literal(d, lit);
			},
			/*
			[&](ast::char_literal lit)
			{
				ret = char_literal(d, lit);
			},
			[&](ast::string_literal lit)
			{
				ret = string_literal(d, lit);
			},
			*/
			[&](ast::bool_literal lit)
			{
				ret = bool_literal(d, lit);
			},
			[&](ast::binary_operator op)
			{
				ret = binary_operator(d, op);
			},
			[&](ast::unary_operator op)
			{
				ret = unary_operator(d, op);
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
				d.ctx.error(error_code::nyi, "dispatch error (no codegen support for this node payload)");
			}

		};
		std::visit(dispatch, payload);
		return ret;
	}

	void codegen_nodes(const ast& tree, const semal::output& input)
	{
		for(std::size_t i = 0; i < tree.root.children.size(); i++)
		{
			codegen_thing({.ctx = {.tree = &tree, .path = ast::path_t{i}}, .state = input}, tree.root.children[i].payload);
		}
	}

	value codegen_builtin(const data& d, ast::function_call call, builtin b)
	{
		value ret
		{
			.llv = nullptr,
			.ty = type::undefined(),
			.is_variable = false
		};
		const semal::function_t& func = get_builtin_function(b);
		switch(b)
		{
			case builtin::_undefined:
				d.ctx.error(error_code::ice, "undefined builtin \"{}\"", call.function_name);
			break;
			case builtin::malloc:
			{
				value size = codegen_thing(d, call.params.front()->expr);
				if(size.is_variable)
				{
					size = get_variable_val(size, d);
				}
				llvm::Type* intptr_type = as_llvm_type(func.params.front().ty, d.state);
				ret.ty = func.return_ty;
				ret.llv = builder->CreateMalloc(intptr_type, as_llvm_type(ret.ty, d.state), size.llv, llvm::ConstantInt::get(intptr_type, 1), nullptr);	
			}
			break;
			case builtin::free:
			{
				value ptr = codegen_thing(d, call.params.front()->expr);
				if(ptr.is_variable)
				{
					ptr = get_variable_val(ptr, d);
				}
				ret.llv = builder->CreateFree(ptr.llv);
				ret.ty = func.return_ty;;
			}
			break;
			case builtin::debugbreak:
				ret.llv = builder->CreateCall(llvm::Intrinsic::getDeclaration(program.get(), llvm::Intrinsic::trap));
				ret.ty = func.return_ty;
			break;
			default:
				d.ctx.error(error_code::nyi, "missing codegen for builtin \"{}\"", call.function_name);
			break;
		}
		return ret;
	}
}