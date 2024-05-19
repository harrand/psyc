#include "codegen.hpp"
#include "diag.hpp"
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
		return this->module_name + ".o";
	}

	void output::write_to_object_file(std::filesystem::path output_dir)
	{
		std::string object_filename = (output_dir / this->get_output_filename()).string();

		diag::error(error_code::nyi, "writing to object file is not yet implemented. i was told to write to \"{}\"", object_filename);
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

	output generate(const ast& tree, const semal::output& input, std::string module_name)
	{
		diag::assert_that(program == nullptr && builder == nullptr && debug == nullptr, error_code::ice, "previous codegen state has not been erased and you want me to move onto codegening another file...");
		output ret
		{
			.codegen_handle = nullptr,
			.module_name = module_name
		};
		program = std::make_unique<llvm::Module>(module_name, *ctx);
		builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
		if(true)
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
		funcdata.ctx.assert_that(funcdata.userdata == nullptr, error_code::ice, "internal compiler error: while running codegen for function \"{}\", userdata ptr was not-null, implying it has already been codegen'd", funcdata.name);
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
		const semal::output& semal;
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
			value init_value = codegen_thing(data{.ctx = globdata.ctx, .semal = semal_input}, decl.initialiser.value()->expr);
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

	// NODE STUFF

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

	value expression(const data& d, ast::expression payload)
	{
		return codegen_thing(d, payload.expr);
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
}