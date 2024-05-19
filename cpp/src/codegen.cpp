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
		llvm::DICompileUnit* cu;
		llvm::DIFile* file;
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
		program = nullptr;
	}

	struct data
	{
		const ast& tree;
		const ast::node& node;
		const ast::path_t& path;
		const semal::output& semantic;

		void error(std::string msg) const
		{
			diag::error(error_code::codegen, "at: {}: {}", node.meta.to_string(), msg);
		}

		void internal_error(std::string msg) const
		{
			diag::error(error_code::ice, "at: {}: {}", node.meta.to_string(), msg);
		}

		void warning(std::string msg) const
		{
			diag::warning("at: {}: {}", node.meta.to_string(), msg);
		}

		void assert_that(bool expr, std::string msg) const
		{
			if(!expr)
			{
				this->error(msg);
			}
		}
	};

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
}