#include "codegen.hpp"
#include "diag.hpp"
#include "llvm/IR/LLVMContext.h"

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
		global_variable_storage.clear();
		builder = nullptr;
		program = nullptr;
	}

	void codegen_structs(const ast& tree, const semantic::state& state);
	void codegen_functions(const ast& tree, const semantic::state& state);
	void codegen_global_variables(const ast& tree, const semantic::state& state);

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
	}

	// given a previous `generate`, retrieve an owning pointer to the result.
	std::unique_ptr<llvm::Module> pop()
	{
		std::unique_ptr<llvm::Module> ret = std::move(program);
		cleanup_program();
		return ret;
	}

	// given a previous `generate`, write the resultant program to an object filename.
	std::filesystem::path write_to_object_file(std::string object_filename)
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
		llvm::raw_fd_ostream dst(object_filename, ec, llvm::sys::fs::OF_None);
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
		return std::filesystem::path{object_filename};
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
		// todo: implement
		diag::fatal_error(std::format("internal compiler error: could not convert type \"{}\" to its corresponding llvm::Type*", ty.name()));
		return nullptr;
	}

	llvm::Type* as_llvm_type(const util::box<type>& ty, const semantic::state& state)
	{
		return as_llvm_type(*ty, state);
	}

	/////////////////////////////////////// TOP-LEVEL CODEGEN ///////////////////////////////////////

	void codegen_structs(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, structdata] : state.struct_decls)
		{
			diag::assert_that(structdata.userdata == nullptr, std::format("internal compiler error: while running codegen for struct \"{}\", userdata ptr was not-null, implying it has already been codegen'd", name));
			std::vector<llvm::Type*> llvm_data_members;
			for(const struct_type::data_member& member : structdata.ty.data_members)
			{
				llvm_data_members.push_back(as_llvm_type(member.type, state));
			}

			llvm::StructType* llvm_ty = llvm::StructType::create(llvm_data_members, name, false);
			// write the struct type ptr into the struct data so it can easily be retrieved later.
			structdata.userdata = llvm_ty;
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
			llvm::Function* llvm_fn = llvm::Function::Create(llvm_fty, llvm::Function::ExternalLinkage, name);
			std::size_t arg_counter = 0;
			for(llvm::Argument& arg : llvm_fn->args())
			{
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
}