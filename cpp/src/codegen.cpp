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

	void static_initialise()
	{
		ctx = std::make_unique<llvm::LLVMContext>();
	}

	void static_terminate()
	{
		diag::assert_that(program == nullptr, "internal compiler error: a llvm Module was not destroyed prior to static terminate");
		ctx = nullptr;
	}

	// generate the program according to the ast and state.
	// resultant module will be left in `program`.
	void generate(const ast& tree, const semantic::state& state)
	{

	}

	// given a previous `generate`, retrieve an owning pointer to the result.
	std::unique_ptr<llvm::Module> pop()
	{
		return std::move(program);
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
		program = nullptr;
	}
}