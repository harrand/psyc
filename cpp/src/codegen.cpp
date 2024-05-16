#include "codegen.hpp"
#include "diag.hpp"
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

	output generate(const semal::output& input, std::string module_name)
	{
		return 
		{
			.codegen_handle = nullptr,
			.module_name = module_name
		};
	}
}