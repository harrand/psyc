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

	// global state:
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

	output generate(const ast& tree, const semal::output& input, std::string module_name)
	{
		diag::assert_that(program == nullptr && builder == nullptr, error_code::ice, "previous codegen state has not been erased and you want me to move onto codegening another file...");
		output ret
		{
			.codegen_handle = nullptr,
			.module_name = module_name
		};
		program = std::make_unique<llvm::Module>(module_name, *ctx);
		builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

		// todo: codegen logic goes here.

		llvm::verifyModule(*program);
		ret.codegen_handle = program.get();
		return ret;
	}
}