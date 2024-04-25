#ifndef PSYC_CODEGEN_HPP
#define PSYC_CODEGEN_HPP
#include "ast.hpp"
#include "semantic.hpp"
#include "session.hpp"
#include <filesystem>

namespace llvm
{
	struct Module;
}
namespace codegen
{
	void static_initialise();
	void static_terminate();

	void generate(const ast& tree, const semantic::state& state, std::string llvm_module_name);
	void cleanup_program();
	std::string get_ir();
	std::unique_ptr<llvm::Module> pop();
	void write_to_object_file(const session& ses, std::filesystem::path object_filename);
}

#endif // PSYC_SEMANTIC_HPP