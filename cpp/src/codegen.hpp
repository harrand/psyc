#ifndef PSYC_CODEGEN_HPP
#define PSYC_CODEGEN_HPP
#include "ast.hpp"
#include "semantic.hpp"
#include <filesystem>

namespace llvm
{
	struct Module;
}
namespace codegen
{
	void static_initialise();
	void static_terminate();

	void generate(const ast& tree, const semantic::state& state);
	std::unique_ptr<llvm::Module> pop();
	std::filesystem::path write_to_object_file(std::string object_filename);
}

#endif // PSYC_SEMANTIC_HPP