#ifndef PSYC_CODEGEN_HPP
#define PSYC_CODEGEN_HPP
#include "ast.hpp"
#include <filesystem>

namespace llvm
{
	struct Module;
}
namespace codegen
{
	std::unique_ptr<llvm::Module> static_generate(const ast& ast, std::string filename);
	void static_terminate();
	std::filesystem::path generate(const ast& ast, std::string filename);
}

#endif // PSYC_SEMANTIC_HPP