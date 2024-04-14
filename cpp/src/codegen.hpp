#ifndef PSYC_CODEGEN_HPP
#define PSYC_CODEGEN_HPP
#include "ast.hpp"

namespace codegen
{
	void generate(const ast& ast, std::string filename);
}

#endif // PSYC_SEMANTIC_HPP