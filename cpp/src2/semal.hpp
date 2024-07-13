#ifndef PSYC_SEMAL_HPP
#define PSYC_SEMAL_HPP
#include "type.hpp"
#include "ast.hpp"

namespace semal
{
	void analyse(const syntax::inode* ast, type_system& tsys);
	void populate_table();
}

#endif // PSYC_SEMAL_HPP