#include "semal.hpp"

namespace semal
{
	void output::combine(const output& rhs)
	{
		// add functions, global variables, structs etc... to *this
	}

	output analyse_predecl(const ast& tree)
	{
		output ret;
		// analyse function declarations, structs and global variables.
		// do *not* analyse function/method implementations.
		return ret;
	}

	output analyse_full(const ast& tree, output predecl)
	{
		output ret = predecl;
		// todo: analyse all code (mainly implementations).
		
		return ret;
	}
}