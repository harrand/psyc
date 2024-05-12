#include "semal.hpp"
#include "diag.hpp"

namespace semal
{
	const srcloc& context::location() const
	{
		return tree->get(this->path).meta;
	}

	void output::combine(const output& rhs)
	{
		// add functions, global variables, structs etc... to *this
		for(const auto& [name, fn] : rhs.functions)
		{
			diag::assert_that(!this->functions.contains(name), error_code::semal, "at: {}: duplicate definition of function \"{}\" (previously defined at: {})", this->functions.at(name).ctx.location().to_string(), fn.name, fn.ctx.location().to_string());
			this->functions[name] = fn;
		}

		for(const auto& [name, gvar] : rhs.global_variables)
		{
			diag::assert_that(!this->global_variables.contains(name), error_code::semal, "at: {}: duplicate definition of global variable \"{}\" (previously defined at: {})", this->functions.at(name).ctx.location().to_string(), gvar.name, gvar.ctx.location().to_string());
			this->global_variables[name] = gvar;
		}

		for(const auto& [name, structdata] : rhs.struct_decls)
		{
			diag::assert_that(!this->struct_decls.contains(name), error_code::semal, "at: {}: duplicate definition of struct \"{}\" (previously defined at: {})", this->functions.at(name).ctx.location().to_string(), structdata.ty.name, structdata.ctx.location().to_string());
			this->struct_decls[name] = structdata;
		}
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