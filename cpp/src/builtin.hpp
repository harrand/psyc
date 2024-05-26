#ifndef PSYC_BUILTIN_HPP
#define PSYC_BUILTIN_HPP
#include "semal.hpp"

enum class builtin
{
	malloc,
	free,
	memcpy,
	debugbreak,
	sine,
	cosine,
	tangent,
	_count,
	_undefined,
};

// return builtin::_undefined if funcname doesnt correspond to a builtin.
builtin try_find_builtin(std::string_view funcname);
semal::function_t& get_builtin_function(builtin b);
#endif // PSYC_BUILTIN_HPP