#ifndef PSYC_ERROR_HPP
#define PSYC_ERROR_HPP
#include <array>

enum class error_code
{
	internal_compiler_error,
	badargs,
	syntax,
	type,
	_count
};

constexpr std::array<const char*, (int)error_code::_count> error_names
{
	"internal",
	"badargs",
	"syntax",
	"type"
};

#endif // PSYC_ERROR_HPP