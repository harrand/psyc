#ifndef PSYC_ERROR_HPP
#define PSYC_ERROR_HPP
#include <array>

enum class error_code
{
	ice,
	nyi,
	badargs,
	lex,
	parse,
	type,
	_count
};

constexpr std::array<const char*, (int)error_code::_count> error_names
{
	"internal compiler",
	"not-yet-implemented",
	"badargs",
	"lex",
	"parse",
	"type"
};

#endif // PSYC_ERROR_HPP