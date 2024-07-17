#ifndef PSYC_ERROR_HPP
#define PSYC_ERROR_HPP
#include <array>

enum class error_code
{
	ice,
	nyi,
	badargs,
	buildmeta,
	lex,
	parse,
	type,
	semal,
	codegen,
	coneval,
	link,
	_count
};

constexpr std::array<const char*, (int)error_code::_count> error_names
{
	"internal compiler",
	"not-yet-implemented",
	"badargs",
	"build metaprogram",
	"lex",
	"parse",
	"type",
	"semantic analysis",
	"code generation",
	"constant evaluation",
	"linker"
};

#endif // PSYC_ERROR_HPP