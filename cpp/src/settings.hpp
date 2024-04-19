#ifndef PSYC_SETTINGS_HPP
#define PSYC_SETTINGS_HPP
#include <cstdint>

using flag_t = std::uint8_t;
enum flag : flag_t
{
	dump_ast = 0b0000001,
	dump_tokens = 0b000010,
	no_stdlib = 0b000100,
};

enum class link_output
{
	none,
	library,
	executable
};

#endif // PSYC_SETTINGS_HPP