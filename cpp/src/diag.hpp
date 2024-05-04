#ifndef PSYC_DIAG_HPP
#define PSYC_DIAG_HPP
#include "error.hpp"
#include <format>

namespace diag
{
	template<typename... Ts>
	void generic_msg(std::string_view preamble, std::format_string<Ts...> fmt, Ts&&... ts)
	{
		std::format("{}: {}\033[0m", preamble, std::format(fmt, std::forward<Ts>(ts)...));
	}

	template<typename... Ts>
	void note(std::format_string<Ts...> fmt, Ts&&... ts)
	{
		generic_msg("note", fmt, std::forward<Ts>(ts)...);
	}

	template<typename... Ts>
	void warning(std::format_string<Ts...> fmt, Ts&&... ts)
	{
		generic_msg("\033[1;33mwarning", fmt, std::forward<Ts>(ts)...);
	}

	template<typename... Ts>
	void error(error_code err, std::format_string<Ts...> fmt, Ts&&... ts)
	{
		std::string error_preamble = std::format("{} error", error_names[static_cast<int>(err)]);
		generic_msg(error_preamble, fmt, std::forward<Ts>(ts)...);
	}

	template<typename... Ts>
	void ice(std::format_string<Ts...> fmt, Ts&&... ts)
	{
		error(error_code::internal_compiler_error, fmt, std::forward<Ts>(ts)...);
	}
}

#endif // PSYC_DIAG_HPP