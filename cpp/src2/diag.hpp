#ifndef PSYC_DIAG_HPP
#define PSYC_DIAG_HPP
#include "error.hpp"
#include <format>
#include <iostream>

namespace diag
{
	namespace detail
	{
		inline void on_error()
		{
			#ifndef NDEBUG
				asm volatile("int3");
			#endif
			std::exit(-1);
		}
	}
	template<typename... Ts>
	void generic_msg(std::string_view preamble, std::format_string<Ts...> fmt, Ts&&... ts)
	{
		std::cout << std::format("psyc: {} \033[0m{}", preamble, std::format(fmt, std::forward<Ts>(ts)...)) << std::endl;
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
	void error_nonblocking(error_code err, std::format_string<Ts...> fmt, Ts&&... ts)
	{
		std::string error_preamble = std::format("\033[1;31m{} error", error_names[static_cast<int>(err)]);
		generic_msg(error_preamble, fmt, std::forward<Ts>(ts)...);
	}

	template<typename... Ts>
	void error(error_code err, std::format_string<Ts...> fmt, Ts&&... ts)
	{
		std::string error_preamble = std::format("\033[1;31m{} error", error_names[static_cast<int>(err)]);
		generic_msg(error_preamble, fmt, std::forward<Ts>(ts)...);
		detail::on_error();
	}

	template<typename... Ts>
	void assert_that(bool expr, error_code err, std::format_string<Ts...> fmt, Ts&&... ts)
	{
		if(!expr)
		{
			error(err, fmt, std::forward<Ts>(ts)...);
		}
	}

	template<typename... Ts>
	void ice(std::format_string<Ts...> fmt, Ts&&... ts)
	{
		error(error_code::ice, fmt, std::forward<Ts>(ts)...);
	}

	template<typename... Ts>
	void nyi(std::format_string<Ts...> fmt, Ts&&... ts)
	{
		error(error_code::nyi, fmt, std::forward<Ts>(ts)...);
	}
}

#endif // PSYC_DIAG_HPP