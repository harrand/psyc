#ifndef PSYC_BUILD_HPP
#define PSYC_BUILD_HPP
#include "config.hpp"
#include <string>
#include <array>

namespace build
{
	enum class linkage_type
	{
		none,
		executable,
		library,
		_count
	};

	constexpr std::array<const char*, (int)linkage_type::_count> linkage_type_names
	{
		"none",
		"executable",
		"library"
	};

	enum class config_type
	{
		debug,
		release,
		_count,
	};

	constexpr std::array<const char*, (int)config_type::_count> config_type_names
	{
		"debug",
		"release",
	};

	struct info
	{
		linkage_type link = linkage_type::none;
		config_type config = config_type::debug;
		std::string link_name = "a";
		std::vector<std::filesystem::path> extra_input_files = {};
		std::string target_triple;
		config::compiler_args compiler_args;

		std::filesystem::path get_output_path() const;
	};

	info gather(const config::compiler_args& args, std::size_t* lex_timers = nullptr, std::size_t* parse_timers = nullptr, std::size_t* semal_timers = nullptr, std::size_t* codegen_timers = nullptr);
}

#endif // PSYC_BUILD_HPP