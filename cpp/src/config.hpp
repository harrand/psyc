#ifndef PSYC_CONFIG_HPP
#define PSYC_CONFIG_HPP
#include <vector>
#include <filesystem>

namespace config
{
	struct compiler_args
	{
		std::filesystem::path output_dir = {};
		std::vector<std::filesystem::path> input_files = {};	
		bool should_dump_ast;
	};
}

#endif // PSYC_CONFIG_HPP