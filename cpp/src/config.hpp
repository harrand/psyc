#ifndef PSYC_CONFIG_HPP
#define PSYC_CONFIG_HPP
#include <vector>
#include <filesystem>

namespace config
{
	struct compiler_args
	{
		std::vector<std::filesystem::path> input_files = {};
		std::filesystem::path output_dir = {};
		std::string target_name = "default";
		std::string linker_name = "";
		bool should_dump_ast = false;
		bool should_dump_ir = false;
	};
}

#endif // PSYC_CONFIG_HPP