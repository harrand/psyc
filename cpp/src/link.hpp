#ifndef PSYC_LINK_HPP
#define PSYC_LINK_HPP
#include "build.hpp"
#include <filesystem>
#include <unordered_map>

namespace linkage
{
	struct state
	{
		std::unordered_map<std::filesystem::path, std::filesystem::path> input_output_files = {};

		void build(build::info binfo);
	};

}

#endif // PSYC_LINK_HPP