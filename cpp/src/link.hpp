#ifndef PSYC_LINK_HPP
#define PSYC_LINK_HPP
#include <vector>
#include <filesystem>

namespace link
{
	void executable(std::vector<std::filesystem::path> object_files, std::filesystem::path output_path, std::string output_name);
	void library(std::vector<std::filesystem::path> object_files, std::filesystem::path output_path, std::string output_name);
}

#endif // PSYC_LINK_HPP