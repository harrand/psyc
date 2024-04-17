#include "link.hpp"
#include "diag.hpp"

namespace link
{
	void executable(std::vector<std::filesystem::path> object_files, std::filesystem::path output_path, std::string output_name)
	{
		diag::warning("executable linkage is not yet implemented.");
	}

	void library(std::vector<std::filesystem::path> object_files, std::filesystem::path output_path, std::string output_name)
	{
		diag::warning("library linkage is not yet implemented.");
	}
}