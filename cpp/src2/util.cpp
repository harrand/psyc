#include "util.hpp"
#include "diag.hpp"
#include <string>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif // _WIN32

namespace util
{
	std::filesystem::path get_this_executable_path()
	{
		#ifdef _WIN32
			std::string path;
			path.resize(1024);
			DWORD path_len = GetModuleFileNameA(nullptr, path.data(), path.size());
			//diag::assert_that(path_len != 0, "fooey");
			path.resize(path_len);
			return path;
		#else
			return std::filesystem::canonical("/proc/self/exe");
		#endif
	}
}