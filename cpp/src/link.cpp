#include "link.hpp"
#include "diag.hpp"
#include <array>

namespace link
{
	enum class linker_type
	{
		gnu_like,
		msvc,
	};

	linker_type divine_linker_type(std::string_view linker_name)
	{
		if(linker_name == "lld-link" // lld windows
		|| linker_name == "link") // msvc link.exe
		{
			return linker_type::msvc;
		}

		if(linker_name == "ld" // gnu linker
		|| linker_name == "lld" // lld generic
		|| linker_name == "ld.lld" // lld unix
		|| linker_name == "ld64.lld" // lld macos
		|| linker_name == "gold") // another gnu linker for elf
		{
			return linker_type::gnu_like;
		}

		#ifdef _WIN32
			return linker_type::msvc;
		#else
			return linker_type::gnu_like;
		#endif
	}

	linker_type divine_archiver_type(std::string_view archiver_name)
	{
		if(archiver_name == "ar"
		|| archiver_name == "llvm-ar")
		{
			return linker_type::gnu_like;
		}
		if(archiver_name == "lib")
		{
			return linker_type::msvc;
		}

		#ifdef _WIN32
			return linker_type::msvc;
		#else
			return linker_type::gnu_like;
		#endif
	}

	std::string guess_a_linker()
	{
		// guess a linker name to use depending on operating system. complete stab in the dark.
		#ifdef _WIN32
			return "link";
		#elif defined(__linux__)
			return "ld";
		#else
			diag::fatal_error("unable to automatically discern linker for your platform. please explicitly specify a linker via -l");
			return "";
		#endif
	}

	std::string guess_a_archiver()
	{
		#ifdef _WIN32
			return "lib";
		#elif defined(__linux__)
			return "ar";
		#else
			diag::fatal_error("unable to automatically discern archiver for your platform. please explicitly specify an archiver via -l");
			return "";
		#endif
	}

	void post_link(std::vector<std::filesystem::path> object_files)
	{
		for(auto file : object_files)
		{
			std::filesystem::remove(file);
		}
	}

	std::string exec_windows(std::string_view cmd)
	{
		#ifdef _WIN32
		std::array<char, 128> buffer;
		std::string result;
		std::unique_ptr<FILE, decltype(&_pclose)> pipe(_popen(cmd.data(), "r"), _pclose);
		if (!pipe) {
			return "";
		}
		while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe.get()) != nullptr) {
			result += buffer.data();
		}
		return result;

		#else

		return "";
		#endif
	}

	std::string replace_all(std::string str, const std::string& from, const std::string& to) {
		size_t start_pos = 0;
		while((start_pos = str.find(from, start_pos)) != std::string::npos) {
			str.replace(start_pos, from.length(), to);
			start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
		}
		return str;
	}

	std::string get_linker_prefix()
	{
		// if we're on win32, we will need to invoke vcvars64.bat first.
		// let's get that.
		#ifdef _WIN32

		std::string root_path = exec_windows("\"\"C:/Program Files (x86)/Microsoft Visual Studio/Installer/vswhere.exe\" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 | findstr \"installationPath\"");
		if(root_path.empty())
		{
			return "";
		}

		root_path.erase(0, sizeof("installationPath:"));
		std::string vcvarsall_path = replace_all(root_path + "\\VC\\Auxiliary\\Build\\vcvars64.bat", "\n", "");
		vcvarsall_path = "\"\"" + vcvarsall_path + "\" && ";
		return vcvarsall_path;
		#else
		return "";
		#endif
	}

	void executable(std::vector<std::filesystem::path> object_files, std::filesystem::path output_path, std::string output_name, std::string linker)
	{
		if(linker.empty())
		{
			linker = guess_a_linker();
		}
		std::string cmd = linker;
		for(std::filesystem::path obj : object_files)
		{
			cmd += std::format(" \"{}\"", obj.string());
		}
		#ifdef _WIN32
			cmd += " libcmt.lib";
		#endif

		auto lt = divine_linker_type(linker);
		std::string full_output = (output_path / output_name).string();
		switch(lt)
		{
			case linker_type::gnu_like:
				cmd += std::format(" -o \"{}\"", full_output);
			break;
			case linker_type::msvc:
				cmd += std::format( " /OUT:\"{}\"", full_output);
				cmd = get_linker_prefix() + cmd;
			break;
		}

		int ret = std::system(cmd.c_str());
		diag::assert_that(ret == 0, "detected that linker failed. unlucky, dickface!");
		post_link(object_files);
	}

	void library(std::vector<std::filesystem::path> object_files, std::filesystem::path output_path, std::string output_name, std::string linker)
	{
		if(linker.empty())
		{
			linker = guess_a_archiver();
		}
		auto lt = divine_linker_type(linker);
		std::string full_output = (output_path / output_name).string();

		std::string cmd = linker;

		switch(lt)
		{
			case linker_type::gnu_like:
				cmd += std::format(" rcs \"{}\"", full_output);
			break;
			case linker_type::msvc:
				cmd += std::format( " /OUT:\"{}\"", full_output);
				cmd = get_linker_prefix() + cmd;
			break;
		}

		for(std::filesystem::path obj : object_files)
		{
			cmd += std::format(" \"{}\"", obj.string());
		}

		#ifdef _WIN32
			cmd += " libcmt.lib";
		#endif

		int ret = std::system(cmd.c_str());
		diag::assert_that(ret == 0, "detected that archiver failed. unlucky, dickface!");
		post_link(object_files);
	}
}