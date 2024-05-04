#include "config.hpp"
#include "diag.hpp"
#include <span>
#include <string_view>

config::compiler_args parse_args(std::span<const std::string_view> cli_args);
void print_version_info();

int main(int argc, char** argv)
{
	const std::vector<std::string_view> cli_args(argv + 1, argv + argc);
	config::compiler_args args = parse_args(cli_args);
	return 0;
}

config::compiler_args parse_args(std::span<const std::string_view> args)
{
	config::compiler_args ret;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const std::string_view& arg = args[i];
		const std::string_view* argnext = nullptr;
		if(i < (args.size() - 1))
		{
			argnext = &args[i + 1];
		}

		// parse args.
		if(arg == "-v")
		{
			print_version_info();
		}
	}	
	return ret;
}

void print_version_info()
{
	diag::nyi("-v");
}
