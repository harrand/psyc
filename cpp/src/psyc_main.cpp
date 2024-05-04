#include "config.hpp"
#include "diag.hpp"
#include <filesystem>
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

		auto getargnext = [&arg, &argnext]()
		{
			diag::assert_that(argnext != nullptr, error_code::badargs, "argument \"{}\" requires a proceeding argument value, but you didn't provide one.", arg);
			return *argnext;
		};
		if(i < (args.size() - 1))
		{
			argnext = &args[i + 1];
		}

		// parse args.
		if(arg == "-v")
		{
			print_version_info();
		}
		else if(arg == "-o")
		{
			ret.output_dir = getargnext();
			diag::assert_that(std::filesystem::exists(ret.output_dir), error_code::badargs, "output directory \"{}\" does not exist. please create it", ret.output_dir.string());
			diag::assert_that(std::filesystem::is_directory(ret.output_dir), error_code::badargs, "output directory \"{}\" is not a directory. please provide a directory.", ret.output_dir.string());
		}
		else if(arg == "-t")
		{
			ret.target_name = getargnext();
		}
		else if(arg == "--dump-ast")
		{
			ret.should_dump_ast = true;
		}
		else if(arg == "--dump-ir")
		{
			ret.should_dump_ir = true;
		}
		else if(arg.starts_with("-"))
		{
			diag::error(error_code::badargs, "unrecognised argument \"{}\"", arg);
		}
		else
		{
			std::filesystem::path input_file = arg;
			diag::assert_that(std::filesystem::exists(input_file), error_code::badargs, "input file \"{}\" could not be located", input_file.string());
		}
	}	
	return ret;
}

void print_version_info()
{
	diag::nyi("-v");
}
