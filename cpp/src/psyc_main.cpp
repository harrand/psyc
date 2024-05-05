#include "config.hpp"
#include "lex.hpp"
#include "parse.hpp"
#include "timer.hpp"
#include "diag.hpp"
#include <filesystem>
#include <span>
#include <string_view>
#include <iomanip>

config::compiler_args parse_args(std::span<const std::string_view> cli_args);
void print_version_info();

struct timers
{
	std::uint64_t lexing = 0u;
	std::uint64_t parsing = 0u;
	std::uint64_t buildmeta = 0u;
	std::uint64_t semal = 0u;
	std::uint64_t codegen = 0u;
	std::uint64_t link = 0u;
	void print()
	{
		constexpr int width = 12;
		std::cout << std::left << std::setw(width) << "lexer:" << std::setw(6) << std::setprecision(3) << std::right << (this->lexing / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "parser:" << std::setw(6) << std::setprecision(3) << std::right << (this->parsing / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "buildmeta:" << std::setw(6) << std::setprecision(3) << std::right << (this->buildmeta / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "semal:" << std::setw(6) << std::setprecision(3) << std::right << (this->semal / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "codegen:" << std::setw(6) << std::setprecision(3) << std::right << (this->codegen / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "link:" << std::setw(6) << std::setprecision(3) << std::right << (this->link / 1000.0f) << " seconds" << std::endl;
		std::uint64_t total =
			this->lexing +
			this->parsing +
			this->buildmeta +
			this->semal +
			this->codegen +
			this->link;
		std::cout << std::right << std::setw(width + 6 + 9) << std::setfill('=') << "\n" << std::setfill(' ');
		std::cout << std::left <<  std::setw(width) << "total:" << std::setw(6) << std::setprecision(3) << std::right << (total / 1000.0f) << " seconds" << std::endl;
	}
};

int main(int argc, char** argv)
{
	const std::vector<std::string_view> cli_args(argv + 1, argv + argc);
	config::compiler_args args = parse_args(cli_args);
	timers t;

	// lex
	timer::start();
	lex::state lex;
	for(const std::filesystem::path input_file : args.input_files)
	{
		lex.tokenised_input_files[input_file] = lex::tokenise(input_file);
	}
	t.lexing = timer::elapsed_millis();

	// parse
	timer::start();
	parse::state parse;
	for(const std::filesystem::path input_file : args.input_files)
	{
		parse.parsed_input_files[input_file] = parse::tokens(lex.tokenised_input_files[input_file].tokens);
	}
	t.parsing = timer::elapsed_millis();

	// buildmeta

	// semal
	
	// codegen

	// link

	t.print();
	return 0;
}

config::compiler_args parse_args(std::span<const std::string_view> args)
{
	config::compiler_args ret;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const std::string_view& arg = args[i];
		const std::string_view* argnext = nullptr;

		auto getargnext = [&i, &arg, &argnext]()
		{
			diag::assert_that(argnext != nullptr, error_code::badargs, "argument \"{}\" requires a proceeding argument value, but you didn't provide one.", arg);
			i++;
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
		else if(arg == "-l")
		{
			ret.linker_name = getargnext();
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
			ret.input_files.push_back(input_file);
		}
	}	
	return ret;
}

void print_version_info()
{
	std::cout << "OVERVIEW: Psy Compiler\n\n";
	std::cout << "USAGE: psyc [options] files...\n\n";
	std::cout << "OPTIONS:\n";
	constexpr int width = 22;
	constexpr int width2 = 42;
	std::cout << std::setw(width) << std::left << "-v" << std::setw(width2) << "display version info and help." << std::endl;
	std::cout << std::setw(width) << std::left << "-o [dir]" << std::setw(width2) << "designates an output directory." << std::endl;
	std::cout << std::setw(width) << std::left << "-t [target-name]" << std::setw(width2) << "tell the compiler to find a build meta-region of the given name to treat as build instructions." << std::endl;
	std::cout << std::setw(width) << std::left << "-l [linker-name]" << std::setw(width2) << "specifies which linker to use." << std::endl;

	std::cout << "\nFILES:\n";
	std::cout << "- Must be absolute or relative paths (with respect to current working directory).\n";
	std::cout << "- Contain valid psy source code.\n";
	std::cout << "\n";
}
