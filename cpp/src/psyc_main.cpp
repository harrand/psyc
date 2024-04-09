#include "diag.hpp"
#include "parse.hpp"
#include "lex.hpp"
#include "semantic.hpp"

#include <span>
#include <string_view>
#include <vector>
#include <format>
#include <filesystem>
#include <fstream>
#include <sstream>

enum class output
{
	unknown,
	llvm_ir,
	x86_64_asm,
};

using flag_t = std::uint8_t;
enum flag : flag_t
{
	verbose = 0b0000001,
};

struct session
{
	std::vector<std::string> input_files = {};
	std::vector<lexer::tokens> lexed_files = {};
	std::vector<parser::ast> parsed_files = {};
	output type = output::unknown;
	flag_t flags = {};
};

void parse_args(std::span<const std::string_view> args, session& ses)
{
	ses.type = output::unknown;
	for(std::size_t i = 0; i < args.size(); i++)
	{
		const std::string_view& arg = args[i];
		const std::string_view* argnext = nullptr;
		if(i < (args.size() - 1))
		{
			argnext = &args[i + 1];
		}
		if(arg == "-v")
		{
			std::cout << "psy compiler\n\tinvocation: \"psyc";
			for(const auto& tmparg : args)
			{
				std::cout << " " << tmparg;
			}
			std::cout << "\"\n";
		}
		else if(arg.starts_with("-ot") || arg.starts_with("--output_type"))
		{
			if(*argnext == "llvm")
			{
				ses.type = output::llvm_ir;
				diag::warning("llvm IR code generation is not yet implemented. no binary will be generated.");
			}
			else if(*argnext == "x86_64_asm")
			{
				ses.type = output::x86_64_asm;
				diag::warning("x86_64_asm code generation is not yet implemented. no binary will be generated.");
			}
			else
			{
				diag::fatal_error(std::format("unknown output type `{} (from \"{}\")", *argnext, std::string(arg) + " " + std::string(*argnext)));
			}
			i++;
			continue;
		}
		else if(arg.starts_with("--dump-ast"))
		{
			ses.flags |= flag::verbose;
		}
		else
		{
			if(arg.starts_with("-"))
			{
				diag::fatal_error(std::format("unrecognised cli option \"{}\"", arg));
			}
			diag::assert_that(arg.ends_with(".psy"), std::format("input file \"{}\" does not end with `.psy`", arg));
			ses.input_files.push_back(std::string{arg});
		}
	}
}

int main(int argc, char** argv)
{
	const std::vector<std::string_view> args(argv + 1, argv + argc);
	session ses;
	parse_args(args, ses);
	diag::assert_that(ses.type != output::unknown, "no output type specified (`-ot / --output_type`)");
	ses.lexed_files.resize(ses.input_files.size());
	ses.parsed_files.resize(ses.input_files.size());
	diag::assert_that(ses.input_files.size(), "no input files specified");
	for(std::size_t i = 0; i < ses.input_files.size(); i++)
	{
		const auto& input_file = ses.input_files[i];
		auto& tokens = ses.lexed_files[i];
		auto& ast = ses.parsed_files[i];
		diag::assert_that(std::filesystem::exists(input_file), std::format("cannot find input file `{}`", input_file));
		// tokens = lex(input_file)
		std::ifstream fstr(input_file);
		std::stringstream buffer;
		buffer << fstr.rdbuf();
		std::string str = buffer.str();
		tokens = lexer::lex(str);
		ast = parser::parse(tokens);
		if(ses.flags & flag::verbose)
		{
			std::cout << "========== " << "input file \"" << input_file << "\" ==========\n";
			std::cout << "=== ast ===\n";
			ast.pretty_print();
		}
		semantic::analysis(ast);
		volatile int x = 5;
	}
}
