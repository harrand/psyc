#include "diag.hpp"
#include "build.hpp"
#include "session.hpp"
#include "parse.hpp"
#include "semantic.hpp"
#include "codegen.hpp"
#include "util.hpp"

#include <filesystem>
#include <span>
#include <string_view>
#include <vector>
#include <format>
#include <fstream>
#include <sstream>

/*
	you're in the main psy compiler source file. you can hopefully follow through the overall logic here.

	psyc works in a few steps:
	- argument parsing (what do you want me to build? which flags? which output?)
	- lexing (reading through the .psy sources and making a big list of tokens for each input file. these tokens provide more context about what syntax the programmer has written.)
	- parsing (stitching those tokens together to figure out what the code is doing (e.g if you put
		an identifier `foo` with a pair of parentheses `()`, you're probably trying to call a function `foo()`)).
		parsing ends up generating a massive tree of these more complicated source behaviours, called an abstract syntax tree (AST)
	- semantic analysis (going through the generated AST, verifying that things make sense e.g making sure called functions were defined previously, type checking, etc...)
	- code generation. if everything is verified then we go through the AST and generate output code (whether thats x86 assembly, or llvm ir depends on argparsing from earlier). you ultimately end up with something vaguely resembling an executable.
*/

void parse_args(std::span<const std::string_view> args, session& ses)
{
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
		else if(arg.starts_with("-o"))
		{
			std::string_view output_dir = *argnext;
			ses.output_dir = std::string{output_dir};
			i++;
			continue;
		}
		else if(arg.starts_with("-l") || arg.starts_with("--linker"))
		{
			std::string_view linker_name = *argnext;
			ses.linker = std::string{linker_name};
			i++;
			continue;
		}
		else if(arg.starts_with("-t") || arg.starts_with("--target"))
		{
			ses.target = *argnext;
			i++;
			continue;
		}
		else if(arg.starts_with("--dump-ast"))
		{
			// would you like me to write the AST into stdout?
			ses.flags |= flag::dump_ast;
		}
		else if(arg.starts_with("--dump-tokens"))
		{
			// would you like me to write out the entire list of tokens into stdout? it may or may not be completely unreadable, but is useful for compiler debugging.
			ses.flags |= flag::dump_tokens;
		}
		else if(arg.starts_with("--no-stdlib"))
		{
			ses.flags |= flag::no_stdlib;
		}
		else
		{
			if(arg.starts_with("-"))
			{
				// you're giving me an option but i dont know what the fuck you've written m8.
				diag::fatal_error(std::format("unrecognised cli option \"{}\"", arg));
			}
			// im just gonna assume you're telling me which files to compile.
			// they better end with .psy or ima fuck ur shit up (maybe remove this, linux andies dont like meaningful file extensions)
			diag::assert_that(arg.ends_with(".psy"), std::format("input file \"{}\" does not end with `.psy`", arg));
			ses.input_files.push_back(std::string{arg});
		}
	}
}

void add_stdlib(session& ses)
{
	std::filesystem::path psyc_parent_dir = util::get_this_executable_path().parent_path();	
	for(std::filesystem::path file : std::filesystem::directory_iterator(psyc_parent_dir))	
	{
		if(file.extension() == ".psy")
		{
			ses.input_files.push_back(file.string());
		}
	}
}

int main(int argc, char** argv)
{
	const std::vector<std::string_view> args(argv + 1, argv + argc);
	session ses;
	parse_args(args, ses);
	if(ses.flags | flag::no_stdlib)
	{
		add_stdlib(ses);
	}
	ses.lexed_files.resize(ses.input_files.size());
	ses.parsed_files.resize(ses.input_files.size());
	diag::assert_that(ses.input_files.size(), "no input files specified");
	volatile std::filesystem::path my_path = util::get_this_executable_path();
	// for every .psy file you gave me, i go through **the process**.
	for(std::size_t i = 0; i < ses.input_files.size(); i++)
	{
		const auto& input_file = ses.input_files[i];
		auto& tokens = ses.lexed_files[i];
		auto& ast = ses.parsed_files[i];
		// did you give me a real file or is this some bollocks?
		diag::assert_that(std::filesystem::exists(input_file), std::format("cannot find input file `{}`", input_file));
		std::ifstream fstr(input_file);
		// slurp the whole file into a string.
		std::stringstream buffer;
		buffer << fstr.rdbuf();
		std::string str = buffer.str();
		// perform lexing.
		tokens = lexer::lex(str);
		if(ses.flags & flag::dump_tokens)
		{
			// print out the tokens if you asked for it earlier.
			std::cout << "========== " << "dump-tokens \"" << input_file << "\" ==========\n";
			for(const auto& tok : tokens)
			{
				std::cout << tok.to_string();
			}
			std::cout << "\n==========\n";
		}
		// perform parsing.
		ast = parser::parse(tokens);
		if(ses.flags & flag::dump_ast)
		{
			// print out the ast if you asked for it earlier.
			std::cout << "========== " << "dump-ast \"" << input_file << "\" ==========\n";
			ast.pretty_print();
			std::cout << "==========\n";
		}
		// perform semantic analysis.
		semantic::analysis(ast);
		// finally, generate code
		std::string just_filename = std::filesystem::path(input_file).stem().string();
		ses.object_files.push_back(codegen::generate(ast, (std::filesystem::path(ses.output_dir) / just_filename).string()));
	}
	build::go(ses);
	return 0;
}
