#include "config.hpp"
#include "lex.hpp"
#include "parser.hpp"
#include "timer.hpp"
#include "diag.hpp"
#include "type.hpp"
#include "profile.hpp"
#include <filesystem>
#include <span>
#include <string_view>
#include <iomanip>

//temp
#include <thread>
#include <chrono>

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
		constexpr int width = 13;
		std::cout << "\n========= SUMMARY =========\n";
		std::cout << std::left << std::setw(width) << "buildmeta:" << std::setw(6) << std::setprecision(3) << std::right << (this->buildmeta / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "lexer:" << std::setw(6) << std::setprecision(3) << std::right << (this->lexing / 1000.0f) << " seconds" << std::endl;
		std::cout << std::left << std::setw(width) << "parser:" << std::setw(6) << std::setprecision(3) << std::right << (this->parsing / 1000.0f) << " seconds" << std::endl;
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
	PROFZONE("psyc main");
	PROFZONE_BEGIN(init);
	const std::vector<std::string_view> cli_args(argv + 1, argv + argc);
	timers t;

	type_system sys;
	sys.make_struct("morbinston")
		.add_member("nested", "i64")
		.build();

	sys.make_struct("morbin2")
		.add_member("unrelated", "f32")
		.add_member("morb", "morbinston")
		.add_member("morbius", "u0")
		.build();

	type_ptr my_func_ty = sys.get_function_type("i64", {"i32", "f64", "typeinfo"});
	type_ptr my_other_func_ty = sys.get_function_type("u0", {});
	volatile typeconv fn_conv = my_func_ty->can_explicitly_convert_to(*my_other_func_ty);
	volatile typeconv fn_p_conv = sys.get_type("morbinston")->ref()->can_explicitly_convert_to(*my_func_ty);
	volatile typeconv p_fn_conv = my_func_ty->can_explicitly_convert_to(*sys.get_type("morbinston")->ref());
	type_ptr my_arr_ty = sys.get_type("i64[123]");
	diag::assert_that(my_arr_ty != nullptr, error_code::type, "couldnt resolve i64[123] to a valid type");
	diag::assert_that(my_arr_ty->is_array(), error_code::type, "type system doesnt think i64[123] is an array type...");
	diag::assert_that(static_cast<array_type*>(my_arr_ty.get())->get_element_type()->is_primitive(), error_code::type, "type system doesnt think element type of i64[123] is a primitive.");

	static_value i64_literal_123
	{
		.ty = sys.get_primitive_type(primitive::i64),
		.val = std::int64_t{123}
	};
	static_value val2 = i64_literal_123.do_explicit_convert(sys.get_primitive_type(primitive::u32), {});
	auto result = val2.value_as<std::uint32_t>();

	primitive_type ty{primitive::f64};
	ty.quals = static_cast<type_qualifier>(qual_const | qual_weak);
	diag::note("{}", ty.ref()->get_qualified_name());

	parse::populate_parse_table();

	/*
	build::info binfo;
	{
		config::compiler_args args = parse_args(cli_args);
		// buildmeta
		std::size_t metalex = 0;
		std::size_t metaparse = 0;
		std::size_t metasemal = 0;
		std::size_t metacodegen = 0;
		binfo = build::gather(args, &metalex, &metaparse, &metasemal, &metacodegen);
		t.buildmeta = metalex + metaparse + metasemal + metacodegen;

		for(std::filesystem::path extra : binfo.extra_input_files)
		{
			binfo.compiler_args.input_files.push_back(extra);
		}
	}
	*/
	// remember: binfo might have extra input files than was specified in the command line.
	// this is because build meta regions are allowed to add extra inputs.
	//const config::compiler_args& args = binfo.compiler_args;
	config::compiler_args args = parse_args(cli_args);

	PROFZONE_END(init);
	PROFZONE_BEGIN(lex);
	// lex
	timer::start();
	lex::state lex;
	for(const std::filesystem::path input_file : args.input_files)
	{
		lex.tokenised_input_files[input_file] = lex::tokenise(input_file);
	}
	t.lexing = timer::elapsed_millis();

	PROFZONE_END(lex);
	PROFZONE_BEGIN(parse);
	// parse
	timer::start();
	parse::state parse;
	for(const std::filesystem::path input_file : args.input_files)
	{
		parse.parsed_input_files[input_file] = parse::tokens(lex.tokenised_input_files[input_file]);
		if(args.should_dump_ast)
		{
			parse.parsed_input_files[input_file].pretty_print();
		}
	}
	t.parsing = timer::elapsed_millis();
	PROFZONE_END(parse);
	PROFZONE_BEGIN(semal);

	// semal
	timer::start();
	/*
	type_system tsys;
	semal::program prog;
	for(const std::filesystem::path input_file : args.input_files)
	{
		const syntax::node& ast = parse.parsed_input_files[input_file];
		semal::program subprogram = semal::analyse_file(ast, tsys);
		for(const auto&[name, unit] : subprogram.module_units)
		{
			prog.module_units[name].merge(unit);
		}
		prog.local_file_units[input_file].merge(subprogram.local_file_units[input_file]);
	}
	t.semal = timer::elapsed_millis();
	PROFZONE_END(semal);
	*/

	/*
	// codegen
	timer::start();
	code::state codegen;
	linkage::state link;
	for(const auto& [input_file, semantic_output] : semal.analysed_input_files)
	{
		codegen.codegend_input_files[input_file] = code::generate(parse.parsed_input_files[input_file], semantic_output, binfo, input_file.string());
		if(args.should_dump_ir)
		{
			std::cout << "==========================\n";
			std::cout << "ir for " << input_file << ":\n";
			std::cout << codegen.codegend_input_files[input_file].dump_ir();
			std::cout << "\n==========================\n\n";
		}
		codegen.codegend_input_files[input_file].write_to_object_file(binfo);
		link.input_output_files[input_file] = binfo.compiler_args.output_dir / codegen.codegend_input_files[input_file].get_output_filename();

		codegen.codegend_input_files[input_file].codegen_handle = nullptr;
		code::cleanup();
	}

	t.codegen = timer::elapsed_millis();
	// link
	timer::start();
	link.build(binfo);

	t.link = timer::elapsed_millis();
	*/
	t.print();

	//code::static_terminate();
	//std::this_thread::sleep_for(std::chrono::seconds(1));
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
