#include "build.hpp"
#include "lex.hpp"
#include "parse.hpp"
#include "semal.hpp"
#include "codegen.hpp"
#include "diag.hpp"
#include "timer.hpp"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
// note: do not remove this, even if your editor says its unused. it's used.
#include "llvm/ExecutionEngine/MCJIT.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"

#ifdef _WIN32
#include <windows.h>
#endif

build::info* cur_build_info = nullptr;

void set_linkage_type(std::int64_t link_type_value)
{
	diag::assert_that(link_type_value < (int)build::linkage_type::_count, error_code::buildmeta, "unexpected linkage type \"{}\"", link_type_value);
	cur_build_info->link = static_cast<build::linkage_type>(link_type_value);	
}

void set_build_type(std::int64_t build_type_value)
{
	diag::assert_that(build_type_value < (int)build::config_type::_count, error_code::buildmeta, "unexpected build type \"{}\"", build_type_value);
	cur_build_info->config = static_cast<build::config_type>(build_type_value);
}

void set_output_name(const char* name)
{
	cur_build_info->link_name = name;
}

void add_source(const char* name)
{
	cur_build_info->extra_input_files.push_back(name);
}

void add_link_library(const char* link_library)
{
	cur_build_info->link_libraries.push_back(link_library);
}

void use_stdlib(std::uint8_t use)
{
	cur_build_info->using_stdlib = use;
}

void install_functions(llvm::ExecutionEngine& exe)
{
	exe.addGlobalMapping("set_linkage_type", reinterpret_cast<std::uintptr_t>(&set_linkage_type));
	exe.addGlobalMapping("set_build_type", reinterpret_cast<std::uintptr_t>(&set_build_type));
	exe.addGlobalMapping("set_output_name", reinterpret_cast<std::uintptr_t>(&set_output_name));
	exe.addGlobalMapping("add_source", reinterpret_cast<std::uintptr_t>(&add_source));
	exe.addGlobalMapping("add_link_library", reinterpret_cast<std::uintptr_t>(&add_link_library));
	exe.addGlobalMapping("use_stdlib", reinterpret_cast<std::uintptr_t>(&use_stdlib));
}

const char* get_output_extension(build::linkage_type link)
{
	const char* extension = "";
	switch(link)
	{
		case build::linkage_type::executable:
			#ifdef _WIN32
				extension = ".exe";
			#elif defined(__linux__)
				extension = ".out";
			#else
				static_assert("unknown platform");
			#endif
		break;
		case build::linkage_type::library:
			#ifdef _WIN32
				extension = ".lib";
			#elif defined(__linux__)
				extension = ".a";
			#else
				static_assert("unknown platform");
			#endif
		break;
		default: break;
	}
	return extension;
}

namespace build
{

	ast::node try_find_build_meta_region(const info& i, std::string_view name, std::size_t* lex_timers, std::size_t* parse_timers);
	ast turn_meta_region_into_program(std::string_view region_name, ast::node meta_region);

	info gather(const config::compiler_args& args, std::size_t* lex_timers, std::size_t* parse_timers, std::size_t* semal_timers, std::size_t* codegen_timers)
	{
		info ret;
		ret.compiler_args = args;
		ret.target_triple = llvm::sys::getDefaultTargetTriple();
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();

		// try to find the node that constitutes a build meta-region with the same name as the target provided.
		ast::node meta_region = try_find_build_meta_region(ret, ret.compiler_args.target_name, lex_timers, parse_timers);
		diag::assert_that(meta_region != ast::node{}, error_code::buildmeta, "build meta region target \"{}\" could not be found in the provided input file(s)", ret.compiler_args.target_name);
		// turn that meta region into a function definition that we can jit compile + run.
		ast metaprogram = turn_meta_region_into_program(ret.compiler_args.target_name, meta_region);

		timer::start();
		semal::output semal_predecl = semal::analyse_predecl(metaprogram);
		semal::output semal = semal::analyse_full(metaprogram, semal_predecl);
		if(semal_timers != nullptr)
		{
			*semal_timers += timer::elapsed_millis();
		}
		code::output metacode = code::generate(metaprogram, semal, ret, "buildmeta");
		// we now own the codegen handle.
		llvm::Module* metaprogram_handle = static_cast<llvm::Module*>(code::unsafe_release());
		// pass ownership directly into ExecutionEngine.
		std::string error;
		{
			llvm::ExecutionEngine* exe = llvm::EngineBuilder(std::unique_ptr<llvm::Module>(metaprogram_handle))
			.setErrorStr(&error)
			.setMCJITMemoryManager(std::make_unique<llvm::SectionMemoryManager>())
			.create();
			diag::assert_that(exe != nullptr, error_code::ice, "failed to create LLVM execution engine while trying to execute build meta-region: {}", error);

			// todo: install functions.

			// retrieve the following from LLVM JIT:
			// - DONE linkage type
			// - DONE config type
			// - TODO output name (if linkage type is not none)
			// - TODO any extra input .psy files to compile.
			install_functions(*exe);
			int (*metafunc)() =(int (*)())exe->getFunctionAddress(ret.compiler_args.target_name);
			// run the program.
			cur_build_info = &ret;
			int ret = metafunc();
			cur_build_info = nullptr;

			// do a sane cleanup.
			// remember: at the end of this scope, ExecutionEngine dies which means the llvm::Module that it owns dies too.
			// code cleanup releases the globals etc, but we need to release references manually first.
			metaprogram_handle->dropAllReferences();
			code::cleanup();
		}
		ret.link_name += get_output_extension(ret.link);
		if(ret.using_stdlib)
		{
			std::string psyc_path;
			#ifdef _WIN32
				psyc_path.resize(MAX_PATH);
				GetModuleFileNameA(nullptr, psyc_path.data(), psyc_path.size());
			#endif
			std::filesystem::path psyc_dir = std::filesystem::path{psyc_path}.parent_path();
			std::filesystem::path stdlib_path = psyc_dir / "stdlib.psy";
			diag::assert_that(std::filesystem::exists(stdlib_path), error_code::buildmeta, "could not find stdlib implementation. expected to find: \"{}\"", stdlib_path.string());
			ret.extra_input_files.push_back(stdlib_path);
		}
		return ret;
	}

	std::filesystem::path info::get_output_path() const
	{
		return this->compiler_args.output_dir / this->link_name;
	}

	ast::node try_find_build_meta_region_from_tree(std::string_view name, const ast& tree, ast::path_t path)
	{
		const ast::node& node = tree.get(path);
		if(std::holds_alternative<ast::meta_region>(node.payload))
		{
			const auto& region = std::get<ast::meta_region>(node.payload);
			if(region.name == name && region.type == "build")
			{
				return node;
			}
		}

		for(std::size_t i = 0; i < node.children.size(); i++)
		{
			auto child_path = path;
			child_path.push_back(i);
			auto maybe_result = try_find_build_meta_region_from_tree(name, tree, child_path);
			if(maybe_result != ast::node{})
			{
				return maybe_result;
			}
		}
		return ast::node{};
	}

	ast::node try_find_build_meta_region(const info& i, std::string_view name, std::size_t* lex_timers, std::size_t* parse_timers)
	{
		for(const std::filesystem::path path : i.compiler_args.input_files)
		{
			timer::start();
			lex::output tokens = lex::tokenise(path);
			if(lex_timers != nullptr)
			{
				*lex_timers += timer::elapsed_millis();
			}

			timer::start();
			ast tree = parse::tokens(tokens.tokens);
			if(parse_timers != nullptr)
			{
				*parse_timers += timer::elapsed_millis();
			}

			auto maybe_result = try_find_build_meta_region_from_tree(name, tree, {});
			if(maybe_result != ast::node{})
			{
				return maybe_result;
			}
		}
		return ast::node{};
	}

	ast turn_meta_region_into_program(std::string_view region_name, ast::node meta_region)
	{
		ast ret;

		// firstly we define some magical buildmeta-only extern functions.
		ret.root.children.push_back(ast::node
		{
			.payload = ast::function_definition
			{
				.func_name = "set_linkage_type",
				.params =
				{
					ast::variable_declaration
					{
						.var_name = "lnkval",
						.type_name = "i64",
						.initialiser = ast::expression{.expr = ast::integer_literal{.val = 0}}
					},
				},
				.ret_type = "u0",
				.is_extern = true
			}
		});
		ret.root.children.push_back(ast::node
		{
			.payload = ast::function_definition
			{
				.func_name = "set_build_type",
				.params =
				{
					ast::variable_declaration
					{
						.var_name = "optval",
						.type_name = "i64",
						.initialiser = ast::expression{.expr = ast::integer_literal{.val = 0}}
					},
				},
				.ret_type = "u0",
				.is_extern = true
			}
		});
		ret.root.children.push_back(ast::node
		{
			.payload = ast::function_definition
			{
				.func_name = "set_output_name",
				.params =
				{
					ast::variable_declaration
					{
						.var_name = "outname",
						.type_name = "i8& const",
						.initialiser = ast::expression{.expr = ast::null_literal{}}
					},
				},
				.ret_type = "u0",
				.is_extern = true
			}
		});
		ret.root.children.push_back(ast::node
		{
			.payload = ast::function_definition
			{
				.func_name = "add_source",
				.params =
				{
					ast::variable_declaration
					{
						.var_name = "filename",
						.type_name = "i8& const",
						.initialiser = ast::expression{.expr = ast::null_literal{}}
					},
				},
				.ret_type = "u0",
				.is_extern = true
			}
		});
		ret.root.children.push_back(ast::node
		{
			.payload = ast::function_definition
			{
				.func_name = "add_link_library",
				.params =
				{
					ast::variable_declaration
					{
						.var_name = "filename",
						.type_name = "i8& const",
						.initialiser = ast::expression{.expr = ast::null_literal{}}
					},
				},
				.ret_type = "u0",
				.is_extern = true
			}
		});
		ret.root.children.push_back(ast::node
		{
			.payload = ast::function_definition
			{
				.func_name = "use_stdlib",
				.params =
				{
					ast::variable_declaration
					{
						.var_name = "use",
						.type_name = "bool",
						.initialiser = ast::expression{.expr = ast::bool_literal{.val = true}}
					},
				},
				.ret_type = "u0",
				.is_extern = true
			}
		});


		// same with some globals.
		for(int i = 0; i < (int)build::linkage_type::_count; i++)
		{
			ret.root.children.push_back(ast::node{.payload = ast::expression{.expr =
			{
				ast::variable_declaration
				{
					.var_name = build::linkage_type_names[i],
					.type_name = "i64",
					.initialiser = ast::expression{.expr = ast::integer_literal{.val = i}}
				}
			}}});
		}
		for(int i = 0; i < (int)build::config_type::_count; i++)
		{
			ret.root.children.push_back(ast::node{.payload = ast::expression{.expr =
			{
				ast::variable_declaration
				{
					.var_name = build::config_type_names[i],
					.type_name = "i64",
					.initialiser = ast::expression{.expr = ast::integer_literal{.val = i}}
				}
			}}});
		}

		meta_region.payload = ast::function_definition{.func_name = std::string{region_name}, .ret_type = type::from_primitive(primitive_type::i64).name()};
		// because its become a function that returns an integer, we *must* sneak in a return statement:
		meta_region.children.front().children.push_back(ast::node
		{
			.payload = ast::expression
			{
				.expr = ast::return_statement
				{
					.expr = ast::expression{.expr = ast::integer_literal{.val = 0}}
				}
			}
		});
		ret.root.children.push_back(meta_region);

		return ret;
	}
}