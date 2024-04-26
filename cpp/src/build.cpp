#include "build.hpp"
#include "diag.hpp"
#include "codegen.hpp"
#include "link.hpp"
#include "semantic.hpp"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
// note: do not remove this, even if your editor says its unused. it's used.
#include "llvm/ExecutionEngine/MCJIT.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"

build::info* cur_build_info = nullptr;

void set_optimisation(std::uint64_t optval)
{
	diag::assert_that(optval >= 0 && optval <= 3, std::format("unknown optimisation level \"{}\". expected a number between 0-3.", optval));
	cur_build_info->optimisation_level = optval;
}

void set_output(const char* name)
{
	cur_build_info->output_name = name;
}

void set_linkage(const char* linkage_cstr)
{
	std::string linkage = linkage_cstr;
	if(linkage == "executable")
	{
		cur_build_info->link = build::linkage_type::executable;
	}
	else if(linkage == "library")
	{
		cur_build_info->link = build::linkage_type::library;
	}
	else if(linkage == "none")
	{
		cur_build_info->link = build::linkage_type::none;
	}
	else
	{
		diag::fatal_error(std::format("unknown linkage \"{}\". must be either \"executable\", \"library\", or \"none\"", linkage));
	}
}

void install_functions(llvm::ExecutionEngine& exe)
{
	exe.addGlobalMapping("set_optimisation", reinterpret_cast<std::uintptr_t>(&set_optimisation));
	exe.addGlobalMapping("set_output", reinterpret_cast<std::uintptr_t>(&set_output));
	exe.addGlobalMapping("set_linkage", reinterpret_cast<std::uintptr_t>(&set_linkage));
}

std::vector<ast::node> get_build_prefix()
{
	return
	{
		ast::node{.payload = ast::function_definition
		{
			.function_name = "set_optimisation",
			.params = 
			{
				ast::variable_declaration
				{
					.var_name = "optval",
					.type_name = "i64",
					.array_size = 0,
				}	
			},
			.return_type = "u0",
			.is_extern = true,
		}},
		ast::node{.payload = ast::function_definition
		{
			.function_name = "set_output",
			.params = 
			{
				ast::variable_declaration
				{
					.var_name = "output_name",
					.type_name = "i8*",
					.array_size = 0,
				}	
			},
			.return_type = "u0",
			.is_extern = true,
		}},
		ast::node{.payload = ast::function_definition
		{
			.function_name = "set_linkage",
			.params = 
			{
				ast::variable_declaration
				{
					.var_name = "output_name",
					.type_name = "i8*",
					.array_size = 0,
				}	
			},
			.return_type = "u0",
			.is_extern = true,
		}}
	};
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace build
{
	const ast::node* find_target(std::string_view target_name, const ast& tree);
	void do_build(const session& ses, const ast::node& node);

	info gather(session& ses)
	{
		ses.target_triple = llvm::sys::getDefaultTargetTriple();
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		// first get the target
		// go through all parsed source files and find the meta-region corresponding to the target.
		const ast::node* target = nullptr;
		for(const ast& tree : ses.parsed_files)
		{
			const ast::node* found = find_target(ses.target, tree);
			if(target == nullptr)
			{
				target = found;
			}
			else
			{
				diag::assert_that(found == nullptr, std::format("found multiple meta regions named \"{}\". meta-regions must be uniquely named across the whole program.", ses.target));
			}
		}

		ast::node default_empty_target
		{
			.payload = ast::meta_region{.region_name = default_target}
		};
		if(target == nullptr)
		{
			// couldnt find the target meta region
			if(ses.target == default_target)
			{
				// default means user never specified one. they probably dont care. just use the default.
				diag::message(std::format("default target \"{}\" was not found. defaulting to an empty target...", ses.target));
				target = &default_empty_target;
			}
			else
			{
				diag::fatal_error(std::format("could not find target \"{}\"", ses.target));
			}
		}

		const ast::node& node = *target;

		// run the build-meta-region
		ast::node node_as_function;
		node_as_function.payload = ast::function_definition{.function_name = "main", .params = {}, .return_type = "i64"};
		node_as_function.children = node.children;

		// finally, add a return to the end of the main function
		node_as_function.children.push_back(ast::node{.payload = ast::return_statement
		{
			.value = ast::expression{.expr = ast::integer_literal{.val = 0}}
		}});

		// run the program and retrieve the results we need.
		ast::node program_node;
		program_node.children = get_build_prefix();
		program_node.children.push_back(node_as_function);
		ast node_as_program{.program = program_node};
		std::string error;

		info binfo;
		{
			semantic::state state = semantic::analysis(node_as_program);
			codegen::static_initialise();
			codegen::generate(node_as_program, state, "build");
			std::unique_ptr<llvm::Module> program_to_move = codegen::pop();
			llvm::Module* program = program_to_move.get();
			llvm::ExecutionEngine* exe = llvm::EngineBuilder(std::move(program_to_move))
			.setErrorStr(&error)
			.setMCJITMemoryManager(std::make_unique<llvm::SectionMemoryManager>())
			.create();
			diag::assert_that(exe != nullptr, std::format("internal compiler error: failed to create LLVM execution engine while trying to execute build meta-region: {}", error));

			cur_build_info = &binfo;
			install_functions(*exe);

			int (*func)() = (int (*)())exe->getFunctionAddress("main");

			// run the program.
			int ret = func();

			diag::assert_that(ret == 0, std::format("build meta-region execution returned non-zero exit code: {}", ret));
			// note: tearing down all this state is actually ridiculously error prone.
			// cant unlink globals until the program stops referencing them.
			// globals must all be destroyed before program itself dies.
			// so:
			// 1.) program stops referencing everything
			program->dropAllReferences();
			// 2.) unlink globals from parent (but doesnt erase them)
			// 3.) kill globals and then program etc...
			codegen::cleanup_program();
		}
		codegen::static_terminate();
		return binfo;
	}

	void go(const session& ses, const info& binfo)
	{
		// build the target.
		switch(binfo.link)
		{
			case linkage_type::library:
				link::library(ses.object_files, ses.output_dir, binfo.output_name, ses.linker);
			break;
			case linkage_type::executable:
				link::executable(ses.object_files, ses.output_dir, binfo.output_name, ses.linker);
			break;
			case linkage_type::none:
				// no need to do anything. object files were already created.
				return;
			break;
		}
	}

	void parse_assignment(info& binfo, std::string_view lhs, const ast::expression& rhs);

	// find the meta region corresponding to the target name within an AST (i.e one of the source files).

	const ast::node* find_target(std::string_view target_name, const ast::node& node)
	{
		if(std::holds_alternative<ast::meta_region>(node.payload))
		{
			const auto& region = std::get<ast::meta_region>(node.payload);
			if(region.region_name == target_name)
			{
				return &node;
			}
		}
		for(const ast::node& child : node.children)
		{
			auto maybe_node = find_target(target_name, child);
			if(maybe_node != nullptr)
			{
				return maybe_node;
			}
		}
		return nullptr;
	}

	const ast::node* find_target(std::string_view target_name, const ast& tree)
	{
		return find_target(target_name, tree.program);
	}
}