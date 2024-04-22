#include "build.hpp"
#include "diag.hpp"
#include "codegen.hpp"
#include "link.hpp"
#include "semantic.hpp"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
// note: do not remove this, even if your editor says its unused. it's used.
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/Constants.h"


namespace build
{
	const ast::node* find_target(std::string_view target_name, const ast& tree);
	void do_build(const session& ses, const ast::node& node);

	enum class linkage_type
	{
		none,
		executable,
		library
	};

	struct build_info
	{
		std::size_t optimisation_level = 0;
		linkage_type link = linkage_type::executable;
		std::string output_name = "a.exe";
	};

	void go(const session& ses)
	{
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

		// build the target.
		do_build(ses, *target);
	}

	void parse_assignment(build_info& binfo, std::string_view lhs, const ast::expression& rhs);

	void do_build(const session& ses, const ast::node& node)
	{
		/*
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		*/

		ast::node node_as_function;
		node_as_function.payload = ast::function_definition{.function_name = "main", .params = {}, .return_type = "i64"};
		node_as_function.children = node.children;
		// create our build variables to read from later.
		// firstly, optimisation level

		std::vector<ast::node> pre_instructions;
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "optimisation",
			.type_name = "i64",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::integer_literal{.val = 0}}
		}});
		// then the link variable itself.
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "link",
			.type_name = "i8*",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::identifier{.name = "null"}}
		}});
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "output",
			.type_name = "i8*",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::identifier{.name = "null"}}
		}});

		// finally, add a return to the end of the main function
		node_as_function.children.push_back(ast::node{.payload = ast::return_statement
		{
			.value = ast::expression{.expr = ast::integer_literal{.val = 0}}
		}});

		// run the program and retrieve the results we need.
		ast::node program_node;
		program_node.children = pre_instructions;
		program_node.children.push_back(node_as_function);
		ast node_as_program{.program = program_node};
		std::string error;

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

		auto* optimisation_ptr = reinterpret_cast<std::int64_t*>(exe->getGlobalValueAddress("optimisation"));

		int (*func)() = (int (*)())exe->getFunctionAddress("main");

		// run the program.
		int ret = func();

		// note: as link and output are pointers, we must retrieve their address *after* program runs, because assignments will re-seat the pointer.
		auto* link_ptr = *reinterpret_cast<const char**>(exe->getGlobalValueAddress("link"));
		auto* output_ptr = *reinterpret_cast<const char**>(exe->getGlobalValueAddress("output"));

		build_info binfo;
		if(std::string_view(link_ptr) == "executable")
		{
			binfo.link = linkage_type::executable;
		}
		else if(std::string_view(link_ptr) == "library")
		{
			binfo.link = linkage_type::library;
		}
		else
		{
			diag::warning(std::format("could not recognise linkage type \"{}\". it should either be `executable` or `library`. defaulting to no linkage (just generate object files)", link_ptr));
			binfo.link = linkage_type::none;
		}
		if(binfo.link != linkage_type::none && std::string_view(output_ptr).empty())
		{
			#ifdef _WIN32
				output_ptr = "a.exe";
			#else
				output_ptr = "a.out";
			#endif
			diag::warning(std::format("you've opted into linking but not provided a output name. defaulting to \"{}\"", output_ptr));
		}
		binfo.output_name = output_ptr;
		binfo.optimisation_level = *optimisation_ptr;

		diag::assert_that(ret == 0, std::format("build meta-region execution returned non-zero exit code: {}", ret));
		// note: tearing down all this state is actually ridiculously error prone.
		// cant unlink globals until the program stops referencing them.
		// globals must all be destroyed before program itself dies.
		// so:
		// 1.) program stops referencing everything
		program->dropAllReferences();
		// 2.) unlink globals from parent (but doesnt erase them)
		// 3.) kill globals and then program etc...
		codegen::static_terminate();
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