#include "build.hpp"
#include "diag.hpp"
#include "codegen.hpp"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"


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
		std::optional<std::size_t> optimisation_level = std::nullopt;
		std::optional<linkage_type> link = std::nullopt;
		std::optional<std::string> output_name = std::nullopt;
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

		if(target == nullptr)
		{
			diag::fatal_error(std::format("could not find target \"{}\"", ses.target));
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
			.initialiser = ast::expression{.expr = ast::integer_literal{0}},
		}});
		// then link values (executable, library and none)
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "executable",
			.type_name = "string",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::string_literal{.val = "executable"}},
		}});
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "library",
			.type_name = "string",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::string_literal{.val = "library"}},
		}});
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "none",
			.type_name = "string",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::string_literal{.val = "none"}},
		}});
		// then the link variable itself.
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "link",
			.type_name = "string",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::identifier{.name = "none"}},
		}});
		// then the last variable - output name.
		#ifdef _WIN32
			constexpr const char* default_output_name = "a.exe";
		#else
			constexpr const char* default_output_name = "a.out";
		#endif
		pre_instructions.push_back(ast::node{.payload = ast::variable_declaration
		{
			.var_name = "output",
			.type_name = "string",
			.array_size = 0,
			.initialiser = ast::expression{.expr = ast::string_literal{.val = default_output_name}},
		}});
		// finally, add a return to the end of the main function
		node_as_function.children.push_back(ast::node{.payload = ast::return_statement
		{
			.value = ast::expression{.expr = ast::integer_literal{.val = 0}}
		}});

		// add the pre instructions.
		node_as_function.children.insert(node_as_function.children.begin(), pre_instructions.begin(), pre_instructions.end());

		// run the program and retrieve the results we need.
		ast::node program_node;
		program_node.children = {node_as_function};
		ast node_as_program{.program = program_node};
		std::string error;

		llvm::ExecutionEngine* exe = llvm::EngineBuilder(codegen::static_generate(node_as_program, "build"))
		.setErrorStr(&error)
		.setMCJITMemoryManager(std::make_unique<llvm::SectionMemoryManager>())
		.create();
		diag::assert_that(exe != nullptr, std::format("internal compiler error: failed to create LLVM execution engine while trying to execute build meta-region: {}", error));
		int (*func)() = (int (*)())exe->getFunctionAddress("main");
		// run the program.
		int ret = func();
		diag::assert_that(ret == 0, std::format("build meta-region execution returned non-zero exit code: {}", ret));

		codegen::static_terminate();
		build_info binfo;
		/*
		diag::assert_that(std::holds_alternative<ast::meta_region>(node.payload), "internal compiler error: node used as build target was not a meta region.");
		build_info binfo;
		auto build_target = std::get<ast::meta_region>(node.payload);
		// the meta-region is expected to contain a series of variable assignments.
		for(const ast::node& child : node.children)
		{
			diag::assert_that(std::holds_alternative<ast::expression>(child.payload), "meta-parse-error: meta-region must be filled with expressions.");
			auto expr = std::get<ast::expression>(child.payload);
			using assignment_t = std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>;
			diag::assert_that(std::holds_alternative<assignment_t>(expr.expr), "meta-parse-error: each expression must be a binary operator");
			auto [op, lhs, rhs] = std::get<assignment_t>(expr.expr);
			diag::assert_that(op.type == lexer::token::type::equals, "meta-parse-error: each binary operator *must* be an assignment");

			diag::assert_that(std::holds_alternative<ast::identifier>(lhs->expr), "meta-parse-error: lhs of an assignment *must* parse as an identifier");
			parse_assignment(binfo, std::get<ast::identifier>(lhs->expr).name, *rhs);
		}

		if(!binfo.link.has_value())
		{
			diag::warning(std::format("meta-region \"{}\" did not specify linkage. defaulting to `executable`.", build_target.region_name));
			binfo.link = linkage_type::executable;
		}
		if(!binfo.optimisation_level.has_value())
		{
			diag::warning(std::format("meta-region \"{}\" did not specify optimisation level. defaulting to 0.", build_target.region_name));
			binfo.optimisation_level = 0;
		}
		if(!binfo.output_name.has_value())
		{
			#ifdef _WIN32
				constexpr const char* default_output_name = "a.exe";
			#else
				constexpr const char* default_output_name = "a.out";
			#endif
			diag::warning(std::format("meta-region \"{}\" did not specify output. defaulting to \"{}\"", build_target.region_name, default_output_name));
			binfo.output_name = default_output_name;
		}

		switch(binfo.link.value())
		{
			case linkage_type::library:
				link::library(ses.object_files, ses.output_dir, binfo.output_name.value(), ses.linker);
			break;
			case linkage_type::executable:
				link::executable(ses.object_files, ses.output_dir, binfo.output_name.value(), ses.linker);
			break;
			case linkage_type::none:
				// no need to do anything. object files were already created.
				return;
			break;
		}
		*/
	}

	std::size_t parse_opt(const ast::expression& rhs)
	{
		diag::assert_that(std::holds_alternative<ast::integer_literal>(rhs.expr), "optimisation level assignment must be an integer literal.");
		int val = std::get<ast::integer_literal>(rhs.expr).val;
		diag::assert_that(val >= 0, std::format("optimisation level must not be negative. you specified {}", val));
		diag::assert_that(val <= 2, std::format("optimisation level must not be greater than 2. you specified {}", val));
		return val;
	}

	linkage_type parse_link(const ast::expression& rhs)
	{
		diag::assert_that(std::holds_alternative<ast::identifier>(rhs.expr), "link assignment rhs must be an identifier.");
		std::string val = std::get<ast::identifier>(rhs.expr).name;
		if(val == "executable")
		{
			return linkage_type::executable;
		}
		else if(val == "library")
		{
			return linkage_type::library;
		}
		else if(val == "none")
		{
			return linkage_type::none;
		}
		diag::fatal_error(std::format("unknown linkage \"{}\". should be either `executable`, `library` or `none`.", val));
		return linkage_type::none;
	}

	std::string parse_output(const ast::expression& rhs)
	{
		diag::assert_that(std::holds_alternative<ast::string_literal>(rhs.expr), "output assignment rhs must be a string literal.");
		return std::get<ast::string_literal>(rhs.expr).val;
	}

	void parse_assignment(build_info& binfo, std::string_view lhs, const ast::expression& rhs)
	{
		if(lhs == "optimisation")
		{
			binfo.optimisation_level = parse_opt(rhs);
		}
		else if(lhs == "link")
		{
			binfo.link = parse_link(rhs);
		}
		else if(lhs == "output")
		{
			binfo.output_name = parse_output(rhs);
		}
		else
		{
			diag::fatal_error(std::format("unknown build config variable \"{}\"", lhs));
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