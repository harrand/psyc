#include "codegen.hpp"
#include "diag.hpp"
#include "llvm/IR/LLVMContext.h"

#include "llvm/ADT/APInt.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h" 

namespace codegen
{
	static std::unique_ptr<llvm::LLVMContext> ctx = nullptr;
	static std::unique_ptr<llvm::Module> program = nullptr;
	static std::unique_ptr<llvm::IRBuilder<>> builder = nullptr;
	// global variables are RAII objects meaning we will have to deal with their lifetimes. to do that, we're just chucking them in there, and spreading the underlying ptr all around the place (as we're allocing we have pointer stability so its safe).
	static std::vector<std::unique_ptr<llvm::GlobalVariable>> global_variable_storage = {};

	void static_initialise()
	{
		ctx = std::make_unique<llvm::LLVMContext>();
	}

	void static_terminate()
	{
		diag::assert_that(program == nullptr, "internal compiler error: a llvm Module was not destroyed prior to static terminate");
		ctx = nullptr;
	}

	void cleanup_program()
	{
		global_variable_storage.clear();
		builder = nullptr;
		program = nullptr;
	}

	void codegen_structs(const ast& tree, const semantic::state& state);
	void codegen_functions(const ast& tree, const semantic::state& state);
	void codegen_global_variables(const ast& tree, const semantic::state& state);
	void codegen_single_node(const ast& tree, const semantic::state& state, ast::path_t path);

	// generate the program according to the ast and state.
	// resultant module will be left in `program`.
	void generate(const ast& tree, const semantic::state& state, std::string llvm_module_name)
	{
		diag::assert_that(ctx != nullptr, "internal compiler error: forgot to codegen::static_initialise");
		diag::assert_that(program == nullptr, "internal compiler error: call to codegen::generate when a previous program was not popped/written.");
		diag::assert_that(builder == nullptr, "internal compiler error: call to codegen::generate when a previous program's llvm-ir-builder was not popped/written.");
		program = std::make_unique<llvm::Module>(llvm_module_name, *ctx);
		builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

		// codegen the structs and functions first, as other code might use them before they're defined (which we allow).
		codegen_structs(tree, state);
		codegen_functions(tree, state);
		codegen_global_variables(tree, state);

		for(std::size_t i = 0; i < tree.program.children.size(); i++)
		{
			codegen_single_node(tree, state, ast::path_t{i});
		}
	}

	// given a previous `generate`, retrieve an owning pointer to the result.
	std::unique_ptr<llvm::Module> pop()
	{
		std::unique_ptr<llvm::Module> ret = std::move(program);
		cleanup_program();
		return ret;
	}

	// given a previous `generate`, write the resultant program to an object filename.
	std::filesystem::path write_to_object_file(std::string object_filename)
	{
		auto target_triple = llvm::sys::getDefaultTargetTriple();
		llvm::InitializeAllTargetInfos();
		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmParsers();
		llvm::InitializeAllAsmPrinters();
		std::string error;
		auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
		if(target == nullptr)
		{
			diag::error(std::format("error while retrieving LLVM output target information(s): {}", error));
		}
		const char* cpu = "generic";
		const char* features = "";
		llvm::TargetOptions opt;
		auto target_machine = target->createTargetMachine(target_triple, cpu, features, opt, llvm::Reloc::PIC_);
		// configure module (no i have no idea whats going on).
		program->setDataLayout(target_machine->createDataLayout());
		program->setTargetTriple(target_triple);
		std::error_code ec;
		llvm::raw_fd_ostream dst(object_filename, ec, llvm::sys::fs::OF_None);
		if(ec)
		{
			diag::error(std::format("error while generating object files: {}", ec.message()));
		}
		llvm::legacy::PassManager pass;
		auto file_type = llvm::CodeGenFileType::ObjectFile;
		if(target_machine->addPassesToEmitFile(pass, dst, nullptr, file_type, false))
		{
			diag::error(std::format("target machine cannot emit a file of this type."));
		}
		pass.run(*program);
		dst.flush();
		// clear out the module afterwards.
		cleanup_program();
		return std::filesystem::path{object_filename};
	}

	/////////////////////////////////////// TYPE SYSTEM ///////////////////////////////////////

	// we have a nice organised type system.
	// llvm's is much more complicated. thanks to opaque pointers, it's not that simple. for that reason, we deal with our own types as much as possible, and just convert it to its equivalent llvm type at the last possible second (thats what this function does).
	// returns nullptr in the case of an undefined type.
	// will ICE if you provided a really dodgy type.
	llvm::Type* as_llvm_type(const type& ty, const semantic::state& state)
	{
		if(ty.is_undefined())
		{
			// todo: potentially emit a compiler error? is it ever valid to pass in an undefined type?
			return nullptr;
		}
		if(ty.is_struct())
		{
			const semantic::struct_t* structdata = state.try_find_struct(ty.name());
			diag::assert_that(structdata != nullptr, std::format("internal compiler error: could not find struct named \"{}\" within the semantic analysis state.", ty.name()));
			diag::assert_that(structdata->userdata != nullptr, std::format("internal compiler error: found struct named \"{}\" but its userdata ptr is null, meaning it has not been codegen'd and i currently need it while evaluating something else.", ty.name()));
			// see codegen_structs to confirm that indeed it is a llvm::StructType*
			return static_cast<llvm::StructType*>(structdata->userdata);
		}
		// todo: implement
		diag::fatal_error(std::format("internal compiler error: could not convert type \"{}\" to its corresponding llvm::Type*", ty.name()));
		return nullptr;
	}

	llvm::Type* as_llvm_type(const util::box<type>& ty, const semantic::state& state)
	{
		return as_llvm_type(*ty, state);
	}

	/////////////////////////////////////// TOP-LEVEL CODEGEN ///////////////////////////////////////

	void codegen_structs(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, structdata] : state.struct_decls)
		{
			diag::assert_that(structdata.userdata == nullptr, std::format("internal compiler error: while running codegen for struct \"{}\", userdata ptr was not-null, implying it has already been codegen'd", name));
			std::vector<llvm::Type*> llvm_data_members;
			for(const struct_type::data_member& member : structdata.ty.data_members)
			{
				llvm_data_members.push_back(as_llvm_type(member.type, state));
			}

			llvm::StructType* llvm_ty = llvm::StructType::create(llvm_data_members, name, false);
			// write the struct type ptr into the struct data so it can easily be retrieved later.
			structdata.userdata = llvm_ty;
		}
	}

	void codegen_functions(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, funcdata] : state.functions)
		{
			diag::assert_that(funcdata.userdata == nullptr, std::format("internal compiler error: while running codegen for function \"{}\", userdata ptr was not-null, implying it has already been codegen'd", name));
			llvm::Type* llvm_return = as_llvm_type(funcdata.return_ty, state);
			std::vector<llvm::Type*> llvm_params;
			for(const semantic::local_variable_t& param : funcdata.params)
			{
				llvm_params.push_back(as_llvm_type(param.ty, state));
			}

			llvm::FunctionType* llvm_fty = llvm::FunctionType::get(llvm_return, llvm_params, false);
			llvm::Function* llvm_fn = llvm::Function::Create(llvm_fty, llvm::Function::ExternalLinkage, name);
			std::size_t arg_counter = 0;
			for(llvm::Argument& arg : llvm_fn->args())
			{
				arg.setName(funcdata.params[arg_counter++].name);
			}

			// add the body if it exists.
			const ast::node& node = tree.get(funcdata.context);
			diag::assert_that(std::holds_alternative<ast::function_definition>(node.payload), std::format("internal compiler error: AST node corresponding to function definition \"{}\" (line {}) was not infact a function definition (variant id {}). something has gone horrendously wrong.", funcdata.name, node.meta.line_number, node.payload.index()));
			const auto& decl = std::get<ast::function_definition>(node.payload);
			if(!decl.is_extern)
			{
				// create a new basic block.	
				llvm::BasicBlock* blk = llvm::BasicBlock::Create(*ctx, "entry", llvm_fn);
				builder->SetInsertPoint(blk);

				bool has_return = false;
				for(const ast::node& child : node.children)
				{
					if(std::holds_alternative<ast::return_statement>(child.payload))
					{
						has_return = true;
					}
					// todo: codegen these nodes.
				}

				if(!has_return && funcdata.return_ty.is_void())
				{
					// automatically add a return :)
					ast::return_statement implicit_return
					{
						.value = std::nullopt
					};
					diag::fatal_error("NYI: implicit return. everything is ready to go though, just hook it up please.");
				}
			}

			funcdata.userdata = llvm_fn;
			llvm::verifyFunction(*llvm_fn);
		}
	}

	void codegen_global_variables(const ast& tree, const semantic::state& state)
	{
		for(const auto& [name, gvardata] : state.global_variables)
		{
			llvm::Type* llvm_ty = as_llvm_type(gvardata.ty, state);
			// note: globals may have an initialiser, which means we will need to codegen that even though we're so early on.
			const ast::node& node = tree.get(gvardata.context);
			diag::assert_that(std::holds_alternative<ast::variable_declaration>(node.payload), std::format("internal compiler error: AST node corresponding to global variable \"{}\" (line {}) was not infact a variable declaration (variant id {}). something has gone horrendously wrong.", gvardata.name, node.meta.line_number, node.payload.index()));
			const auto& decl = std::get<ast::variable_declaration>(node.payload);

			llvm::Constant* llvm_initialiser = nullptr;
			if(decl.initialiser.has_value())
			{
				// todo: assign llvm_initialiser to the codegen'd expression.
			}

			// create our owning global variable.
			std::unique_ptr<llvm::GlobalVariable> llvm_gvar = std::make_unique<llvm::GlobalVariable>(*program, llvm_ty, false, llvm::GlobalValue::ExternalLinkage, llvm_initialiser);
			// keep ahold of the underlying ptr.
			gvardata.userdata = llvm_gvar.get();
			// move the owning ptr into our global storage.
			global_variable_storage.push_back(std::move(llvm_gvar));
		}
	}

	/////////////////////////////////////// NODE CODEGEN ///////////////////////////////////////

	struct data
	{
		const ast& tree;
		const ast::node& node;
		const ast::path_t& path;
		const semantic::state& state;

		std::size_t line() const
		{
			return this->tree.get(path).meta.line_number;
		}

		void fatal_error(std::string msg) const
		{
			diag::fatal_error(std::format("(codegen) line {} - {}", this->line(), msg));
		}

		void warning(std::string msg) const
		{
			diag::warning(std::format("(codegen) line {} - {}", this->line(), msg));
		}

		void assert_that(bool expr, std::string msg) const
		{
			if(!expr)
			{
				this->fatal_error(msg);	
			}
		}
	};

	template<typename P>
	llvm::Value* codegen_thing(const data& d, const P& payload);

	llvm::Value* integer_literal(const data& d, ast::integer_literal payload)
	{
		// decimal literals are always i64
		return llvm::ConstantInt::get(*ctx, llvm::APInt{64, static_cast<std::uint64_t>(payload.val), true});
	}

	llvm::Value* decimal_literal(const data& d, ast::decimal_literal payload)
	{
		// decimal literals are always f64 (double)
		return llvm::ConstantFP::get(builder->getDoubleTy(), llvm::APFloat{payload.val});
	}

	llvm::Value* char_literal(const data& d, ast::char_literal payload)
	{
		return llvm::ConstantInt::get(*ctx, llvm::APInt{8, static_cast<std::uint64_t>(payload.val)});
	}

	llvm::Value* string_literal(const data& d, ast::string_literal payload)
	{
		return llvm::ConstantDataArray::getString(*ctx, payload.val, true);
	}

	llvm::Value* bool_literal(const data& d, ast::bool_literal payload)
	{
		return llvm::ConstantInt::get(*ctx, llvm::APInt{1, payload.val ? 1u : 0u, true});
	}

	llvm::Value* unary_expression(const data& d, unary_expression_t payload)
	{
		return nullptr;
	}

	llvm::Value* binary_expression(const data& d, binary_expression_t payload)
	{
		return nullptr;
	}

	llvm::Value* identifier(const data& d, ast::identifier payload)
	{
		return nullptr;
	}

	llvm::Value* function_call(const data& d, ast::function_call payload)
	{
		return nullptr;
	}

	llvm::Value* member_access(const data& d, ast::member_access payload)
	{
		return nullptr;
	}

	llvm::Value* expression(const data& d, ast::expression payload)
	{
		return nullptr;
	}

	llvm::Value* if_statement(const data& d, ast::if_statement payload)
	{
		return nullptr;
	}

	llvm::Value* for_statement(const data& d, ast::for_statement payload)
	{
		return nullptr;
	}

	llvm::Value* return_statement(const data& d, ast::return_statement payload)
	{
		return nullptr;
	}

	llvm::Value* variable_declaration(const data& d, ast::variable_declaration payload)
	{
		if(d.path.size() <= 1)
		{
			// its a global variable.
			// globals are already done in the pre-pass.
			return static_cast<llvm::GlobalVariable*>(d.state.try_find_global_variable(payload.var_name)->userdata);
		}
		return nullptr;
	}

	llvm::Value* function_definition(const data& d, ast::function_definition payload)
	{
		// functions are already done in the pre-pass.
		return nullptr;
	}

	llvm::Value* struct_definition(const data& d, ast::struct_definition payload)
	{
		// functions are already done in the pre-pass.
		return nullptr;
	}

	llvm::Value* meta_region(const data& d, ast::meta_region payload)
	{
		// functions are already done in the pre-pass.
		return nullptr;
	}

	template<typename ... Ts> 
	struct overload : Ts ... { 
		using Ts::operator() ...;
	};
	template<class... Ts> overload(Ts...) -> overload<Ts...>;

	template<typename P>
	llvm::Value* codegen_thing(const data& d, const P& payload)
	{
		llvm::Value* ret = nullptr;
		auto dispatch = overload
		{
			[&](ast::integer_literal lit)
			{
				ret = integer_literal(d, lit);
			},
			[&](ast::decimal_literal lit)
			{
				ret = decimal_literal(d, lit);
			},
			[&](ast::char_literal lit)
			{
				ret = char_literal(d, lit);
			},
			[&](ast::string_literal lit)
			{
				ret = string_literal(d, lit);
			},
			[&](ast::bool_literal lit)
			{
				ret = bool_literal(d, lit);
			},
			[&](ast::binary_operator op)
			{
				d.fatal_error("dispatch error");
			},
			[&](ast::unary_operator op)
			{
				d.fatal_error("dispatch error");
			},
			[&](unary_expression_t uexpr)
			{
				ret = unary_expression(d, uexpr);
			},
			[&](binary_expression_t bexpr)
			{
				ret = binary_expression(d, bexpr);
			},
			[&](ast::identifier id)
			{
				ret = identifier(d, id);
			},
			[&](ast::function_call call)
			{
				ret = function_call(d, call);
			},
			[&](ast::member_access mem)
			{
				ret = member_access(d, mem);
			},
			[&](ast::expression expr)
			{
				ret = expression(d, expr);
			},
			[&](ast::if_statement ifst)
			{
				ret = if_statement(d, ifst);
			},
			[&](ast::for_statement forst)
			{
				ret = for_statement(d, forst);
			},
			[&](ast::return_statement returnst)
			{
				ret = return_statement(d, returnst);
			},
			[&](ast::variable_declaration decl)
			{
				ret = variable_declaration(d, decl);
			},
			[&](ast::function_definition fdef)
			{
				ret = function_definition(d, fdef);
			},
			[&](ast::struct_definition sdef)
			{
				ret = struct_definition(d, sdef);
			},
			[&](ast::meta_region meta)
			{
				ret = meta_region(d, meta);
			},
			[&](auto)
			{
				d.fatal_error("dispatch error (no codegen support for this node payload)");
			}

		};
		std::visit(dispatch, payload);
		return ret;
	}

	void codegen_single_node(const ast& tree, const semantic::state& state, ast::path_t path)
	{
		const ast::node& node = tree.get(path);
		codegen_thing({.tree = tree, .node = node, .path = path, .state = state}, node.payload);
	}
}