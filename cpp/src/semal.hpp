#ifndef PSYC_SEMAL_HPP
#define PSYC_SEMAL_HPP
#include "type.hpp"
#include "ast.hpp"
#include <map>
#include <unordered_map>

namespace semal
{
	struct context
	{
		const ast* tree;
		ast::path_t path;
		const srcloc& location() const;
	};
	struct local_variable_t
	{
		type ty;
		std::string name;
		context ctx;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
	};

	struct function_t
	{
		type return_ty;
		std::string name;
		std::vector<local_variable_t> params = {};
		context ctx;
		bool is_method = false;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
	};

	struct struct_t
	{
		struct_type ty;
		context ctx;
		std::unordered_map<std::string, function_t> methods = {};
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
	};

	struct scope_reference
	{
		std::unordered_map<std::string, local_variable_t> variables = {};
		std::unordered_map<std::size_t, scope_reference> children = {};
	};

	struct output
	{
		scope_reference variables = {};
		std::map<std::string, function_t> functions = {};
		std::map<std::string, local_variable_t> global_variables = {};
		std::map<std::string, struct_t> struct_decls = {};	

		void combine(const output& rhs);
	};

	output analyse_predecl(const ast& tree);
	output analyse_full(const ast& tree, output predecl = {});

	struct state
	{
		output program_decls = {};
		std::unordered_map<std::filesystem::path, output> analysed_input_files = {};
	};
}

#endif // PSYC_SEMAL_HPP