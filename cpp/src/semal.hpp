#ifndef PSYC_SEMAL_HPP
#define PSYC_SEMAL_HPP
#include "type.hpp"
#include "ast.hpp"
#include "diag.hpp"
#include <map>
#include <unordered_map>

namespace semal
{
	struct context
	{
		const ast* tree;
		ast::path_t path;
		const ast::node& node() const;
		const srcloc& location() const;

		template<typename... Ts>
		void warning(std::string fmt, Ts&&... ts) const
		{
			diag::warning("at {}: {}", this->location().to_string(), std::vformat(fmt, std::make_format_args(ts...)));
		}
		template<typename... Ts>
		void error(error_code err, std::string fmt, Ts&&... ts) const
		{
			diag::error(err, "at {}: {}", this->location().to_string(), std::vformat(fmt, std::make_format_args(ts...)));
		}
		template<typename... Ts>
		void assert_that(bool expr, error_code err, std::string fmt, Ts&&... ts) const
		{
			if(!expr)
			{
				diag::assert_that(expr, err, "at {}: {}", this->location().to_string(), std::vformat(fmt, std::make_format_args(ts...)));
			}
		}
	};
	struct local_variable_t
	{
		type ty;
		std::string name;
		context ctx;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
		mutable void* debuginfo = nullptr;
	};

	struct function_t
	{
		type return_ty;
		std::string name;
		std::vector<local_variable_t> params = {};
		context ctx;
		bool is_method = false;
		bool is_builtin = false;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
		mutable void* debuginfo = nullptr;
	};

	struct struct_t
	{
		struct_type ty;
		context ctx;
		std::unordered_map<std::string, function_t> methods = {};
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
		mutable void* debuginfo = nullptr;
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
		type get_type_from_name(std::string type_name) const;
		void register_function(function_t fn);
		void register_global_variable(local_variable_t gvar);
		void register_local_variable(local_variable_t gvar);
		void register_struct(struct_t structdata);

		const function_t* try_find_function(std::string_view name) const;
		const function_t* try_find_parent_function(const ast& tree, ast::path_t path) const;
		const local_variable_t* try_find_global_variable(std::string_view) const;
		const local_variable_t* try_find_local_variable(const ast::path_t& context, std::string_view name) const;
		const struct_t* try_find_struct(std::string_view name) const;

		type get_type_from_payload(const ast::node::payload_t& payload, const ast& tree, const ast::path_t& path) const;
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