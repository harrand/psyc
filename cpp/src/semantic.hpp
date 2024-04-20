#ifndef PSYC_SEMANTIC_HPP
#define PSYC_SEMANTIC_HPP
#include "ast.hpp"
#include "type.hpp"
#include <map>
#include <unordered_map>

namespace std
{
	template<>
	struct hash<ast::path_t>
	{
		std::size_t operator()(const ast::path_t& val) const
		{
			std::size_t seed = val.size();
			for(auto x : val)
			{
				x = ((x >> 16) ^ x) * 0x45d9f3b;
				x = ((x >> 16) ^ x) * 0x45d9f3b;
				x = (x >> 16) ^ x;
				seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
			}
			return seed;
		}
	};
}

namespace semantic
{
	// semantic analysis. get type information and do checks etc...
	struct local_variable_t
	{
		type ty;
		std::string name;
		ast::path_t context;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
	};

	struct function_t
	{
		type return_ty;
		std::string name;
		std::vector<local_variable_t> params = {};
		ast::path_t context;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
	};

	struct struct_t
	{
		struct_type ty;
		ast::path_t context;
		// codegen may want to track extra data. however, semal doesnt care about any of this.
		mutable void* userdata = nullptr;
	};

	struct scope_reference
	{
		std::unordered_map<std::string, local_variable_t> variables = {};
		std::unordered_map<std::size_t, scope_reference> children = {};
	};
	struct state
	{
		//std::unordered_map<ast::path_t, std::map<std::string, local_variable_t>> variables = {};	
		scope_reference variables = {};
		std::map<std::string, function_t> functions = {};
		std::map<std::string, local_variable_t> global_variables = {};
		std::map<std::string, struct_t> struct_decls = {};
		std::unordered_map<ast::path_t, type> type_breadcrumbs = {};
		std::string last_error = "";
		std::string last_warning = "";

		void pre_pass(const ast& tree);
		void process(const ast& tree);
		void process_node(ast::path_t path, const ast& tree);
		void process_single_node(ast::path_t path, const ast& tree);

		std::pair<type, ast::path_t> get_type_from_name(std::string_view type_name) const;
		// returns nullptr if we didnt gather any type information
		// if not nullptr, type could still be an undefined type (e.g if the node is an if-statement, coz an if statement doesnt really evaluate to a type)
		const type* try_get_type_from_node(const ast::path_t& path) const;

		const function_t* try_find_function(const std::string& function_name) const;
		const struct_t* try_find_struct(const std::string& struct_name) const;
		const local_variable_t* try_find_global_variable(const std::string& var_name) const;
		const local_variable_t* try_find_local_variable(const ast::path_t& context, const std::string& var_name) const;

		void register_struct(struct_t str);
		void register_function(function_t func);
		void register_global_variable(local_variable_t var);
		void register_local_variable(local_variable_t var);
	};

	state analysis(const ast& ast);
}


using unary_expression_t = std::pair<ast::unary_operator, util::box<ast::expression>>;
using binary_expression_t = std::tuple<ast::binary_operator, util::box<ast::expression>, util::box<ast::expression>>;

#endif // PSYC_SEMANTIC_HPP