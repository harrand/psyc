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
	};

	struct function_t
	{
		type return_ty;
		std::string name;
		std::vector<local_variable_t> params = {};
		ast::path_t context;
	};

	struct struct_t
	{
		struct_type ty;
		ast::path_t context;
	};
	struct state
	{
		std::unordered_map<ast::path_t, std::map<std::string, local_variable_t>> variables = {};	
		std::map<std::string, function_t> functions = {};
		std::map<std::string, local_variable_t> global_variables = {};
		std::map<std::string, struct_t> struct_decls = {};
		std::string last_error = "";
		std::string last_warning = "";

		std::pair<type, ast::path_t> get_type_from_name(std::string_view type_name) const;

		void pre_pass(const ast& tree);
		void process(const ast& tree);
		void process_node(ast::path_t path, const ast& tree);
		void process_single_node(ast::path_t path, const ast& tree);
		void register_struct(struct_t str);
		void register_function(function_t func);
		void register_global_variable(local_variable_t var);
		void register_local_variable(local_variable_t var);
	};

	state analysis(const ast& ast);
}

#endif // PSYC_SEMANTIC_HPP