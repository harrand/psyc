#include "builtin.hpp"
#include <array>

std::array<const char*, (int)builtin::_count> builtin_names
{
	"malloc",
	"free"	
};

std::array<semal::function_t, (int)builtin::_count> builtin_functions
{
	semal::function_t
	{
		.return_ty = type::from_primitive(primitive_type::i8).pointer_to(qualifier_weak),
		.name = "malloc",
		.params =
		{
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::u64),
				.name = "size_bytes",
				.ctx = {},
			}
		},
		.ctx = {},
		.is_method = false
	},
	semal::function_t
	{
		.return_ty = type::from_primitive(primitive_type::u0),
		.name = "free",
		.params =
		{
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::i8).pointer_to(qualifier_weak),
				.name = "ptr",
				.ctx = {},
			}
		},
		.ctx = {},
		.is_method = false
	},
};

builtin try_find_builtin(std::string_view funcname)
{
	for(int i = 0; i < (int)builtin::_count; i++)
	{
		if(std::format("__builtin_{}", builtin_names[i]) == funcname)
		{
			return (builtin)i;
		}
	}
	return builtin::_undefined;
}

semal::function_t& get_builtin_function(builtin b)
{
	return builtin_functions[(int)b];
}