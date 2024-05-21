#include "builtin.hpp"
#include <array>

std::array<semal::function_t, (int)builtin::_count> builtin_functions
{
	semal::function_t
	{
		.return_ty = type::from_primitive(primitive_type::i8).pointer_to(),
		.name = "malloc",
		.params =
		{
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::u64, qualifier_weak),
				.name = "size_bytes",
				.ctx = {},
			}
		},
		.ctx = {},
		.is_method = false,
		.is_builtin = true
	},
	semal::function_t
	{
		.return_ty = type::from_primitive(primitive_type::u0),
		.name = "free",
		.params =
		{
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::i8).pointer_to(),
				.name = "ptr",
				.ctx = {},
			}
		},
		.ctx = {},
		.is_method = false,
		.is_builtin = true
	},
	semal::function_t
	{
		.return_ty = type::from_primitive(primitive_type::u0),
		.name = "memcpy",
		.params =
		{
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::i8).pointer_to(qualifier_weak),
				.name = "dst",
				.ctx = {},
			},
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::i8).pointer_to(qualifier_weak),
				.name = "src",
				.ctx = {},
			},
			semal::local_variable_t
			{
				.ty = type::from_primitive(primitive_type::u64, qualifier_weak),
				.name = "bytes",
				.ctx = {},
			},
		},
		.ctx = {},
		.is_method = false,
		.is_builtin = true
	},
	semal::function_t
	{
		.return_ty = type::from_primitive(primitive_type::u0),
		.name = "debugbreak",
		.params =
		{
		},
		.ctx = {},
		.is_method = false,
		.is_builtin = true
	},
};

builtin try_find_builtin(std::string_view funcname)
{
	for(int i = 0; i < (int)builtin::_count; i++)
	{
		if(std::format("__builtin_{}", builtin_functions[i].name) == funcname)
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