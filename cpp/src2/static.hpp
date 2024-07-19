#ifndef PSYC_STATIC_HPP
#define PSYC_STATIC_HPP
#include "type.hpp"
#include "srcloc.hpp"
#include <any>

struct static_value
{
	type_ptr ty = nullptr;
	std::any val = {};
	std::unordered_map<std::string, static_value> children = {};

	static static_value null();
	static static_value type_only(type_ptr ty);

	template<typename T>
	T value_as() const
	{
		return std::any_cast<T>(this->val);
	}
	bool has_value() const;

	static_value clone() const;
	static_value do_convert(type_ptr to, srcloc ctx);
	static_value do_explicit_convert(type_ptr to, srcloc ctx);
};

#endif // PSYC_STATIC_HPP