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
	static static_value create(type_ptr ty, std::any val);

	bool is_null() const;

	template<typename T>
	T value_as() const
	{
		return std::any_cast<T>(this->val);
	}
	bool has_value() const;

	static_value clone() const;
	static_value do_convert(type_ptr to, srcloc ctx) const;
	static_value do_explicit_convert(type_ptr to, srcloc ctx) const;

	// assign to a new value (if newval has no value, then it is considered a runtime value meaning we will no longer have a compile-time value either)
	void set_value(const static_value& newval, srcloc ctx);
	// clear the value, meaning it is no longer a value known at compile-time
	void clear_value();
};

#endif // PSYC_STATIC_HPP