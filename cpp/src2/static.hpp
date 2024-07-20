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
	static static_value typeinfo(const type_system& tsys, const itype& ty);

	bool is_null() const;

	template<typename T>
	T value_as() const
	{
		return std::any_cast<T>(this->val);
	}
	bool has_value() const;
	bool has_children() const;

	bool equals(const static_value& rhs) const;

	static_value clone() const;
	static_value do_convert(type_ptr to, srcloc ctx) const;
	static_value do_explicit_convert(type_ptr to, srcloc ctx) const;

	static_value with_type_qualifier(type_qualifier q) const;
	static_value discarded_type_qualifiers() const;

	// assign to a new value (if newval has no value, then it is considered a runtime value meaning we will no longer have a compile-time value either)
	void set_value(const static_value& newval, srcloc ctx);
	// clear the value, meaning it is no longer a value known at compile-time
	void clear_value();
};

std::int64_t get_int_value(const itype& ty, const std::any& int_of_some_size);
std::any to_int_value(const itype& ty, std::int64_t ival);

#endif // PSYC_STATIC_HPP