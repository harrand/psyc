#ifndef PSYC_TYPE_HPP
#define PSYC_TYPE_HPP
#include "util.hpp"
#include <vector>
#include <array>
#include <variant>
#include <string>

enum class primitive_type
{
	i64,
	i32,
	i16,
	i8,

	u64,
	u32,
	u16,
	u8,
	u0,

	boolean,

	f64,
	f32,
	f16,
	_count
};

constexpr std::array<const char*, static_cast<int>(primitive_type::_count)> primitive_type_names
{
	"i64",
	"i32",
	"i16",
	"i8",

	"u64",
	"u32",
	"u16",
	"u8",
	"u0",

	"bool",

	"f64",
	"f32",
	"f16"
};

struct type;
struct struct_type
{
	struct data_member
	{
		std::string member_name;
		util::box<type> type;
	};

	std::string name;
	std::vector<data_member> data_members = {};
};

constexpr std::size_t array_size_dyn_array = std::numeric_limits<std::size_t>::max();

struct type
{
	std::variant
	<
		primitive_type,
		struct_type,
		std::monostate
	> ty = std::monostate{};
	std::size_t pointer_level = 0;

	bool operator==(const type& rhs) const = default;

	bool is_undefined() const; // an ill-defined type.

	bool is_primitive() const;
	bool is_void() const;
	bool is_pointer() const;
	bool is_struct() const;
	const struct_type& as_struct() const;

	std::string name() const;

	type dereference() const;
	type reference() const;

	static type undefined();
	static type from_primitive(primitive_type t);
	static type from_struct(struct_type t);
};

#endif // PSYC_TYPE_HPP