#ifndef PSYC_TYPE_HPP
#define PSYC_TYPE_HPP
#include "util.hpp"
#include <array>
#include <variant>
#include <vector>

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

		bool operator==(const data_member& rhs) const = default;
	};

	std::string name;
	std::vector<data_member> data_members = {};
	bool operator==(const struct_type& rhs) const = default;
};

enum type_qualifier
{
	qualifier_none = 0,
	qualifier_const = 0b00000001,
};

constexpr std::size_t array_size_dyn_array = std::numeric_limits<std::size_t>::max();

struct type
{
	std::variant
	<
	std::monostate,
	primitive_type, // primitive
	struct_type, // struct
	util::box<type> // pointer
	> ty = std::monostate{};
	type_qualifier qualifiers = qualifier_none;

	bool operator==(const type& rhs) const = default;

	bool is_undefined() const;
	bool is_primitive() const;
	bool is_pointer() const;
	bool is_struct() const;
	bool is_integer_type() const;
	bool is_signed_integer_type() const;
	bool is_unsigned_integer_type() const;
	bool is_floating_point_type() const;
	bool is_void() const;

	primitive_type as_primitive() const;
	struct_type as_struct() const;

	std::string name() const;

	type dereference() const;
	type pointer_to() const;

	static type undefined();
	static type from_primitive(primitive_type t);
	static type from_struct(struct_type t);
};
#endif // PSYC_TYPE_HPP