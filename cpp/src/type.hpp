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
		util::box<type> ty;

		bool operator==(const data_member& rhs) const = default;
	};

	std::string name;
	std::vector<data_member> data_members = {};
	bool operator==(const struct_type& rhs) const = default;
	std::size_t size_bytes() const;
};

enum type_qualifier
{
	qualifier_none = 0,
	qualifier_const = 0b00000001,
	qualifier_weak = 0b00000010,
};

constexpr std::size_t array_size_dyn_array = std::numeric_limits<std::size_t>::max();

enum class conversion_type
{
	none,
	i2i,
	f2f,
	i2f,
	p2p,
	i2p,
	impossible,
};

constexpr inline bool typecon_valid(const conversion_type& conv)
{
	return conv != conversion_type::impossible;
}

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

	bool is_const() const;
	bool is_weak() const;

	conversion_type is_implicitly_convertible_to(const type& rhs) const;
	conversion_type is_explicitly_convertible_to(const type& rhs) const;

	primitive_type as_primitive() const;
	struct_type as_struct() const;

	std::string name() const;
	std::size_t size_bytes() const;

	type dereference() const;
	type pointer_to(type_qualifier quals = qualifier_none) const;
	type without_qualifiers() const;

	static type undefined();
	static type from_primitive(primitive_type t, type_qualifier quals = qualifier_none);
	static type from_struct(struct_type t, type_qualifier quals = qualifier_none);
};
#endif // PSYC_TYPE_HPP