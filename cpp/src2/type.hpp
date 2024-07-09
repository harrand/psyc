#ifndef PSYC_TYPE_HPP
#define PSYC_TYPE_HPP
#include "util.hpp"
#include <string>
#include <array>
#include <unordered_map>

struct itype;
using type_ptr = std::unique_ptr<itype>;

struct pointer_type;

struct itype : public util::unique_cloneable<itype>
{
	enum class hint
	{
		primitive_type,
		struct_type,
		pointer_type,
		alias_type,
		ill_formed,
		_count
	};

	itype();
	itype(std::string name, hint h);
	itype(const itype& cpy) = default;

	virtual std::string get_name() const;
	const char* hint_name() const;
	bool is_pointer() const;
	bool is_struct() const;
	bool is_primitive() const;
	bool is_well_formed() const;

	virtual type_ptr deref() const;
	virtual type_ptr ref() const;

	std::string name;
	hint h;
};

struct pointer_type : public itype
{
	pointer_type(type_ptr base_type);
	pointer_type(const pointer_type& cpy);
	COPY_UNIQUE_CLONEABLE(itype)

	virtual std::string get_name() const final;
	virtual type_ptr deref() const final;

	type_ptr base;
};

enum class primitive
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
constexpr std::array<const char*, static_cast<int>(primitive::_count)> primitive_names =
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

struct primitive_type : public itype
{
	primitive_type(std::string primitive_typename);
	COPY_UNIQUE_CLONEABLE(itype)

	primitive prim;
};

struct alias_type : public itype
{
	alias_type(type_ptr alias, std::string alias_name);
	alias_type(const alias_type& cpy);
	COPY_UNIQUE_CLONEABLE(itype)
	type_ptr original() const;

	type_ptr alias;
};

struct struct_type : public itype
{
	struct data_member
	{
		std::string name;
		type_ptr ty;
	};

	struct_type(std::string name, std::vector<data_member> members = {});
	struct_type(const struct_type& cpy);
	COPY_UNIQUE_CLONEABLE(itype)

	std::vector<data_member> members;
};

class type_system
{
public:
	type_system() = default;
	struct struct_builder
	{
		type_system& sys;
		std::string struct_name;
		std::vector<struct_type::data_member> members = {};

		struct_builder& add_member(std::string name, std::string type_name);
		void build();
	};

	struct_builder make_struct(std::string name);
	void make_alias(std::string name, std::string typename_to_alias);
	type_ptr get_type(std::string type_name) const;
private:
	std::string suggest_valid_typename_for_typo(std::string invalid_typename) const;
	std::unordered_map<std::string, type_ptr> types = {};
};

namespace type_helpers
{
	bool is_primitive(std::string_view type_name);
}

#endif // PSYC_TYPE_HPP