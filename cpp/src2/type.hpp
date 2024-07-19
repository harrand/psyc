#ifndef PSYC_TYPE_HPP
#define PSYC_TYPE_HPP
#include "util.hpp"
#include <string>
#include <array>
#include <unordered_map>

struct itype;
using type_ptr = std::unique_ptr<itype>;

struct pointer_type;

enum type_qualifier
{
	qual_none = 0x0,
	qual_const = 0x1,	
	qual_weak = 0x2,
	qual_static = 0x4,
};

enum class typeconv
{
	noop,
	i2i,
	f2f,
	i2f,
	f2i,
	p2p,
	i2p,
	p2i,
	fn2fn,
	p2fn,
	fn2p,
	cant	
};

struct itype : public util::unique_cloneable<itype>
{
	enum class hint
	{
		primitive_type,
		struct_type,
		pointer_type,
		alias_type,
		function_type,
		ill_formed,
		_count
	};

	itype();
	itype(std::string name, hint h);
	itype(const itype& cpy) = default;

	virtual std::string get_name() const;
	std::string get_qualified_name() const;
	const char* hint_name() const;
	bool is_pointer() const;
	bool is_struct() const;
	bool is_function() const;
	bool is_alias() const;
	bool is_primitive() const;
	bool is_integer() const;
	bool is_floating_point() const;
	bool is_signed() const;
	bool is_unsigned() const;
	bool is_well_formed() const;
	short numeric_bit_count() const;

	bool is_weak() const;
	bool is_const() const;
	bool is_static() const;

	void add_qualifier(type_qualifier q);
	type_ptr with_qualifier(type_qualifier q) const; 
	void remove_qualifier(type_qualifier q);
	type_ptr without_qualifier(type_qualifier q) const;

	type_ptr discarded_qualifiers() const;

	virtual type_ptr deref() const;
	virtual type_ptr ref() const;

	typeconv can_implicitly_convert_to(const itype& rhs) const;
	typeconv can_explicitly_convert_to(const itype& rhs) const;

	bool operator==(const itype& rhs) const{return this->get_name() == rhs.get_name();}

	std::string name;
	hint h;
	type_qualifier quals = qual_none;
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
	primitive_type(primitive prim);
	COPY_UNIQUE_CLONEABLE(itype)

	primitive prim;
};

struct function_type : public itype
{
	function_type(type_ptr return_type, std::vector<type_ptr> params);
	function_type(const function_type& cpy);
	COPY_UNIQUE_CLONEABLE(itype)

	type_ptr return_type;
	std::vector<type_ptr> params;

	virtual std::string get_name() const final;
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

struct incomplete_type : public itype
{
	incomplete_type(std::string name);
	incomplete_type(const incomplete_type& cpy);

	COPY_UNIQUE_CLONEABLE(itype)
};

class type_system
{
public:
	type_system();
	struct struct_builder
	{
		type_system& sys;
		std::string struct_name;
		std::vector<struct_type::data_member> members = {};

		struct_builder& add_member(std::string name, std::string type_name);
		type_ptr build();
	};

	struct_builder make_struct(std::string name);
	type_ptr make_alias(std::string name, std::string typename_to_alias);
	type_ptr get_type(std::string type_name) const;
	type_ptr get_primitive_type(primitive prim) const;
	type_ptr get_function_type(std::string return_type_name, std::vector<std::string> param_type_names) const;
	std::string suggest_valid_typename_for_typo(std::string invalid_typename) const;
private:
	void add_compiler_supported_types();
	std::unordered_map<std::string, type_ptr> types = {};
};

namespace type_helpers
{
	bool is_primitive(std::string_view type_name);
}

#endif // PSYC_TYPE_HPP