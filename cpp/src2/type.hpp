#ifndef PSYC_TYPE_HPP
#define PSYC_TYPE_HPP
#include "util.hpp"
#include "llvm/Demangle/MicrosoftDemangleNodes.h"
#include <string>

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
	"u8",
	"u0",

	"bool",

	"f64",
	"f32",
	"f16"
};

struct primitive_type : public itype
{
	COPY_UNIQUE_CLONEABLE(itype)
	primitive_type(std::string primitive_typename);

	primitive prim;
};

namespace type_helpers
{
	bool is_primitive(std::string_view type_name);
}

#endif // PSYC_TYPE_HPP