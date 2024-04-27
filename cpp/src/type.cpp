#include "type.hpp"
#include "diag.hpp"

bool type::is_undefined() const
{
	return std::holds_alternative<std::monostate>(this->ty);
}

bool type::is_primitive() const
{
	return std::holds_alternative<primitive_type>(this->ty)
		&& !this->is_pointer();
}

bool type::is_integer_type() const
{
	if(!this->is_primitive())
	{
		return false;
	}
	switch(std::get<primitive_type>(this->ty))
	{
		case primitive_type::i64:
			[[fallthrough]];
		case primitive_type::i32:
			[[fallthrough]];
		case primitive_type::i16:
			[[fallthrough]];
		case primitive_type::i8:
			[[fallthrough]];
		case primitive_type::u64:
			[[fallthrough]];
		case primitive_type::u32:
			[[fallthrough]];
		case primitive_type::u16:
			[[fallthrough]];
		case primitive_type::u8:
			return true;
		break;
		default:
			return false;
		break;
	}
}

bool type::is_signed_integer_type() const
{
	if(!this->is_integer_type())
	{
		return false;
	}
	switch(std::get<primitive_type>(this->ty))
	{
		case primitive_type::u64:
			[[fallthrough]];
		case primitive_type::u32:
			[[fallthrough]];
		case primitive_type::u16:
			[[fallthrough]];
		case primitive_type::u8:
			return false;
		break;
		case primitive_type::i64:
			[[fallthrough]];
		case primitive_type::i32:
			[[fallthrough]];
		case primitive_type::i16:
			[[fallthrough]];
		case primitive_type::i8:
			return true;
		break;
		default:
			diag::fatal_error("type system error. no support for checking signedness of the provided integer type.");
			return false;
		break;
	}
}

bool type::is_unsigned_integer_type() const
{
	if(!this->is_integer_type())
	{
		return false;
	}
	switch(std::get<primitive_type>(this->ty))
	{
		case primitive_type::u64:
			[[fallthrough]];
		case primitive_type::u32:
			[[fallthrough]];
		case primitive_type::u16:
			[[fallthrough]];
		case primitive_type::u8:
			return true;
		break;
		case primitive_type::i64:
			[[fallthrough]];
		case primitive_type::i32:
			[[fallthrough]];
		case primitive_type::i16:
			[[fallthrough]];
		case primitive_type::i8:
			return false;
		break;
		default:
			diag::fatal_error("type system error. no support for checking signedness of the provided integer type.");
			return false;
		break;
	}
}

bool type::is_floating_point_type() const
{
	if(!this->is_primitive())
	{
		return false;
	}
	switch(std::get<primitive_type>(this->ty))
	{
		case primitive_type::f64:
			[[fallthrough]];
		case primitive_type::f32:
			[[fallthrough]];
		case primitive_type::f16:
			return true;
		break;
		default:
			return false;
		break;
	}
}

bool type::is_void() const
{
	return this->is_primitive()
		&& std::get<primitive_type>(this->ty) == primitive_type::u0
		&& !this->is_pointer();
}

bool type::is_pointer() const
{
	return !this->is_undefined() && this->pointer_level > 0;
}

bool type::is_struct() const
{
	return std::holds_alternative<struct_type>(this->ty) && !this->is_pointer();
}

primitive_type type::as_primitive() const
{
	diag::assert_that(this->is_primitive(), std::format("internal compiler error: attempt to re-interpret non-primitive type \"{}\" as a primitive", this->name()));
	return std::get<primitive_type>(this->ty);
}

const struct_type& type::as_struct() const
{
	diag::assert_that(this->is_struct(), std::format("internal compiler error: attempt to re-interpret non-struct type \"{}\" as struct", this->name()));
	return std::get<struct_type>(this->ty);
}

std::string type::name() const
{
	if(this->is_undefined())
	{
		return "<undefined type>";
	}
	type thiscpy = *this;
	for(std::size_t i = 0; i < this->pointer_level; i++)
	{
		thiscpy = thiscpy.dereference();
	}

	std::string ret;

	if(thiscpy.is_primitive())
	{
		ret = primitive_type_names[static_cast<int>(std::get<primitive_type>(this->ty))];
	}
	else
	{
		ret = std::get<struct_type>(thiscpy.ty).name;
	}

	for(std::size_t i = 0; i < this->pointer_level; i++)
	{
		ret += "*";
	}	
	return ret;
}

type type::dereference() const
{
	if(this->is_pointer())
	{
		type ret = *this;
		ret.pointer_level--;
		return ret;
	}
	return type::undefined();
}

type type::pointer_to() const
{
	if(this->is_undefined())
	{
		return type::undefined();
	}
	type ret = *this;
	ret.pointer_level++;
	return ret;
}

type type::undefined()
{
	return type
	{
		.ty = std::monostate{},
		.pointer_level = 0
	};
}

type type::from_primitive(primitive_type t)
{
	return type
	{
		.ty = t,
		.pointer_level = 0
	};
}

type type::from_struct(struct_type t)
{
	return type
	{
		.ty = t,
		.pointer_level = 0
	};
}