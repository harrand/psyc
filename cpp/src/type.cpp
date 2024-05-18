#include "type.hpp"
#include "diag.hpp"

bool type::is_undefined() const
{
	return std::holds_alternative<std::monostate>(this->ty);
}

bool type::is_primitive() const
{
	return std::holds_alternative<primitive_type>(this->ty);
}

bool type::is_pointer() const
{
	return std::holds_alternative<util::box<type>>(this->ty);
}

bool type::is_struct() const
{
	return std::holds_alternative<struct_type>(this->ty);
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
			diag::error(error_code::type, "type system error. no support for checking signedness of the provided integer type.");
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
			diag::error(error_code::type, "type system error. no support for checking signedness of the provided integer type.");
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

bool type::is_const() const
{
	return this->qualifiers & qualifier_const;
}

bool type::is_weak() const
{
	return this->qualifiers & qualifier_weak;
}

conversion_type type::is_implicitly_convertible_to(const type& rhs) const
{
	if(*this == rhs)
	{
		return conversion_type::none;
	}
	// const T has the exact same rules as T
	if(this->is_const())
	{
		auto const_this = *this;
		const_this.qualifiers = type_qualifier(int(const_this.qualifiers) | qualifier_const);
		return const_this.is_implicitly_convertible_to(rhs);
	}
	if(this->is_weak() || rhs.is_weak())
	{
		// weak types are implicitly convertible to a bunch of things.
		auto plain_this = this->without_qualifiers();
		if(plain_this == type::from_primitive(primitive_type::i64) && rhs.is_pointer())
		{
			// i64 -> any pointer (i.e uintptr_t)
			return conversion_type::i2p;
		}
		if(this->is_integer_type() && rhs.is_integer_type())
		{
			// integer promotion.
			return conversion_type::i2i;
		}
		if(this->is_floating_point_type() && rhs.is_floating_point_type())
		{
			// float promotion???
			return conversion_type::f2f;
		}
		if(this->is_integer_type() && rhs.is_floating_point_type())
		{
			// integer -> floating point conversion.
			return conversion_type::i2f;
		}
		if(this->is_pointer() && rhs.is_pointer())
		{
			// pointer -> pointer conversion.
			return conversion_type::p2p;
		}
	}
	return conversion_type::impossible;
}

conversion_type type::is_explicitly_convertible_to(const type& rhs) const
{
	// you can explicitly convert (cast) to something if the weak variant of the current type is implicitly convertible.
	auto weak_this = *this;
	weak_this.qualifiers = type_qualifier(int(weak_this.qualifiers) | qualifier_weak);
	return weak_this.is_implicitly_convertible_to(rhs);
}

primitive_type type::as_primitive() const
{
	diag::assert_that(this->is_primitive(), error_code::ice, "attempt to resolve non-primitive type \"{}\" as a primitive", this->name());
	return std::get<primitive_type>(this->ty);
}

struct_type type::as_struct() const
{
	diag::assert_that(this->is_struct(), error_code::ice, "attempt to resolve non-struct type \"{}\" as a struct", this->name());
	return std::get<struct_type>(this->ty);
}

std::string type::name() const
{
	if(this->is_undefined())
	{
		return "<undefined type>";
	}
	std::string ret;
	std::string qualstr = "";
	if(this->qualifiers & qualifier_const)
	{
		qualstr += " const";
	}
	if(this->qualifiers & qualifier_weak)
	{
		qualstr += " weak";
	}

	if(this->is_primitive())
	{
		ret = std::format("{}{}", primitive_type_names[static_cast<int>(std::get<primitive_type>(this->ty))], qualstr);
	}
	else if(this->is_struct())
	{
		ret = std::format("{}{}", std::get<struct_type>(this->ty).name, qualstr);
	}
	else
	{
		ret = std::format("{}&{}", std::get<util::box<type>>(this->ty)->name(), qualstr);
	}

	return ret;
}

type type::dereference() const
{
	diag::assert_that(this->is_pointer(), error_code::ice, "attempt to dereference type \"{}\" when it is not a pointer type.", this->name());
	return *std::get<util::box<type>>(this->ty);
}

type type::pointer_to(type_qualifier quals) const
{
	diag::assert_that(!this->is_undefined(), error_code::type, "attempt to get pointer to <undefined> type.");
	return
	{
		.ty = util::box<type>{*this},
		.qualifiers = quals,
	};
}

type type::without_qualifiers() const
{
	type ret = *this;
	ret.qualifiers = qualifier_none;
	return ret;
}

/*static*/type type::undefined()
{
	return
	{
		.ty = std::monostate{}
	};
}


/*static*/type type::from_primitive(primitive_type t, type_qualifier quals)
{
	return
	{
		.ty = t,
		.qualifiers = quals
	};
}

/*static*/type type::from_struct(struct_type t, type_qualifier quals)
{
	return
	{
		.ty = t,
		.qualifiers = quals
	};
}