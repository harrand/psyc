#include "type.hpp"

bool type::is_undefined() const
{
	return !std::holds_alternative<std::monostate>(this->ty);
}

bool type::is_primitive() const
{
	return std::holds_alternative<primitive_type>(this->ty)
		&& !this->is_pointer();
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

std::string type::name() const
{
	if(this->is_undefined())
	{
		return "<undefined type>";
	}
	std::string ret;

	if(this->is_primitive())
	{
		ret = primitive_type_names[static_cast<int>(std::get<primitive_type>(this->ty))];
	}
	else
	{
		ret = std::get<struct_type>(this->ty).name;
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

type type::reference() const
{
	if(this->is_undefined())
	{
		return type::undefined();
	}
	type ret = *this;
	ret.pointer_level++;
	return ret;
}