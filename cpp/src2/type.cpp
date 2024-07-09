#include "type.hpp"
#include "diag.hpp"

constexpr static std::array<const char*, static_cast<int>(itype::hint::_count)> hint_names
{
	"primitive",
	"struct",
	"pointer",
	"ill-formed"
};

itype::itype(): name("<undefined type>"), h(itype::hint::ill_formed){}
itype::itype(std::string name, hint h): name(name), h(h){}

/*virtual*/ std::string itype::get_name() const
{
	return this->name;
}

const char* itype::hint_name() const
{
	return hint_names[static_cast<int>(this->h)];
}

bool itype::is_pointer() const
{
	return this->h == hint::pointer_type;
}

bool itype::is_struct() const
{
	return this->h == hint::struct_type;
}

bool itype::is_primitive() const
{
	return this->h == hint::primitive_type;
}

bool itype::is_well_formed() const
{
	return this->h != hint::ill_formed;
}

/*virtual*/ type_ptr itype::deref() const
{
	diag::error(error_code::type, "attempt to dereference {} type {}, but you can only dereference pointer types", this->hint_name(), this->get_name()); return nullptr;
}

/*virtual*/ type_ptr itype::ref() const
{
	return std::make_unique<pointer_type>(this->unique_clone());
}

pointer_type::pointer_type(type_ptr base_type): base(std::move(base_type)){}

pointer_type::pointer_type(const pointer_type& cpy): base(cpy.base->unique_clone())
{

}

/*virtual*/ std::string pointer_type::get_name() const /*final*/
{
	return std::format("{}&", this->base->get_name());
}

/*virtual*/ type_ptr pointer_type::deref() const /*final*/
{
	return this->base->unique_clone();
}

primitive_type::primitive_type(std::string primitive_typename):
itype(primitive_typename, itype::hint::primitive_type)
{
	for(std::size_t i = 0; i < static_cast<int>(primitive::_count); i++)
	{
		if(primitive_typename == primitive_names[i])
		{
			this->prim = static_cast<primitive>(i);
			return;
		}
	}
	// no primitive types matched. set ourselves to an ill-formed type.
	itype::h = itype::hint::ill_formed;
}

namespace type_helpers
{
	bool is_primitive(std::string_view type_name)
	{
		return primitive_type(std::string{type_name}).is_well_formed();
	}
}