#include "type.hpp"
#include "diag.hpp"

constexpr static std::array<const char*, static_cast<int>(itype::hint::_count)> hint_names
{
	"primitive",
	"struct",
	"pointer",
	"alias",
	"ill-formed"
};

itype::itype(): name("<undefined type>"), h(itype::hint::ill_formed){}
itype::itype(std::string name, hint h): name(name), h(h){}

/*virtual*/ std::string itype::get_name() const
{
	return this->name;
}

std::string itype::get_qualified_name() const
{
	return std::format("{}{}{}", this->get_name(), this->is_const() ? " const" : "", this->is_weak() ? " weak" : "");
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

bool itype::is_integer() const
{
	if(!this->is_primitive())
	{
		return false;
	}
	switch(static_cast<const primitive_type*>(this)->prim)
	{
		case primitive::i64:
			[[fallthrough]];
		case primitive::i32:
			[[fallthrough]];
		case primitive::i16:
			[[fallthrough]];
		case primitive::i8:
			[[fallthrough]];
		case primitive::u64:
			[[fallthrough]];
		case primitive::u32:
			[[fallthrough]];
		case primitive::u16:
			[[fallthrough]];
		case primitive::u8:
			[[fallthrough]];
		case primitive::boolean:
			return true;
		break;
		default:
			return false;
		break;
	}
}

bool itype::is_floating_point() const
{
	if(!this->is_primitive())
	{
		return false;
	}
	switch(static_cast<const primitive_type*>(this)->prim)
	{
		case primitive::f64:
			[[fallthrough]];
		case primitive::f32:
			[[fallthrough]];
		case primitive::f16:
			return true;
		break;
		default:
			return false;
		break;
	}
}

bool itype::is_well_formed() const
{
	return this->h != hint::ill_formed;
}

bool itype::is_weak() const
{
	return this->quals & qual_weak;
}

bool itype::is_const() const
{
	return this->quals & qual_const;
}

type_ptr itype::discarded_qualifiers() const
{
	auto ret = this->unique_clone();
	ret->quals = qual_none;
	return ret;
}

/*virtual*/ type_ptr itype::deref() const
{
	diag::error(error_code::type, "attempt to dereference non-pointer type \"{}\", which is a {} type", this->get_name(), this->hint_name());
	return nullptr;
}

/*virtual*/ type_ptr itype::ref() const
{
	return std::make_unique<pointer_type>(this->unique_clone());
}

typeconv itype::can_implicitly_convert_to(const itype& rhs) const
{
	if(*this->discarded_qualifiers() == *rhs.discarded_qualifiers())
	{
		if(this->is_pointer() && this->is_const() && !rhs.is_const())
		{
			return typeconv::cant;
		}
		return typeconv::noop;
	}
	if(this->is_weak() || rhs.is_weak())
	{
		type_ptr plain_this = this->discarded_qualifiers();
		type_ptr plain_rhs = rhs.discarded_qualifiers();
		if(plain_this->is_primitive() && *plain_this == primitive_type{primitive::i64} && rhs.is_pointer())
		{
			return typeconv::i2p;
		}
		if(plain_rhs->is_primitive() && *plain_rhs == primitive_type{primitive::i64} && this->is_pointer())
		{
			return typeconv::p2i;
		}
		if(this->is_integer() && rhs.is_integer())
		{
			return typeconv::i2i;
		}
		if(this->is_floating_point() && rhs.is_floating_point())
		{
			return typeconv::f2f;
		}
		if(this->is_integer() && rhs.is_floating_point())
		{
			return typeconv::i2f;
		}
		if(this->is_floating_point() && rhs.is_integer())
		{
			return typeconv::f2i;
		}
		if(this->is_pointer() && rhs.is_pointer())
		{
			return typeconv::p2p;
		}
	}
	return typeconv::cant;
}

typeconv itype::can_explicitly_convert_to(const itype& rhs) const
{
	// you can explicitly convert (cast) to something if the weak variant of the current type is implicitly convertible.
	auto weak_this = this->unique_clone();
	weak_this->quals = type_qualifier(weak_this->quals | qual_weak);
	return weak_this->can_implicitly_convert_to(rhs);
}

pointer_type::pointer_type(type_ptr base_type): itype(*base_type->discarded_qualifiers()), base(std::move(base_type)){}

pointer_type::pointer_type(const pointer_type& cpy): itype(cpy), base(cpy.base->unique_clone())
{

}

/*virtual*/ std::string pointer_type::get_name() const /*final*/
{
	return std::format("{}&", this->base->get_qualified_name());
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

primitive_type::primitive_type(primitive prim):
itype(primitive_names[static_cast<int>(prim)], itype::hint::primitive_type),
prim(prim)
{}

alias_type::alias_type(type_ptr alias, std::string alias_name): itype(alias_name, itype::hint::alias_type), alias(std::move(alias))
{

}

alias_type::alias_type(const alias_type& cpy): alias_type(cpy.alias->unique_clone(), cpy.name){}

type_ptr alias_type::original() const
{
	return this->alias->unique_clone();
}

struct_type::struct_type(std::string name, std::vector<data_member> members): itype(name, itype::hint::struct_type), members(std::move(members)){}

struct_type::struct_type(const struct_type& cpy): itype(cpy), members()
{
	this->members.reserve(cpy.members.size());
	for(const auto& mem : cpy.members)
	{
		this->members.push_back({.name = mem.name, .ty = mem.ty->unique_clone()});
	}
}

incomplete_type::incomplete_type(std::string name): itype(name, itype::hint::ill_formed){}

incomplete_type::incomplete_type(const incomplete_type& cpy): itype(cpy){}

// type system
type_system::type_system()
{
	this->add_compiler_supported_types();
}

type_system::struct_builder& type_system::struct_builder::add_member(std::string name, std::string type_name)
{
	type_ptr memty = this->sys.get_type(type_name);
	std::string suggestion = this->sys.suggest_valid_typename_for_typo(type_name);
	if(memty == nullptr)
	{
		diag::error(error_code::type, "unknown type \"{}\"\ncontext: as type of data-member {}::{}{}", type_name, this->struct_name, name, suggestion.size() ? std::format("\n\tdid you mean: \"{}\"", suggestion) : "");
	}
	else if(!memty->is_well_formed())
	{
		// ill-formed data member type.
		std::string ill_formed_name = memty->get_name();
		// remove all &
		bool ill_formed_ptr = false;
		ill_formed_name.erase(std::remove_if(ill_formed_name.begin(), ill_formed_name.end(), [&ill_formed_ptr](char c){
			if(c == '&')
			{
				ill_formed_ptr = true;
				return true;
			}
			return false;
			}), ill_formed_name.end());
		// is it our struct type?
		if(ill_formed_name == this->struct_name)
		{
			// edge-case: struct X has a data member that references X
			// that's actually okay, we can patch it up later.
			// note: so long as its a pointer type.
			diag::assert_that(ill_formed_ptr, error_code::type, "type \"{}\"\ncontext: as type of data-member {}::{} is ill-formed as a data member of a struct cannot be that same struct (recursive struct). did you mean to make it a {}&", type_name, this->struct_name, name, this->struct_name);
		}
		else
		{
			diag::error(error_code::type, "type \"{}\"\ncontext: as type of data-member {}::{} is ill-formed.", type_name, this->struct_name, name);
			// pointer to an ill-formed or incomplete type.
		}
	}
	this->members.push_back({.name = name, .ty = std::move(memty)});
	return *this;
}

type_ptr type_system::struct_builder::build()
{
	this->sys.types[this->struct_name] = std::make_unique<struct_type>(this->struct_name, std::move(this->members));
	// as promised, if any data members are ill-formed, then we would've errored out earlier UNLESS we determined they were a pointer or something to our struct type itself. so let's try again and assert that it works now its a complete type.
	for(auto& mem : static_cast<struct_type*>(this->sys.types[this->struct_name].get())->members)
	{
		if(!mem.ty->is_well_formed())
		{
			std::string mem_name = mem.ty->get_name();
			mem.ty = this->sys.get_type(mem.ty->get_name());
			diag::assert_that(mem.ty != nullptr && mem.ty->is_well_formed(), error_code::type, "data member {}::{} was of incomplete type {}, but still failed to resolve the type after the struct has completed.", this->struct_name, mem.name, mem_name);
		}
	}
	return this->sys.types[this->struct_name]->unique_clone();
}

type_system::struct_builder type_system::make_struct(std::string name)
{
	// firstly register us as an incomplete type. when you build() it will become a complete type.
	// this is necessary incase the struct has a data member that points to the same type.
	this->types[name] = incomplete_type(name).unique_clone();
	return {.sys = *this, .struct_name = name};
}

type_ptr type_system::make_alias(std::string name, std::string typename_to_alias)
{
	type_ptr aliased = this->get_type(typename_to_alias);
	this->types[name] = alias_type{aliased->unique_clone(), name}.unique_clone();
	return aliased->unique_clone();
}

type_ptr type_system::get_type(std::string type_name) const
{
	type_ptr ret = nullptr;
	for(auto iter = type_name.begin(); iter != type_name.end();iter++)
	{
		std::string_view previous{type_name.begin(), iter};
		std::string next{&*iter};
		const bool found_const = next.starts_with(" const");
		const bool found_weak = next.starts_with(" weak");
		const bool found_ptr = next.starts_with("&");
		if(found_const || found_weak || found_ptr)
		{
			ret = this->get_type(std::string{previous});
			if(found_const)
			{
				ret->quals = static_cast<type_qualifier>(ret->quals | qual_const);
			}
			if(found_weak)
			{
				ret->quals = static_cast<type_qualifier>(ret->quals | qual_weak);
			}
			if(found_ptr)
			{
				ret = ret->ref();
			}
		}
	}
	if(ret != nullptr)
	{
		return ret;
	}
	primitive_type prim{type_name};
	if(prim.is_well_formed())
	{
		return prim.unique_clone();
	}

	auto iter = this->types.find(type_name);
	if(iter != this->types.end())
	{
		return iter->second->unique_clone();
	}
	return nullptr;
}

type_ptr type_system::get_primitive_type(primitive prim) const
{
	return primitive_type{primitive_names[static_cast<int>(prim)]}.unique_clone();
}

static int levenshtein_distance(std::string_view s1, std::string_view s2)
{
	const std::size_t m(s1.size());
	const std::size_t n(s2.size());

	if (m == 0) return n;
	if (n == 0) return m;

	std::vector<std::vector<int>> d(m + 1, std::vector<int>(n + 1));

	for (std::size_t i = 0; i <= m; ++i) d[i][0] = i;
	for (std::size_t j = 0; j <= n; ++j) d[0][j] = j;

	for (std::size_t i = 1; i <= m; ++i)
	{
		for (std::size_t j = 1; j <= n; ++j)
		{
			int cost = (s1[i - 1] == s2[j - 1]) ? 0 : 1;
			d[i][j] = std::min({ d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost });
		}
	}
	return d[m][n];
}

std::string type_system::suggest_valid_typename_for_typo(std::string invalid_typename) const
{
	std::vector<std::string_view> all_types = {};
	for(std::size_t i = 0; i < static_cast<int>(primitive::_count); i++)
	{
		all_types.push_back(primitive_names[i]);
	}

	for(const auto& [type_name, type_ptr] : this->types)
	{
		all_types.push_back(type_name);
	}

	std::string suggestion = "";
	int dist = std::numeric_limits<int>::max();
	for(std::string_view alternative : all_types)
	{
		int cur_dist = levenshtein_distance(alternative, invalid_typename);
		if(cur_dist < dist)
		{
			suggestion = alternative;
			dist = cur_dist;
		}
	}

	constexpr std::size_t wrong_char_threshold = 4;
	if(std::cmp_less_equal(dist, std::min(wrong_char_threshold, std::min(suggestion.size(), invalid_typename.size()))))
	{
		return suggestion;
	}
	return "";
}

void type_system::add_compiler_supported_types()
{
	this->make_struct("typeinfo")
		.add_member("name", "i8&")
		.add_member("size", "u64")
		.add_member("align", "u64")
		.add_member("member_count", "u64")
		.add_member("member_names", "i8&&")
		.add_member("member_types", "typeinfo&")
		.build();
}

namespace type_helpers
{
	bool is_primitive(std::string_view type_name)
	{
		return primitive_type(std::string{type_name}).is_well_formed();
	}
}