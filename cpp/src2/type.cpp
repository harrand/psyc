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

// type system

type_system::struct_builder& type_system::struct_builder::add_member(std::string name, std::string type_name)
{
	type_ptr memty = this->sys.get_type(type_name);
	if(memty == nullptr || (memty != nullptr && !memty->is_well_formed()))
	{
		std::string suggestion = this->sys.suggest_valid_typename_for_typo(type_name);
		diag::error(error_code::type, "unknown type \"{}\"\ncontext: as type of {}'s data-member \"{}\"{}", type_name, this->struct_name, name, suggestion.size() ? std::format("\n\tdid you mean: \"{}\"", suggestion) : "");
	}
	this->members.push_back({.name = name, .ty = std::move(memty)});
	return *this;
}

void type_system::struct_builder::build()
{
	this->sys.types[this->struct_name] = std::make_unique<struct_type>(this->struct_name, std::move(this->members));
}

type_system::struct_builder type_system::make_struct(std::string name)
{
	return {.sys = *this, .struct_name = name};
}

void type_system::make_alias(std::string name, std::string typename_to_alias)
{
	type_ptr aliased = this->get_type(typename_to_alias);
	this->types[name] = alias_type{std::move(aliased), name}.unique_clone();
}

type_ptr type_system::get_type(std::string type_name) const
{
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

	constexpr int wrong_char_threshold = 4;
	if(dist <= wrong_char_threshold)
	{
		return suggestion;
	}
	return "";
}

namespace type_helpers
{
	bool is_primitive(std::string_view type_name)
	{
		return primitive_type(std::string{type_name}).is_well_formed();
	}
}