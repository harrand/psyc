#include "static.hpp"
#include "diag.hpp"

static_value static_value::null()
{
	return {};
}

static_value static_value::type_only(type_ptr ty)
{
	return {.ty = std::move(ty)};
}

std::int64_t get_int_value(const itype& ty, const std::any& int_of_some_size)
{
	auto bit_count = ty.numeric_bit_count();
	if(ty.is_unsigned())
	{
		bit_count = -bit_count;
	}
	switch(bit_count)
	{
		case 64:
			return std::any_cast<std::int64_t>(int_of_some_size);
		break;
		case -64:
			return std::any_cast<std::uint64_t>(int_of_some_size);
		break;
		case 32:
			return std::any_cast<std::int32_t>(int_of_some_size);
		break;
		case -32:
			return std::any_cast<std::uint32_t>(int_of_some_size);
		break;
		case 16:
			return std::any_cast<std::int16_t>(int_of_some_size);
		break;
		case -16:
			return std::any_cast<std::uint16_t>(int_of_some_size);
		break;
		case 8:
			return std::any_cast<std::int8_t>(int_of_some_size);
		break;
		case -8:
			return std::any_cast<std::uint8_t>(int_of_some_size);
		break;
		default:
			diag::error(error_code::ice, "panic: any -> int of some size ({})", ty.get_qualified_name());
			return {};
		break;
	}
}

std::any to_int_value(const itype& ty, std::int64_t ival)
{
	auto bit_count = ty.numeric_bit_count();
	if(ty.is_unsigned())
	{
		bit_count = -bit_count;
	}
	switch(bit_count)
	{
		case 64:
			return static_cast<std::int64_t>(ival);
		break;
		case -64:
			return static_cast<std::uint64_t>(ival);
		break;
		case 32:
			return static_cast<std::int32_t>(ival);
		break;
		case -32:
			return static_cast<std::uint32_t>(ival);
		break;
		case 16:
			return static_cast<std::int16_t>(ival);
		break;
		case -16:
			return static_cast<std::uint16_t>(ival);
		break;
		case 8:
			return static_cast<std::int8_t>(ival);
		break;
		case -8:
			return static_cast<std::uint8_t>(ival);
		break;
		default:
			diag::error(error_code::ice, "panic: any -> int of some size ({})", ty.get_qualified_name());
			return {};
		break;
	}
}

static_value static_value::do_convert(type_ptr to, srcloc ctx)
{
	auto conv = this->ty->can_implicitly_convert_to(*to);
	switch(conv)
	{
		case typeconv::cant:
			diag::error(error_code::coneval, "at {}: illegal conversion from {} to {}", ctx.to_string(), this->ty->get_qualified_name(), to->get_qualified_name());
			return {};
		break;
		case typeconv::noop:
		[[fallthrough]];
		case typeconv::p2p: // all pointers are void* in terms of static values.
		[[fallthrough]];
		case typeconv::f2f:  // all floating point types are double in terms of static values (mainly coz half is not a thing in c -.-)
			return
			{
				.ty = to->unique_clone(),
				.val = this->val
			};
		break;
		case typeconv::i2p:
		{
			std::int64_t val = get_int_value(*this->ty, this->val);
			return
			{
				.ty = to->unique_clone(),
				.val = reinterpret_cast<void*>(static_cast<std::uintptr_t>(val))
			};
		}
		break;
		case typeconv::p2i:
			return
			{
				.ty = to->unique_clone(),
				.val = to_int_value(*this->ty, static_cast<std::int64_t>(reinterpret_cast<std::uintptr_t>(std::any_cast<void*>(this->val))))
			};
		break;
		case typeconv::i2i:
			return
			{
				.ty = to->unique_clone(),
				.val = to_int_value(*to, get_int_value(*this->ty, this->val))
			};
		break;
		default:
			diag::error(error_code::nyi, "at {}: no implementation for type conversion (id {}) from {} to {}", ctx.to_string(), static_cast<int>(conv), this->ty->get_qualified_name(), to->get_qualified_name());
			return {};
		break;
	}
}

static_value static_value::do_explicit_convert(type_ptr to, srcloc ctx)
{
	auto cpy = this->clone();
	cpy.ty->quals = static_cast<type_qualifier>(cpy.ty->quals | qual_weak);
	return cpy.do_convert(std::move(to), ctx);
}

bool static_value::has_value() const
{
	return this->val.has_value();
}

static_value static_value::clone() const
{
	static_value cpy;
	cpy.ty = this->ty->unique_clone();
	cpy.val = this->val;
	for(const auto& [name, child_val] : this->children)
	{
		cpy.children[name] = child_val.clone();
	}
	return cpy;
}